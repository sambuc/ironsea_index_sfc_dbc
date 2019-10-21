use std::fmt::Debug;
use std::hash::Hash;
use std::io;
use std::iter::FromIterator;
use std::marker;
use std::ops::Index;

use serde::de::DeserializeOwned;
use serde::Serialize;

pub use ironsea_index::IndexedOwned;
pub use ironsea_index::Record;
pub use ironsea_index::RecordBuild;
pub use ironsea_index::RecordFields;
use ironsea_store::Load;
use ironsea_store::Store;
use ironsea_table::Table;

use super::cell_space::CellSpace;
use super::morton::MortonCode;
use super::morton::MortonEncoder;
use super::morton::MortonValue;

type SFCCode = u32;
type SFCOffset = u32;

//FIXME: Remove the need for a constant, how can we make it type-checked instead?
//       type-num crate?
const MAX_K: usize = 3;

#[derive(Debug)]
struct Limit<V> {
    idx: usize,
    position: Vec<V>,
}

#[derive(Debug)]
struct Limits<'a, V> {
    start: Limit<&'a V>,
    end: Limit<&'a V>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
struct SFCRecord<F> {
    //FIXME: Find a way around hardcoding MAX_K
    offsets: [SFCOffset; MAX_K],
    fields: F,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
struct SFCCell<F> {
    code: MortonCode,
    records: Vec<SFCRecord<F>>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct SpaceFillingCurve<T, R, K, V, F>
where
    T: Table<R>,
    R: Record<K> + RecordFields<F> + RecordBuild<K, F, R> + Debug,
    //    K: Debug + ExactSizeIterator + Index<usize, Output = V> + FromIterator<V>,
    V: Clone + Ord + Debug + From<usize>,
{
    dimensions: usize,
    morton: MortonEncoder,
    space: CellSpace<K, V>,
    index: Vec<SFCCell<F>>,
    _marker: marker::PhantomData<(T, R)>,
}

impl<T, R, K, V, F> SpaceFillingCurve<T, R, K, V, F>
where
    T: Table<R>,
    R: Record<K> + RecordFields<F> + RecordBuild<K, F, R> + Debug,
    V: Clone + Debug + From<usize> + Hash + Ord,
    K: Debug + Index<usize, Output = V> + FromIterator<V>,
{
    //FIXME: Should accept indexing 0 elements, at least not crash!
    pub fn new(table: &T, dimensions: usize, cell_bits: usize) -> Self {
        // 1. build the dictionnary space, called here CellSpace, as well as
        // initialize the morton encoder used to project the multi-dimensional
        // coordinates into a single dimension.
        let mut index = SpaceFillingCurve {
            dimensions,
            morton: MortonEncoder::new(dimensions, cell_bits),
            space: CellSpace::new(table, dimensions, cell_bits),
            index: vec![],
            _marker: marker::PhantomData,
        };

        // 2. Build a flat table of (code, offset, entries)
        let mut flat_table = vec![];

        for record in table.get_table() {
            let position = record.key();
            match index.space.key(&position) {
                Ok((cell_ids, offsets)) => match index.encode(&cell_ids) {
                    Ok(code) => {
                        let offsets = offsets.iter().map(|i| *i as SFCOffset).collect::<Vec<_>>();
                        flat_table.push((
                            code,
                            SFCRecord {
                                offsets: *array_ref!(offsets, 0, MAX_K),
                                fields: record.fields(),
                            },
                        ))
                    }
                    Err(e) => error!("Unable to encode position {:#?}: {}", cell_ids, e),
                },
                Err(e) => error!("Invalid position {:#?}: {}", position, e),
            }
        }

        debug!(
            "Processed {:#?} records into the index",
            table.get_table().len()
        );

        // 5. Sort by SFCcode
        flat_table.sort_unstable_by(|a, b| a.0.cmp(&b.0));
        let nb_records = flat_table.len();

        let mut current_cell_code = flat_table[0].0;
        let mut count = 0;
        index.index.push(SFCCell {
            code: current_cell_code,
            records: vec![],
        });
        for (code, record) in flat_table {
            if code == current_cell_code {
                index.index[count].records.push(record);
            } else {
                index.index.push(SFCCell {
                    code,
                    records: vec![record],
                });
                current_cell_code = code;
                count += 1;
            }
        }
        debug!("Inserted {:#?} records into the index", nb_records);

        index
    }

    pub fn find_by_value(&self, value: &F) -> Vec<R>
    where
        F: std::cmp::PartialEq,
    {
        let mut results = vec![];
        for cell in &self.index {
            for record in &cell.records {
                if &record.fields == value {
                    if let Ok(r) = self.get_record(cell.code, &record) {
                        results.push(r);
                    }
                }
            }
        }

        results
    }

    // Map the cell_ids of a point to its SFCcode
    #[inline]
    fn encode(&self, cell_ids: &[usize]) -> Result<SFCCode, String> {
        let mut t = vec![];
        for v in cell_ids.iter() {
            t.push(*v as MortonValue);
        }

        self.morton.encode(&t)
    }

    fn last(&self) -> (Vec<usize>, Vec<usize>) {
        self.space.last()
    }

    fn value(&self, code: SFCCode, offsets: &[SFCOffset]) -> Result<Vec<&V>, String> {
        Ok(self.space.value(
            self.morton
                .decode(code)
                .iter()
                .map(|e| *e as usize)
                .collect(),
            offsets.iter().map(|e| *e as usize).collect(),
        )?)
    }

    // Build coordinate values from encoded value
    fn position(&self, code: SFCCode, offsets: &[SFCOffset]) -> Result<K, String> {
        let position = self.value(code, offsets)?;

        Ok(position.iter().map(|i| (*i).clone()).collect())
    }

    // Rebuild a specific record
    fn get_record(&self, code: SFCCode, entry: &SFCRecord<F>) -> Result<R, String> {
        let position = &self.position(code, &entry.offsets)?;

        Ok(R::build(position, &entry.fields))
    }

    fn limits(&self, start: &K, end: &K) -> Result<Limits<V>, String> {
        trace!("limits: {:?} - {:?}", start, end);

        // Round down if not found, for start of range:
        let (cells, offsets) = self.space.key_down(start)?;
        let code = self.encode(&cells)?;
        let idx = match self.index.binary_search_by(|e| e.code.cmp(&code)) {
            Err(e) => {
                if e > 0 {
                    e - 1
                } else {
                    0
                }
            }
            Ok(c) => c,
        };
        let position = self.space.value(cells, offsets)?;
        let start = Limit { idx, position };

        // Round up if not found, for end of range:
        let (cells, offsets) = self.space.key_up(end)?;
        let code = self.encode(&cells)?;
        let idx = match self.index.binary_search_by(|e| e.code.cmp(&code)) {
            Err(e) => {
                if e >= self.index.len() {
                    self.index.len()
                } else {
                    e
                }
            }
            Ok(c) => c + 1,
        };

        let position = self.space.value(cells, offsets)?;
        let end = Limit { idx, position };

        trace!("limits: {:?} - {:?}", start, end);

        Ok(Limits { start, end })
    }
}

impl<T, R, K, V, F> IndexedOwned<T, R, K> for SpaceFillingCurve<T, R, K, V, F>
where
    T: Table<R>,
    R: Record<K> + RecordFields<F> + RecordBuild<K, F, R> + Debug,
    K: Debug + Index<usize, Output = V> + FromIterator<V>,
    V: Clone + Debug + From<usize> + Hash + Ord,
{
    fn find(&self, key: &K) -> Vec<R> {
        let mut values = vec![];

        if let Ok((cell_ids, offsets)) = self.space.key(key) {
            match self.encode(&cell_ids) {
                Err(e) => error!("{}", e),
                Ok(code) => {
                    if let Ok(cell) = self.index.binary_search_by(|a| a.code.cmp(&code)) {
                        for record in &self.index[cell].records {
                            let mut select = true;
                            for (k, o) in offsets.iter().enumerate().take(self.dimensions) {
                                select &= record.offsets[k] == (*o as SFCOffset);
                            }

                            if select {
                                match self.get_record(code, record) {
                                    Err(e) => error!("{}", e),
                                    Ok(r) => values.push(r),
                                }
                            }
                        }
                    }
                }
            }
        }

        values
    }

    fn find_range(&self, start: &K, end: &K) -> Vec<R> {
        let mut values = vec![];

        match self.limits(start, end) {
            Ok(limits) => {
                for idx in limits.start.idx..limits.end.idx {
                    let code = self.index[idx].code;
                    for record in &self.index[idx].records {
                        let mut select = true;
                        let pos = match self.position(code, &record.offsets) {
                            Err(e) => {
                                error!("{}", e);
                                continue;
                            }
                            Ok(p) => p,
                        };

                        // FIXME: Reduce number of comparison by using the cells boundaries.
                        for k in 0..self.dimensions {
                            select = select
                                && *limits.start.position[k] <= pos[k]
                                && *limits.end.position[k] >= pos[k];
                        }
                        if select {
                            match self.get_record(code, &record) {
                                Err(e) => error!("{}", e),
                                Ok(r) => values.push(r),
                            }
                        }
                    }
                }
            }
            Err(e) => error!("find_range: limits failed: {}", e),
        };

        values
    }
}
// Rough check, based on per-dimension cell Ids.
/*
            // If the cell_ids are between ]pos_start and pos_end[, then the value is within the range,
            // If the cell_ids are outside [pos_start, pos_end], then the value is out, stop checking
            // Else, check the offsets of each entry to be within [off_start, off_end], then the value is within the range.
            let mut rough_in = true;
            for k in 0..self.dimensions {
                if !(cells[k] > start_limits.cells[k] && cells[k] < end_limits.cells[k]) {
                    rough_in = false;
                }
            }

            if rough_in {
                // This is a cell well within the volume, so all points are a match, add all points,
                // go to next cell.
                for entry in entries {
                    values.push(self.get_element(code, entry))
                }

                continue;
            }

            let mut rough_out = false;
            for k in 0..self.dimensions {
                if cells[k] < start_limits.cells[k] || cells[k] > end_limits.cells[k] {
                    rough_out = false;
                }
            }

            // If rough is not true, then we have nothing to double check.
            if rough_out {
                continue;
            }
*/

impl<T, R, K, V, F> Store for SpaceFillingCurve<T, R, K, V, F>
where
    T: Table<R>,
    R: Record<K> + RecordFields<F> + RecordBuild<K, F, R> + Debug,
    //    K: Debug + ExactSizeIterator + Index<usize, Output = V> + FromIterator<V>,
    K: Serialize,
    V: Clone + Ord + Debug + From<usize> + Serialize,
    F: Serialize,
{
    fn store<W>(&mut self, writer: W) -> io::Result<()>
    where
        W: std::io::Write,
    {
        match bincode::serialize_into(writer, &self) {
            Ok(_) => Ok(()),
            Err(e) => Err(io::Error::new(io::ErrorKind::WriteZero, e)),
        }
    }
}

impl<T, R, K, V, F> Load for SpaceFillingCurve<T, R, K, V, F>
where
    T: Table<R>,
    R: Record<K> + RecordFields<F> + RecordBuild<K, F, R> + Debug,
    K: DeserializeOwned,
    V: Clone + Ord + Debug + From<usize> + DeserializeOwned,
    F: DeserializeOwned,
{
    fn load<Re: io::Read>(reader: Re) -> io::Result<Self> {
        match bincode::deserialize_from(reader) {
            Ok(data) => Ok(data),
            Err(e) => Err(io::Error::new(io::ErrorKind::InvalidData, e)),
        }
    }

    // only required for store_mapped_file
    fn load_slice(from: &[u8]) -> io::Result<Self> {
        match bincode::deserialize(from) {
            Ok(data) => Ok(data),
            Err(e) => Err(io::Error::new(io::ErrorKind::InvalidData, e)),
        }
    }
}
