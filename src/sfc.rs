#![allow(clippy::type_repetition_in_bounds)]

use std::cmp::PartialEq;
use std::fmt::Debug;
use std::hash::Hash;
//use std::io;
use std::iter::FromIterator;
use std::ops::Index;

pub use ironsea_index::IndexedDestructured;
pub use ironsea_index::Record;
pub use ironsea_index::RecordFields;
//use ironsea_store::Load;
//use ironsea_store::Store;
//use serde::de::DeserializeOwned;
use serde::Deserialize;
use serde::Serialize;

use super::cell_space::CellSpace;
use super::morton::MortonCode;
use super::morton::MortonEncoder;
use super::morton::MortonValue;

type SFCCode = MortonCode;
type SFCOffset = u32;

//FIXME: Remove the need for a constant, how can we make it type-checked instead?
//       type-num crate?
const MAX_K: usize = 3;

#[derive(Clone, Debug)]
struct Limit<V> {
    idx: usize,
    position: Vec<V>,
}

#[derive(Clone, Debug)]
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

/// Space Filling Curve-based index.
///
/// This structure retains the state of the index.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct SpaceFillingCurve<F, K, V>
where
    F: PartialEq,
    K: Debug + FromIterator<V> + Index<usize, Output = V>,
    V: Clone + Debug + From<usize> + Ord,
{
    dimensions: usize,
    morton: MortonEncoder,
    space: CellSpace<K, V>,
    index: Vec<SFCCell<F>>,
}

impl<F, K, V> SpaceFillingCurve<F, K, V>
where
    F: PartialEq,
    K: Debug + FromIterator<V> + Index<usize, Output = V>,
    V: Clone + Debug + From<usize> + Hash + Ord,
{
    /// Creates a new Index from the provided iterator.
    ///
    /// * `dimensions`: The number of dimensions of the space, a.k.a the
    ///                 length of the vector representing a single
    ///                 position.
    /// * `cell_bits`: The number of bits to reserve for the grid we
    ///                build on top of the coordinate dictionaries.
    ///                We generate 2^`cell_bits` Cells per dimension.
    ///
    //FIXME: Should accept indexing 0 elements, at least not crash!
    pub fn new<I, R>(iter: I, dimensions: usize, cell_bits: usize) -> Self
    where
        I: Clone + Iterator<Item = R>,
        R: Debug + Record<K> + RecordFields<F>,
    {
        // 1. build the dictionnary space, called here CellSpace, as well as
        // initialize the morton encoder used to project the multi-dimensional
        // coordinates into a single dimension.
        let mut index = SpaceFillingCurve {
            dimensions,
            morton: MortonEncoder::new(dimensions, cell_bits),
            space: CellSpace::new(iter.clone(), dimensions, cell_bits),
            index: vec![],
        };

        // 2. Build a flat table of (code, offset, entries)
        let mut flat_table = vec![];
        let (nb_records, _) = iter.size_hint();
        for record in iter.into_iter() {
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

        debug!("Processed {:#?} records into the index", nb_records);

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

    /// Returns a vector of keys which have stored values in the index
    /// equal to `value`.
    pub fn find_by_value<'s>(&'s self, value: &'s F) -> Box<dyn Iterator<Item = K> + 's> {
        Box::new(
            self.index
                .iter()
                .map(|cell| (cell, cell.records.iter()))
                .flat_map(move |(cell, records)| {
                    records.filter_map(move |record| {
                        if &record.fields == value {
                            if let Ok(key) = self.position(cell.code, &record.offsets) {
                                Some(key)
                            } else {
                                None
                            }
                        } else {
                            None
                        }
                    })
                }),
        )
    }

    // Map the cell_ids of a point to its SFCcode
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
        self.space.value(
            self.morton
                .decode(code)
                .iter()
                .map(|e| *e as usize)
                .collect(),
            offsets.iter().map(|e| *e as usize).collect(),
        )
    }

    // Build coordinate values from encoded value
    fn position(&self, code: SFCCode, offsets: &[SFCOffset]) -> Result<K, String> {
        let position = self.value(code, offsets)?;

        Ok(position.iter().map(|i| (*i).clone()).collect())
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

impl<F, K, V> IndexedDestructured<F, K> for SpaceFillingCurve<F, K, V>
where
    F: PartialEq,
    K: Debug + FromIterator<V> + Index<usize, Output = V>,
    V: Clone + Debug + From<usize> + Hash + Ord,
{
    fn find<'i>(&'i self, key: &K) -> Box<dyn Iterator<Item = &F> + 'i> {
        if let Ok((cell_ids, offsets)) = self.space.key(key) {
            match self.encode(&cell_ids) {
                Err(e) => error!("{}", e),
                Ok(code) => {
                    if let Ok(cell) = self.index.binary_search_by(|a| a.code.cmp(&code)) {
                        return Box::new(self.index[cell].records.iter().filter_map(
                            move |record| {
                                let mut select = true;
                                for (k, o) in offsets.iter().enumerate().take(self.dimensions) {
                                    select &= record.offsets[k] == (*o as SFCOffset);
                                }

                                if select {
                                    Some(&record.fields)
                                } else {
                                    None
                                }
                            },
                        ));
                    }
                }
            }
        }

        Box::new(Vec::with_capacity(0).into_iter())
    }

    fn find_range<'i>(&'i self, start: &K, end: &K) -> Box<dyn Iterator<Item = (K, &F)> + 'i> {
        match self.limits(start, end) {
            Ok(limits) => {
                let iter = (limits.start.idx..limits.end.idx)
                    .filter_map(move |idx| {
                        match self.value(self.index[idx].code, &self.index[idx].records[0].offsets)
                        {
                            Err(_) => None,
                            Ok(first) => Some((idx, first)),
                        }
                    })
                    .filter_map(move |(idx, first)| {
                        let (cell_ids, last_offsets) = self.last();
                        match self.space.value(cell_ids, last_offsets) {
                            Err(_) => None,
                            Ok(last) => Some((idx, first, last)),
                        }
                    })
                    .flat_map(move |(idx, first, last)| {
                        // Check first & last point of the cell, if both are fully
                        // in the bounding box, then all the points of the cell will
                        // be.
                        let limits = limits.clone();
                        let b: Box<dyn Iterator<Item = _>> = if limits.start.position <= first
                            && first <= limits.end.position
                            && limits.start.position <= last
                            && last <= limits.end.position
                        {
                            Box::new(self.index[idx].records.iter().filter_map(move |record| {
                                let code = self.index[idx].code;
                                if let Ok(key) = self.position(code, &record.offsets) {
                                    Some((key, &record.fields))
                                } else {
                                    None
                                }
                            }))
                        } else {
                            // We have points which are outside of the bounding box,
                            // so check every points one by one.
                            Box::new(self.index[idx].records.iter().filter_map(move |record| {
                                let code = self.index[idx].code;
                                if let Ok(pos) = self.value(code, &record.offsets) {
                                    if limits.start.position <= pos && pos <= limits.end.position {
                                        if let Ok(key) = self.position(code, &record.offsets) {
                                            Some((key, &record.fields))
                                        } else {
                                            None
                                        }
                                    } else {
                                        None
                                    }
                                } else {
                                    None
                                }
                            }))
                        };

                        b
                    });
                Box::new(iter)
            }
            Err(e) => {
                error!("find_range: limits failed: {}", e);
                Box::new(Vec::with_capacity(0).into_iter())
            }
        }
    }
}

/*
impl<F, K, V> Store for SpaceFillingCurve<F, K, V>
where
    F: PartialEq + Serialize,
    K: Debug + Serialize + FromIterator<V> + Index<usize, Output = V>,
    V: Clone + Debug + From<usize> + Ord + Serialize,
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

impl<F, K, V> Load for SpaceFillingCurve<F, K, V>
where
    F: PartialEq + DeserializeOwned,
    K: Debug + DeserializeOwned + FromIterator<V> + Index<usize, Output = V>,
    V: Clone + Debug + DeserializeOwned + From<usize> + Ord,
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
*/
