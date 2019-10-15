use std::collections::HashSet;
use std::fmt::Debug;
use std::hash::Hash;
use std::marker;
use std::ops::Index;

use ironsea_index::Record;
use ironsea_table::Table;

type Cell<T> = Vec<T>;

#[derive(Clone, Debug, Deserialize, Serialize)]
struct CellDictionary<K, V> {
    table: Vec<Cell<V>>,
    max_offset: usize,
    _marker: marker::PhantomData<(K)>,
}

impl<K, V> CellDictionary<K, V>
where
    V: Clone + Ord + Debug + Hash,
    K: Debug + Index<usize, Output = V>,
{
    pub fn new<T, R>(table: &T, dimension: usize, cell_bits: usize) -> Self
    where
        T: Table<R>,
        R: Record<K> + Debug,
    {
        // 1. Retrieve a list of distinct values for the coordinate `dimension`
        let mut distinct: HashSet<V> = table
            .get_table()
            .iter()
            .map(|&record| record.key()[dimension].clone())
            .collect();

        // 2. Build a sorted list, of distinct elements
        let mut distinct = distinct.drain().collect::<Vec<_>>();
        distinct.sort_unstable();

        info!(
            "Number of distinct coordinates on dim[{}]: {}",
            dimension,
            distinct.len()
        );

        trace!("min {:?}, max {:?}", distinct[0], distinct.last());

        // 3. Build the dictionary space
        // 3.1. Build dictionnary per dimension, Add cell and offset
        //      informations
        let mut count = 0;
        let mut cell = 0;

        // Beware integer division is rounded towards zero, so add 1 to the
        // result as this is the max number of elements per bucket.
        let max_offset = (distinct.len() / (1 << cell_bits)) + 1;

        let mut cells: Vec<Cell<V>> = Vec::with_capacity(1 << cell_bits);

        // Do not forget to initialise cells[0]!
        cells.push(Vec::with_capacity(max_offset));

        for coordinate in distinct {
            //trace!("{:?} {:?} {:?} {:?}", dimension, coordinate, cell, count);

            // Check first, otherwise we might add a cell which will stay empty.
            if count == max_offset {
                count = 0;
                cell += 1;
                cells.push(Vec::with_capacity(max_offset));
            }

            cells[cell].push(coordinate);
            count += 1;
        }

        info!(
            "dim[{}]: {} cells, {} max per cell",
            dimension,
            cells.len(),
            max_offset,
        );

        CellDictionary {
            table: cells,
            max_offset,
            _marker: marker::PhantomData,
        }
    }

    fn max_offset(&self) -> usize {
        self.max_offset
    }

    fn cells(&self) -> &Vec<Cell<V>> {
        &self.table
    }

    fn cell_id(&self, position: &V) -> Option<usize>
    where
        V: Clone + Ord + Debug,
    {
        let mut id = 0;
        // If the last value of the current cell is >= than the value, then
        // the value is stored in the cell.
        // If this is the first cell, we will look into it as `id` is
        // still 0.
        for cell in self.cells() {
            // last cell is likely to be only partially full
            match cell.last() {
                Some(x) => {
                    if x >= position {
                        break;
                    }
                }
                None => break,
            };
            id += 1;
        }

        if id >= self.cells().len() {
            None
        } else {
            Some(id)
        }
    }

    fn key(&self, position: &V) -> Option<(usize, usize)> {
        let mut result = None;
        if let Some(id) = self.cell_id(position) {
            if let Ok(offset) = self.table[id].binary_search(position) {
                result = Some((id, offset));
            }
        }

        result
    }

    fn key_down(&self, position: &V) -> (usize, usize) {
        match self.cell_id(position) {
            Some(id) => match self.table[id].binary_search(position) {
                Ok(offset) => (id, offset),
                Err(offset) => {
                    if offset > 0 {
                        (id, offset - 1)
                    } else if id == 0 {
                        (0, 0)
                    } else {
                        let id = id - 1;
                        (id, self.table[id].len() - 1)
                    }
                }
            },
            None => self.last(),
        }
    }

    fn last(&self) -> (usize, usize) {
        let last_id = self.table.len() - 1;
        let last_offset = self.table[last_id].len() - 1;

        (last_id, last_offset)
    }

    fn key_up(&self, position: &V) -> (usize, usize) {
        match self.cell_id(position) {
            Some(id) => match self.table[id].binary_search(position) {
                Ok(offset) => (id, offset),
                Err(offset) => {
                    if offset < self.max_offset {
                        (id, offset)
                    } else if id < self.table.len() {
                        (id + 1, 0)
                    } else {
                        self.last()
                    }
                }
            },
            None => self.last(),
        }
    }

    fn value(&self, cell_id: usize, offset: usize) -> &V {
        &self.table[cell_id][offset]
    }
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct CellSpace<K, V> {
    dimensions: usize,
    coordinates: Vec<CellDictionary<K, V>>,
    coordinates_max_offsets: Vec<usize>,
}

impl<K, V> CellSpace<K, V>
where
    V: Clone + Ord + Debug + Hash,
    K: Debug + Index<usize, Output = V>,
{
    pub fn new<T, R>(table: &T, dimensions: usize, cell_bits: usize) -> Self
    where
        T: Table<R>,
        R: Record<K> + Debug,
        V: Clone + Ord + Debug,
    {
        let mut space = CellSpace {
            dimensions,
            coordinates: vec![],
            coordinates_max_offsets: vec![],
        };

        // FIXME: Add check to ensure all positions have the required number of dimensions.
        for k in 0..dimensions {
            let dic = CellDictionary::new(table, k, cell_bits);
            let max = dic.max_offset();
            space.coordinates.push(dic);
            space.coordinates_max_offsets.push(max);
        }

        space
    }

    /*
        pub fn cells_id(&self, position: &Vec<V>) -> Result<Vec<Option<usize>>, String> {
            trace!("cells_id: position {:?}", position);
            //TODO: Should we check inside each objects, or just assume it is correct and/or rely on the bound checks?
            if self.dimensions != position.len() {
                return Err(format!(
                    "Incorrect number of dimensions, expected {}, got {} for {:?}",
                    self.dimensions,
                    position.len(),
                    position
                ));
            }

            let mut cells = vec![];
            for k in 0..self.dimensions {
                cells.push(self.coordinates[k].cell_id(&position[k]));
            }
            trace!("cells_id: cells {:?}", cells);
            Ok(cells)
        }
    */
    pub fn key(&self, position: &K) -> Result<(Vec<usize>, Vec<usize>), String> {
        //TODO: Should we check inside each objects, or just assume it is correct and/or rely on the bound checks?
        /* This impose to require ExactSizeIterator, which is not implemented on Vec, and can't be in any easy way.
        if self.dimensions != position.len() {
            return Err(format!(
                "Incorrect number of dimensions, expected {}, got {} for {:?}",
                self.dimensions,
                position.len(),
                position
            ));
        }*/

        let mut cells = vec![];
        let mut offsets = vec![];
        for k in 0..self.dimensions {
            match self.coordinates[k].key(&position[k]) {
                None => {
                    return Err(format!(
                        "Incorrect value for position[{:?}]: {:?}",
                        k, &position[k]
                    ))
                }
                Some((id, offset)) => {
                    cells.push(id);
                    offsets.push(offset)
                }
            };
        }

        Ok((cells, offsets))
    }

    // Round down to the preceding element or self if in the space
    pub fn key_down(&self, position: &K) -> Result<(Vec<usize>, Vec<usize>), String> {
        //TODO: Should we check inside each objects, or just assume it is correct and/or rely on the bound checks?
        /* This impose to require ExactSizeIterator, which is not implemented on Vec, and can't be in any easy way.
        if self.dimensions != position.len() {
            return Err(format!(
                "Incorrect number of dimensions, expected {}, got {} for {:?}",
                self.dimensions,
                position.len(),
                position
            ));
        }*/

        let mut cells = vec![];
        let mut offsets = vec![];
        for k in 0..self.dimensions {
            let (id, offset) = self.coordinates[k].key_down(&position[k]);
            cells.push(id);
            offsets.push(offset);
        }

        Ok((cells, offsets))
    }

    // Round up to the next element or self if in the space
    pub fn key_up(&self, position: &K) -> Result<(Vec<usize>, Vec<usize>), String> {
        //TODO: Should we check inside each objects, or just assume it is correct and/or rely on the bound checks?
        /* This impose to require ExactSizeIterator, which is not implemented on Vec, and can't be in any easy way.
        if self.dimensions != position.len() {
            return Err(format!(
                "Incorrect number of dimensions, expected {}, got {} for {:?}",
                self.dimensions,
                position.len(),
                position
            ));
        }*/

        let mut cells = vec![];
        let mut offsets = vec![];
        for k in 0..self.dimensions {
            let (id, offset) = self.coordinates[k].key_up(&position[k]);
            cells.push(id);
            offsets.push(offset);
        }

        Ok((cells, offsets))
    }

    pub fn value(&self, cells_id: Vec<usize>, offsets: Vec<usize>) -> Result<Vec<&V>, String> {
        //TODO: Should we check inside each objects, or just assume it is correct and/or rely on the bound checks?
        if self.dimensions != cells_id.len() {
            return Err(format!(
                "Incorrect number of dimensions, expected {}, got {} for {:?}",
                self.dimensions,
                cells_id.len(),
                cells_id
            ));
        }

        //TODO: Should we check inside each objects, or just assume it is correct and/or rely on the bound checks?
        if self.dimensions != offsets.len() {
            return Err(format!(
                "Incorrect number of dimensions, expected {}, got {} for {:?}",
                self.dimensions,
                offsets.len(),
                offsets
            ));
        }

        let mut values = vec![];
        for k in 0..self.dimensions {
            values.push(self.coordinates[k].value(cells_id[k], offsets[k]));
        }

        Ok(values)
    }
}
