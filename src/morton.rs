use std::fmt;
use std::fmt::Debug;

use serde::de;
use serde::de::Deserialize;
use serde::de::Deserializer;
use serde::de::MapAccess;
use serde::de::SeqAccess;
use serde::de::Visitor;
use serde::ser::Serialize;
use serde::ser::SerializeStruct;
use serde::ser::Serializer;

pub type MortonCode = u32;
pub type MortonValue = u16;

const MORTON_CODE_BITS: usize = 32;
const MORTON_VALUE_BITS: usize = 10;
const MORTON_MAX_VALUES: usize = 1024;

#[derive(Clone)]
pub struct MortonEncoder {
    cell_bits: usize,
    cell_mask: usize,
    dimensions: usize,
    table: Vec<[MortonCode; MORTON_MAX_VALUES]>,
}

impl Debug for MortonEncoder {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "MortonEncoder {{ cell_bits: {}, cell_mask: {}, dimensions: {}, table: ",
            self.cell_bits, self.cell_mask, self.dimensions
        )?;
        write!(f, "[ ")?;
        for k in &self.table {
            write!(f, "[ ")?;
            for v in k.iter() {
                write!(f, "{}, ", v)?;
            }
            write!(f, "], ")?;
        }
        write!(f, "] }}")
    }
}

impl MortonEncoder {
    pub fn new(dimensions: usize, cell_bits: usize) -> Self {
        // Make sure we can store the encoding in a single T.
        // Don't know how to make that test generically
        assert!(MORTON_VALUE_BITS >= cell_bits);
        assert!(MORTON_CODE_BITS >= cell_bits * dimensions);

        //let mut masks = vec![];
        let mut table = vec![];
        let cell_max = 1 << cell_bits;
        let cell_mask = cell_max - 1;

        // Build lookup table & masks
        for k in 0..dimensions {
            table.push([0; MORTON_MAX_VALUES]);
            for i in 0..cell_max {
                let mut v = 0;
                for p in 0..cell_bits {
                    // Note: bit is at position p, so shift it only K-1 p position again below, instead
                    // of K times
                    let bit = i & (1 << p);
                    let new_bit = bit << (p * (dimensions - 1) + k);
                    v |= new_bit;
                }
                table[k][i] = v as MortonCode;
            }
            /*
            let mut v = 0usize;
            for p in 0..cell_bits {
                let new_bit = 1 << p * (dimensions - 1) + k;
                v = v | new_bit;
            }
            masks.push(v as MortonCode);
            */
        }

        MortonEncoder {
            cell_bits,
            cell_mask,
            dimensions,
            table,
            //masks,
        }
    }

    fn encode_1(&self, k: usize, v: MortonValue) -> MortonCode {
        // Already done by the array bound checker anyway
        //assert!((v as usize) < MORTON_MAX_VALUES);
        //assert!(k < self.table.len());

        // Ensure we only have valid values in inputs, even when less bits than
        // the maximum is used to define those values.
        let v = v as usize & self.cell_mask;
        self.table[k][v]
    }

    fn decode_1(&self, k: usize, code: MortonCode) -> MortonValue {
        // Already done by the array bound checker anyway
        //assert!(k < self.table.len());

        let mut v = 0;

        for i in 0..self.cell_bits {
            let bit_pos = i * self.table.len() + k;
            let bit = code as usize & (1 << bit_pos);
            let bit_pos = bit_pos - i;
            v |= (bit >> bit_pos) as MortonValue;
        }

        v as MortonValue
    }

    pub fn encode(&self, v: &[MortonValue]) -> Result<MortonCode, String> {
        //TODO: Should we check inside each objects, or just assume it is correct and/or rely on the bound checks?
        if self.dimensions != v.len() {
            return Err(format!(
                "Incorrect number of dimensions, expected {}, got {} for {:?}",
                self.dimensions,
                v.len(),
                v
            ));
        }

        let mut code = 0;

        for (k, i) in v.iter().enumerate().take(self.dimensions) {
            code |= self.encode_1(k, *i);
        }

        Ok(code)
    }

    pub fn decode(&self, code: MortonCode) -> Vec<MortonValue> {
        let mut values = vec![];

        for k in 0..self.dimensions {
            values.push(self.decode_1(k, code));
        }

        values
    }
}

impl Serialize for MortonEncoder {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        // We serialize the minimum amount of information necessary to
        // deserialize the table.
        // This is the parameters to init(dimensions, cell_bits)
        let mut state = serializer.serialize_struct("MortonEncoder", 2)?;
        state.serialize_field("cell_bits", &self.cell_bits)?;
        state.serialize_field("dimensions", &self.dimensions)?;
        state.end()
    }
}

impl<'de> Deserialize<'de> for MortonEncoder {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        enum Field {
            CellBits,
            Dimensions,
        };

        impl<'de> Deserialize<'de> for Field {
            fn deserialize<D>(deserializer: D) -> Result<Field, D::Error>
            where
                D: Deserializer<'de>,
            {
                struct FieldVisitor;

                impl<'de> Visitor<'de> for FieldVisitor {
                    type Value = Field;

                    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                        formatter.write_str("`cell_bits` or `dimensions`")
                    }

                    fn visit_str<E>(self, value: &str) -> Result<Field, E>
                    where
                        E: de::Error,
                    {
                        match value {
                            "cell_bits" => Ok(Field::CellBits),
                            "dimensions" => Ok(Field::Dimensions),
                            _ => Err(de::Error::unknown_field(value, FIELDS)),
                        }
                    }
                }

                deserializer.deserialize_identifier(FieldVisitor)
            }
        }

        struct MortonEncoderVisitor;

        impl<'de> Visitor<'de> for MortonEncoderVisitor {
            type Value = MortonEncoder;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("struct MortonEncoder")
            }

            fn visit_seq<V>(self, mut seq: V) -> Result<MortonEncoder, V::Error>
            where
                V: SeqAccess<'de>,
            {
                let cell_bits = seq
                    .next_element()?
                    .ok_or_else(|| de::Error::invalid_length(0, &self))?;
                let dimensions = seq
                    .next_element()?
                    .ok_or_else(|| de::Error::invalid_length(1, &self))?;
                Ok(MortonEncoder::new(dimensions, cell_bits))
            }

            fn visit_map<V>(self, mut map: V) -> Result<MortonEncoder, V::Error>
            where
                V: MapAccess<'de>,
            {
                let mut cell_bits = None;
                let mut dimensions = None;
                while let Some(key) = map.next_key()? {
                    match key {
                        Field::CellBits => {
                            if cell_bits.is_some() {
                                return Err(de::Error::duplicate_field("cell_bits"));
                            }
                            cell_bits = Some(map.next_value()?);
                        }
                        Field::Dimensions => {
                            if dimensions.is_some() {
                                return Err(de::Error::duplicate_field("dimensions"));
                            }
                            dimensions = Some(map.next_value()?);
                        }
                    }
                }
                let cell_bits = cell_bits.ok_or_else(|| de::Error::missing_field("cell_bits"))?;
                let dimensions =
                    dimensions.ok_or_else(|| de::Error::missing_field("dimensions"))?;
                Ok(MortonEncoder::new(dimensions, cell_bits))
            }
        }

        const FIELDS: &[&str] = &["cell_bits", "dimensions"];
        deserializer.deserialize_struct("MortonEncoder", FIELDS, MortonEncoderVisitor)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    mod init {
        use super::*;

        /* Check the assertions */
        #[test]
        #[should_panic]
        fn dim1_bit32() {
            let _m = MortonEncoder::new(1, 31);
        }

        #[test]
        #[should_panic]
        fn dim2_bit16() {
            // Max 10 bit for the codes, even if 16 would fit
            let _m = MortonEncoder::new(2, 16);
        }

        #[test]
        #[should_panic]
        fn dim33_bit1() {
            let _m = MortonEncoder::new(33, 1);
        }

        #[test]
        #[should_panic]
        fn dim17_bit2() {
            let _m = MortonEncoder::new(17, 2);
        }

        #[test]
        fn dim1_bit10() {
            let _m = MortonEncoder::new(1, 10);
        }

        #[test]
        fn dim2_bit10() {
            let _m = MortonEncoder::new(2, 10);
        }

        #[test]
        fn dim3_bit10() {
            let _m = MortonEncoder::new(3, 10);
        }

        #[test]
        fn dim4_bit8() {
            let _m = MortonEncoder::new(4, 8);
        }

        #[test]
        fn dim32_bit1() {
            let _m = MortonEncoder::new(32, 1);
        }

        /*
            morton_init();
        // Morton table looks OK
        //    for n in 0..10 {
        //        println!("{:4}", n);
        //        for k in 0..K {
        //            println!("{:032b}", unsafe {MORTON[k][n]});
        //        }
        //    }

            for n in 0..CELL_MAX {
                println!("## {:04}", n);
                let mut c = 0 as Code;
                for k in 0..K {
                    // check diagonal
                    c = c | morton_encode(k, n as u16);
                }
                let f = n as u16;
                for k in 1..2 {
                    // check diagonal
                    let p = morton_decode(k, c);
                    println!("\n{:04} \n f {:04}\n p {:04}\n 𝚫 {:06}\n", c, f, p, f-p);

                }
            }


            let mut f = 0.0f64;
        //    while f < 1.0 {
        //        let v = convert_to_fixed(&f);
        //        let p = convert_to_f64(&v);
        //        println!("\n{:010} \n f {:+0.16e}\n p {:+03.16e}\n 𝚫 {:+03.16e}\n", v, f, p, f - p);
        //
        //        f += 0.1e-1;
        //    }

            let f =0.000724939184752;
            let v = convert_to_fixed(&f);
            let p = convert_to_f64(&v);
            println!("\n{:010} \n f {:+0.16e}\n p {:+03.16e}\n 𝚫 {:+03.16e}\n", v, f, p, f - p);

        */
    }

    mod encode {
        use super::*;

        /* Check the lookup table produced */
        #[test]
        fn dim1_bit10() {
            let m = MortonEncoder::new(1, 10);
            for n in 0..MORTON_MAX_VALUES {
                assert_eq!(n as MortonCode, m.encode_1(0, n as MortonValue));
            }
        }

        #[test]
        fn table_dim2_bit10() {
            let m = MortonEncoder::new(2, 10);
            let mut lookup = Vec::<Vec<MortonCode>>::new();

            for k in 0..2 {
                lookup.push(Vec::new());

                for n in 0..MORTON_MAX_VALUES {
                    // Morton numbers are number where the bit are exploded so that we can
                    // interleave them. This means that for each position of a value, we need to
                    // insert dimensions - 1 columns between each bits, and shift that result by the
                    // dimension number so that we can OR all the dimensions together without having
                    // bits colliding.
                    let mut v = 0;
                    for p in 0..MORTON_VALUE_BITS {
                        let b = (n & (1 << p)) >> p;
                        v = v | b << (p * 2 + k);
                    }
                    lookup[k].push(v as MortonCode);
                }
            }

            for k in 0..2 {
                for n in 0..MORTON_MAX_VALUES {
                    assert_eq!(lookup[k][n], m.encode_1(k, n as MortonValue));
                }
            }
        }

        fn check(dimensions: usize, value_max: usize, value_bits: usize, m: MortonEncoder) -> () {
            let mut lookup = Vec::<Vec<MortonCode>>::new();

            for k in 0..dimensions {
                lookup.push(Vec::new());

                for n in 0..value_max {
                    // Morton numbers are number where the bit are exploded so that we can
                    // interleave them. This means that for each position of a value, we need to
                    // insert dimensions -1 columns between each bits, and shift that result by the
                    // dimension number so that we can OR all the dimensions together without having
                    // bits colliding.
                    let mut v = 0;
                    for p in 0..value_bits {
                        let b = (n & (1 << p)) >> p;
                        v = v | b << (p * dimensions + k);
                    }
                    lookup[k].push(v as MortonCode);
                }
            }

            for k in 0..dimensions {
                for n in 0..value_max {
                    assert_eq!(lookup[k][n], m.encode_1(k, n as MortonValue));
                }
            }
        }

        #[test]
        fn table_dim3_bit10() {
            let m = MortonEncoder::new(3, 10);
            check(3, 1024, 10, m);
        }

        #[test]
        fn table_dim4_bit8() {
            let m = MortonEncoder::new(4, 8);
            check(4, 256, 8, m);
        }
    }
}
