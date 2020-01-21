#![forbid(unsafe_code)]

#[macro_use]
extern crate log;

#[macro_use]
extern crate arrayref;

mod cell_space;
mod morton;
mod sfc;

pub use sfc::Record;
pub use sfc::RecordFields;
pub use sfc::SpaceFillingCurve as IndexOwned;
