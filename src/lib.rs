#![forbid(unsafe_code)]
#![warn(missing_docs)]

//! # Iron Sea - Index SFC DBC
//!
//! Index for the Iron Sea toolkit, based on a Space Filling Curve (SFC),
//! over Dictionary-Based Compression (DBC), which offers great
//! performances for both range queries over point cloud data and at the
//! same time uses a storage-efficient index.
//!
//! More details in the [paper].
//!
//! [paper]: https://infoscience.epfl.ch/record/232536?ln=en
//!
//! ## Iron Sea: Database Toolkit
//! **Iron Sea** provides a set of database engine bricks, which can be
//! combined and applied on arbitrary data structures.
//!
//! Unlike a traditional database, it does not assume a specific
//! physical structure for the tables nor the records, but relies on the
//! developer to provide a set of extractor functions which are used by
//! the specific indices provided.
//!
//! This enables the index implementations to be agnostic from the
//! underlying data structure, and re-used.
//!

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
