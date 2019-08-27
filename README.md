# Iron Sea - Index SFC DBC

Index for the Iron Sea toolkit, based on a Space Filling Curve (SFC), over dictionary-based compression (DBC), which offers great
performances for both range queries over point cloud data and at the same time uses a storage-efficient index.

More details in the paper: https://infoscience.epfl.ch/record/232536?ln=en

## Iron Sea: Database Toolkit

**Iron Sea** provides a set of database engine bricks, which can be combined and applied on arbitrary data structures.

Unlike a traditional database, it does not assume a specific physical structure for the tables nor the records, but relies on the developper to provide a set of extractor functions which are used by the specific indices provided.

This enables the index implementations to be agnostic from the underlying data structure, and re-used.

## Requirements

### Software

 * Rust: https://www.rust-lang.org

## Documentation

For more information, please refer to the [documentation](https://epfl-dias.github.io/ironsea_index_sfc_dbc/).

If you want to build the documentation and access it locally, you can use:

```sh
cargo doc --open
```

## Acknowledgements

This open source software code was developed in part or in whole in the
Human Brain Project, funded from the European Union’s Horizon 2020
Framework Programme for Research and Innovation under the Specific Grant
Agreement No. 785907 (Human Brain Project SGA2).
