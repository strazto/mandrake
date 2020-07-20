# Mandrake 0.0.1

Tentative First-Pass

- Implement roxygen-based `@col` tag + column doc workflow [#8](https://github.sydney.edu.au/speed-extract/mandrake/issues/8).
  - Implement serialization of column docs from docstrings to `yaml`.
  - Implement deserialization of column docs, by specifying package owning the
    column specs.
- Implement Column spec lookup, extraction and formatting / integration with workflow
  graphs [#6](https://github.sydney.edu.au/speed-extract/mandrake/issues/6).
  - Incl. Extraction of metadata from plan.


# mandrake 0.0.0.9002

Initial Dev Version

## Implementations

- Implement target-doc extraction function `extract_column_names()`
  - This uses the plan and the cache to extract column names from each target,
  - and returns the plan with a new list-col containing that target's variable names.

## Demonstrations

- Add demo article "Test Usecase", showing the ability to embed metadata about a specific
  target directly into the network graph

