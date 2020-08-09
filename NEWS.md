# Mandrake 0.1.0.9004

## Additions

- Implement new tag, `inheritCol`. that allows columns to be inherited from other packages.
  Give a package name, and a list of columns.

## Modifications

- When fetching package name in `roclet_process.roclet_col()`, 
  - if not found from `roxygen2::roxy_meta_get("current_package")`, 
    will attempt to use `env$.packageName`. 
- When processing output for `roclet_col()`, 
  - Writes columns from separate packages to separate files.
- When adding columns to lookup cache, caches columns to three namespaces:
  - All columns are added to
    - `"objects"` (or the default `storr` namespace), 
      where column is referenced by
      - Its name
      - Or any of its aliases
    - `"unique"`, where they are referenced by name only
   - Each column, whose definition is provided by package `srcpkg` is added to
     `"package:{srcpkg}"`, which contains only columns defined by `srcpkg`.
     
## Export / NAMESPACE Changes

- Now use `@importFrom roxygen2 roclet_process`, etc to import roxygen2 generics
- Now export roxygen2 S3 methods as S3Methods.
   
## Bugfixes

- Fixed bug where if no column definitions were given to output, roclet would crash
- Fixed (?) bug where if trying to self-inherit, would fail if docs for a col hadn't already been 
  generated. 
  - Because inheritance and column documentation were processed one-by-one (for each block, process col, then
    inheritCol), this failure would cause things to consistently fail, as `@col` outputs never got written to file,
    to be found by `@inheritCol`.
    - Now, all `@col` are processed, and results cached, then all `@inheritCol` outputs are processed.

# Mandrake 0.1.0

- Implement a top level `decorate_plan()` function. 
  This function wraps the column extraction and linking process, and 
  allows: 
  - The user to specify a column containing markdown description, that 
    will be rendered in the sidebar
  - The `command` column of the plan is parsed, highlighted, and code
    referenced are automagically linked to their docs (thanks to downlit).
    - This is also rendered in a collapsible segment in the sidebar.
  - The table of columns -> docs is still rendered in the sidebar, under the 
    header "columns".

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

