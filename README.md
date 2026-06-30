# Delphi.Parallel.FileScanner

Get a file list in parallel.

Currently more or less a one-trick pony: it collects files matching a list of
extensions from a list of root directories. You can exclude files by path prefix
and by filename suffix.

It scans the given list of folders in parallel. This pays off when you can split
the work into several smaller pieces (e.g. there is little benefit when scanning
just one large folder).

## Variants

- `TParallelFileScanner` &mdash; returns results in a standard RTL `TStringList`.
- `TParallelFileScannerSpring` &mdash; returns results in a Spring4D `IList<string>`.

The threading backend is selected at compile time in
`Source/Units/DPFSUnit.Parallel.FileScanner.inc`:

- Define `USE_OMNI_THREAD_LIBRARY` to use OmniThreadLibrary (the default).
- Leave it undefined to use the RTL PPL (`System.Threading`).

## TODO

- Code needs further refactoring.
- Filtering should be done while searching &mdash; at least for prefixes, since
  there is no point starting to scan a folder that will be ignored.
- ...
