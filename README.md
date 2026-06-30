# Delphi.Parallel.FileScanner

Get a file list in parallel.

Currently more or less a one-trick pony: it collects files matching a list of
extensions from a list of root directories. You can exclude files by path prefix
and by filename suffix.

It walks each root directory once and spreads the work across CPU cores by splitting
every root into its immediate subdirectories (plus the root's own files). That means
it scales both with the number of root directories and with the number of
subdirectories inside a single large root.

## Variants

- `TParallelFileScanner` &mdash; returns results in a standard RTL `TStringList`.
- `TParallelFileScannerSpring` &mdash; returns results in a Spring4D `IList<string>`.

The threading backend is selected at compile time in
`Source/Units/DPFSUnit.Parallel.FileScanner.inc`:

- Define `USE_OMNI_THREAD_LIBRARY` to use OmniThreadLibrary (the default).
- Leave it undefined to use the RTL PPL (`System.Threading`).

## Tests

`Tests/ScannerTests` is a console regression test: it checks that every result-container
API (RTL `TStringList`, the OmniThreadLibrary value queue, and the Spring4D `IList<string>`)
returns the same file set as a straightforward flat enumeration, and that prefix exclusion
keeps excluded subtrees out. It exits with a non-zero code on failure.

## TODO

- Better parallel load balancing (work stealing). Today each root is split into one
  recursive job per immediate subdirectory, so when a single subtree dominates (e.g. a
  vendored library folder) it becomes one recursive job on one thread &mdash; and the scan
  can even be slower than single-threaded, because the other workers idle while that thread
  does all the work. A dynamic work-stealing walk (workers pull subdirectories from a shared
  queue as they are discovered) would spread the load; the tricky part is termination
  detection (knowing when the queue is empty *and* no worker will add more).
- `GetFileCounts` (used only for the lazy "skipped files in excluded directories" count)
  still walks each skipped directory once per extension; it could share the single-pass walk.
- ...
