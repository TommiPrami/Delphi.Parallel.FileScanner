# Delphi.Parallel.FileScanner

Get a file list in parallel.

Currently more or less a one-trick pony: it collects files matching a list of
extensions from a list of root directories. You can exclude files by path prefix
and by filename suffix.

It walks the roots with one worker per CPU core. Each worker walks a subtree depth-first
on its own private stack, enumerating every directory exactly once (`FindFirstFileEx` with
the basic info level and large fetch), and hands its shallowest pending directories &mdash;
the biggest remaining subtrees &mdash; to any worker that has run out of work. A single
dominant subtree (a vendored library folder, `C:\Windows\WinSxS`, ...) is therefore spread
across all cores instead of pinning one thread, while a balanced tree pays almost nothing
for the coordination: the shared lock is only taken when work actually changes hands.

## Variants

- `TParallelFileScanner` &mdash; returns results in a standard RTL `TStringList`.
- `TParallelFileScannerSpring` &mdash; returns results in a Spring4D `IList<string>`.

The threading backend is selected at compile time in
`Source/Units/DPFSUnit.Parallel.FileScanner.inc`:

- Define `USE_OMNI_THREAD_LIBRARY` to use OmniThreadLibrary (the default).
- Leave it undefined to use the RTL PPL (`System.Threading`).

## Memory manager

All workers allocate path strings concurrently, so the memory manager matters. Delphi's
built-in memory manager answers lock contention with `Sleep(10)`, which can add ~10 ms
stalls to a scan; FastMM5 (used by the demo app, first unit in its `.dpr`) does not.

## Tests

`Tests/ScannerTests` is a console regression test: it checks that every result-container
API (RTL `TStringList`, the OmniThreadLibrary value queue, and the Spring4D `IList<string>`)
returns the same file set as a straightforward flat enumeration, and that prefix exclusion
keeps excluded subtrees out. It exits with a non-zero code on failure.

## TODO

- `GetFileCounts` (used only for the lazy "skipped files in excluded directories" count)
  still walks each skipped directory once per extension; it could share the single-pass walk.
- ...
