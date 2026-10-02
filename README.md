# Delphi.Parallel.FileScanner

Get a file list in parallel.

Currently more or less a one-trick pony: it collects files matching a list of
search patterns from a list of root directories. You can exclude folders and files by
wildcard pattern, by folder path prefix and by filename suffix.

It walks the roots with one worker per CPU core. Each worker walks a subtree depth-first
on its own private stack, enumerating every directory exactly once (`FindFirstFileEx` with
the basic info level and large fetch), and hands its shallowest pending directories &mdash;
the biggest remaining subtrees &mdash; to any worker that has run out of work. A single
dominant subtree (a vendored library folder, `C:\Windows\WinSxS`, ...) is therefore spread
across all cores instead of pinning one thread, while a balanced tree pays almost nothing
for the coordination: the shared lock is only taken when work actually changes hands.

Entries are examined straight in the find-data buffer; a path string is only built for a
matching file or a subdirectory to walk. With `SortResultList`, every worker sorts its own
files as soon as its walk is done (in parallel) and the results are merged, instead of one
full sort at the end.

## Matching rules

Wildcards are [Delphi.WildCardMatcher](Source/3rdPartyLibraries/Delphi.WildCardMatcher/README.md)
patterns (`*`, `?`, `#` for a digit, `[a-z]`, `["foo"|"bar"]`), case-insensitive, with `*`
matching across folder boundaries (DOS style). `DPFSUnit.Parallel.FileScanner` needs that
unit on the search path; it is pure RTL.

- **Search patterns** (the list given to `Create`) match file **names**. A plain `*.ext` is a
  fast, allocation-free extension test; anything else is a wildcard, e.g. `Unit?.pas` or
  `*["Form"|"Frame"]*.pas`. A pattern containing a path delimiter, an empty one or a
  malformed one raises `EInOutArgumentException` when the scan starts.
- **Exclusions** (`TFileScanExclusions`) combine: anything any of them matches is left out.
  - `Patterns` are wildcards matched against **full paths**, e.g. `*\__history\*`,
    `*\.git\*` or `C:\MyCode\*["3rdParty"|"ThirdParty"]\*`. A pattern ending in `*` that
    matches a folder (its path plus `\`) prunes the whole folder from the walk; every
    pattern is also matched against each file's full path, so `*.inc` excludes files. A
    pattern ending in `\` names folders, as in `.gitignore`: `*\__history\` means
    `*\__history\*`. A malformed pattern raises `EInOutArgumentException`.
  - `PathPrefixes` exclude a folder and everything under it, by path. Whole folder names
    only: `C:\Code\Lib` does not exclude `C:\Code\Library`.
  - `PathSuffixes` exclude files whose full path ends with one of them.
  - Blank entries are ignored.
- Roots are walked once each: a root that is the same as, or inside, another root is
  dropped before the walk (case-insensitively), so overlapping roots do not produce
  duplicate results. A nested root inside an excluded folder is still scanned, since it
  was asked for explicitly.
- Search patterns, path prefixes and filename suffixes are compared ordinally and
  case-insensitively, the way the file system compares names &mdash; not by the current
  locale's rules.
- The sorted order is `CompareText` order.
- `SkippedFilesCount` counts the excluded files: those excluded by a pattern or suffix,
  plus the files the search patterns match inside pruned folders (counted on demand).

Wildcard exclusions cost next to nothing: the folder and file paths they are matched
against are built by the walk anyway. Scanning `C:\git_opensource` (49k folders, 250k files)
with five patterns (`*\.git\*`, `*\__history\*`, `*\__recovery\*`, `*\3rdParty*\*`,
`*\ThirdParty*\*`) takes as long as with the 284 absolute folder prefixes they replace; the
matching itself is about 20 ms of CPU per scan, spread over all workers.

## Scanner classes

All of them share `TParallelFileScannerCustom` (in `DPFSUnit.Parallel.FileScanner`), which
does all the work &mdash; matching, the walk, sorting &mdash; and has the common API:
`GetFileList` into a `TStringList`, and the streaming `ScanFiles` callbacks. Only running
the walk's workers is left to the descendant, so code can be written against
`TParallelFileScannerCustom` whatever the threading library. Worker priorities are the
RTL's `TThreadPriority`.

| Class | Unit | Workers run on | Needs | Extra results |
| --- | --- | --- | --- | --- |
| `TParallelFileScanner` | `DPFSUnit.Parallel.FileScanner` | RTL PPL (`System.Threading`) | RTL only | &mdash; |
| `TParallelFileScannerOTL` | `DPFSUnit.Parallel.FileScanner.OTL` | OmniThreadLibrary | OmniThreadLibrary | OTL value queue (`TOmniQueue`) |
| `TParallelFileScannerSpring` | `DPFSUnit.Parallel.FileScanner.Spring` | RTL PPL (`System.Threading`) | Spring4D | Spring4D `IList<string>` |

`DPFSUnit.Parallel.FileScanner` alone is all you need without external libraries. The
defines in `Source/Units/DPFSUnit.Parallel.FileScanner.inc` (`USE_OMNI_THREAD_LIBRARY`,
`USE_SPRING4D`) only switch the optional units on and off: undefined, they compile to empty
units, and the demo app disables the buttons that use them.

With OmniThreadLibrary the workers are pooled tasks that each call owns and releases
itself (not `Parallel.For`, whose Unobserved tasks are only released once the calling
thread processes their termination messages), so scanning needs no message loop: a
console app, a service, a worker thread or a loop of back-to-back scans is fine. They run
in the scanner's own pool of at most one thread per core (at most 56), so back-to-back
scans reuse the same threads, and a scan started from inside a `ScanFiles` callback runs
on that callback's thread. `TParallelFileScannerOTL.ToOTLThreadPriority` converts the
priority; OTL has no time-critical level, so `tpTimeCritical` becomes OTL's `tpHighest`.

With every scanner class an exception in a worker, e.g. one raised by a `ScanFiles`
callback, is raised in the caller.

## Memory manager

All workers allocate path strings concurrently, so the memory manager matters. Delphi's
built-in memory manager answers lock contention with `Sleep(10)`, which can add ~10 ms
stalls to a scan; FastMM5 (used by the demo app, first unit in its `.dpr`) does not.

## Tests

`Tests/UnitTests` (`DPFSUnitTests`) is the DUnitX suite, for Win32 and Win64. The shared
tests run on every scanner class (`TParallelFileScanner`, `TParallelFileScannerOTL`): every
result API returns the same file set as a straightforward flat enumeration of this
repository's `Source` tree, prefix exclusion keeps excluded subtrees out, overlapping roots
give no duplicates, sorted results are in `CompareText` order, the workers run at the
requested priority, a worker's exception reaches the caller, a scan from inside a callback
works, and back-to-back scans pile up no handles or memory. Then the OTL value queue, the
priority conversion and the Spring4D `IList<string>` are tested.

| Configuration | Runs the tests with |
| --- | --- |
| `Debug`, `Release` | the DUnitX console runner (NUnit XML to `dunitx-results.xml`) |
| `Debug - TestInsight`, `Release - TestInsight` | TestInsight in the IDE (`TESTINSIGHT` defined) |

Debug builds run with FastMM5 in debug mode, so memory errors fail loudly. The worker
exception test raises `EScannerTestCallbackFailure` on purpose; add it to the debugger's
ignored exceptions when running under the debugger. From the command line, build a
non-TestInsight configuration (a TestInsight build only talks to the IDE), e.g.
`msbuild Tests\UnitTests\DPFSUnitTests.dproj /p:Config=Debug /p:Platform=Win64`, and run
`DPFSUnitTests.exe --exitbehavior:Continue`.

`Tests/ScannerTests` (`DPFSScannerTests`) keeps the checks that need a process of their
own, since they measure process-wide state from a clean start: that back-to-back scans keep
the OTL scanner pool's thread count bounded. It exits with a non-zero code on failure.

## TODO

- `GetFileCounts` (used only for the lazy "skipped files in excluded directories" count)
  still walks each skipped directory once per extension; it could share the single-pass walk.
- ...
