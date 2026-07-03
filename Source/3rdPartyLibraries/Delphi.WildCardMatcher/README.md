# Delphi.WildCardMatcher

Simple Windows / DOS style wildcard matcher for Delphi, with a couple of
useful extensions (`#` for digits, quoted-string alternation inside `[...]`).

Pure Pascal, single unit, no dependencies beyond the RTL.

## Wildcard syntax

| Token              | Matches                                                  | Example pattern             | Matches                  | Does not match    |
| ------------------ | -------------------------------------------------------- | --------------------------- | ------------------------ | ----------------- |
| `*`                | Zero or more characters                                  | `wh*`                       | `what`, `why`            | `awhile`, `watch` |
| `?`                | Exactly one character                                    | `b?ll`                      | `ball`, `bell`           | `bll`, `baal`     |
| `#`                | Exactly one decimal digit (0-9)                          | `1#3`                       | `103`, `113`             | `1a3`, `13`       |
| `[abc]`            | Any one character in the set                             | `b[ae]ll`                   | `ball`, `bell`           | `bill`            |
| `[!abc]`           | Any one character NOT in the set                         | `b[!ae]ll`                  | `bill`, `bull`           | `ball`, `bell`    |
| `[a-z]`            | Any one character in the range (low..high, ascending)    | `b[a-c]d`                   | `bad`, `bbd`             | `bdd`, `b0d`      |
| `["foo"\|"bar"]`   | Any ONE of the listed literal strings at this position   | `*["3rdparty"\|"ThirdParty"]*.md` | `docs/3rdparty/x.md`, `src/ThirdPartyReadme.md` | `docs/internal/x.md` |
| `[!"foo"\|"bar"]`  | A slice of length `max(altLen)` that is NEITHER prefix   | `[!"foo"\|"bar"]*`          | `quxyz`                  | `fooxyz`, `barxyz`|

Sets may combine literals and ranges, e.g. `[a-zA-Z0-9_]`.
A literal `]` inside a single-char class must be the first content character:
`[]abc]` matches `]`, `a`, `b` or `c`.
A `-` that is the first or last content character is treated as a literal,
e.g. `[-x]` matches `-` or `x`; `[a-]` matches `a` or `-`.

`**` collapses to `*`.

### Two flavours of `[...]`

`[...]` is **auto-detected**:

- If the first content character (after an optional `!`) is `"`, the class
  is parsed as **quoted-string alternation**: `["foo"|"bar"|"baz"]`. The
  `|` separates alternatives. The class matches the FIRST alternative that
  succeeds at the current position and consumes its length.
- Otherwise the class follows the **legacy single-character** rules
  (`[abc]`, `[a-z]`, `[!xyz]`) and consumes exactly one character.

An empty alternative `""` is allowed and matches zero characters
(`[""|"foo"]` will match either nothing or `foo`).

Quoted alternation is intended for file-mask use. Backslash escapes are
NOT supported - none are needed because Windows file names cannot contain
the characters `[`, `]`, `|` or `"` to begin with, so the syntax stays
safe to embed in literal masks.

Negated alternation `[!"foo"|"bar"]` succeeds when NONE of the listed
alternatives is a prefix at the current position; it then consumes the
length of the LONGEST alternative. When the input has fewer characters
left than the longest alternative, the match fails.

`|` OUTSIDE of `[...]` has no special meaning and is treated as a literal
character.

Matching is **case-insensitive by default** (Windows convention).
Pass `ACaseSensitive = True` for ordinal comparison.

## Usage

```pascal
uses
  Delphi.WildCardMatcher;
```

### One-off match

`TWildCard.Create` (no patterns) gives you an empty matcher you can use
for ad-hoc one-shot calls. Case-sensitivity is set at `Create` time.

```pascal
if TWildCard.Create.Match(AFileName, '*.pas') then
  // ...

// Case-sensitive
if TWildCard.Create(True).Match('Unit1.PAS', '*.pas') then
  // ... will NOT match because of the trailing-case difference
```

### Pre-registered patterns (recommended for repeated matching)

When you match many inputs against the same fixed pattern set, register
the patterns at `Create`. Registered patterns are **compiled once** into
token programs (literal runs become block compares, classes and
alternations are parsed once, `*` scans are length-pruned), which makes
repeated matching several times faster than the ad-hoc form. Then
`Match(input)` walks the registered set short-circuiting on the first hit.

```pascal
var
  LMask: TWildCard;
begin
  LMask := TWildCard.Create(['*.pas', '*.dpr', '*.dpk', '*.inc']);
  for var LFile in TDirectory.GetFiles(ARoot) do
    if LMask.Match(LFile) then
      AddToProjectFileList(LFile);
end;
```

The constructor accepts a single pattern, a `TArray<string>`, or a
`TStrings` (handy for patterns loaded from a `TStringList` /
`Memo.Lines` / `.ini` file):

```pascal
var
  LMasks: TStringList;
  LIgnore: TWildCard;
begin
  LMasks := TStringList.Create;
  try
    LMasks.LoadFromFile('ignore-masks.txt');
    LIgnore := TWildCard.Create(LMasks);

    for var LFile in TDirectory.GetFiles(ARoot) do
      if not LIgnore.Match(LFile) then
        ProcessFile(LFile);
  finally
    LMasks.Free;
  end;
end;
```

### Ad-hoc pattern on a registered instance

You can pass an extra one-off pattern to an existing instance. By
default only that pattern is tried; pass `True` as the third argument
to also try the registered set.

```pascal
LMask.Match(LFile, '*.dproj');           // only the ad-hoc pattern
LMask.Match(LFile, '*.dproj', True);     // ad-hoc + registered set
```

### Include / exclude filtering with `TWildCardFilter`

`TWildCardFilter` wraps two matchers - one list that filters IN and one
that filters OUT - with the usual filtering conventions:

- empty include list = everything is included (the include list only
  restricts when it has patterns)
- empty exclude list = nothing is excluded
- **exclude always wins over include**

```pascal
var
  LFilter: TWildCardFilter;
begin
  LFilter := TWildCardFilter.Create(
    ['*.pas', '*.dpr', '*.inc'],                 // filter in
    ['*\__history\*', '*backup*', '*.tmp']);     // filter out

  for var LFile in TDirectory.GetFiles(ARoot) do
    if LFilter.Accepts(LFile) then
      ProcessFile(LFile);
end;
```

Both lists accept the full pattern syntax and are compiled at `Create`.
Constructors take two single patterns, two `TArray<string>` or two
`TStrings` (either may be `nil` / empty), plus the usual trailing
`ACaseSensitive` flag that applies to both lists.

### Quoted-string alternation in practice

Quoted alternation collapses several "same shape, different word" patterns
into a single mask. Instead of:

```pascal
LMask := TWildCard.Create(['*3rdparty*.md', '*ThirdParty*.md']);
```

you can write:

```pascal
LMask := TWildCard.Create('*["3rdparty"|"ThirdParty"]*.md');
```

The negated form is handy for "skip files whose name contains any of
these tokens":

```pascal
if TWildCard.Create.Match(LFile, '*[!"backup"|"draft"|"old"]*.docx') then
  ProcessOfficialDocument(LFile);
```

(Bear in mind negated alternation consumes a fixed-length slice equal to
the longest alternative - it is not a true word-boundary check, just a
positional negation.)

## API

```pascal
type
  TWildCard = record
  public
    // Constructors - ACaseSensitive is locked in for the lifetime of the
    // instance and defaults to case-insensitive (Windows convention).
    class function Create(const ACaseSensitive: Boolean = False): TWildCard; overload; static;
    class function Create(const APattern: string;
      const ACaseSensitive: Boolean = False): TWildCard; overload; static;
    class function Create(const APatterns: TArray<string>;
      const ACaseSensitive: Boolean = False): TWildCard; overload; static;
    class function Create(const APatterns: TStrings;
      const ACaseSensitive: Boolean = False): TWildCard; overload; static;

    // Match against the registered set only
    function Match(const AInput: string): Boolean; overload;

    // Match against an ad-hoc pattern; AAlsoMatchRegistered=True also
    // tries the registered set after the ad-hoc one fails.
    function Match(const AInput, APattern: string;
      const AAlsoMatchRegistered: Boolean = False): Boolean; overload;
    function Match(const AInput: string; const APatterns: TArray<string>;
      const AAlsoMatchRegistered: Boolean = False): Boolean; overload;
    function Match(const AInput: string; const APatterns: TStrings;
      const AAlsoMatchRegistered: Boolean = False): Boolean; overload;

    property CaseSensitive: Boolean read FCaseSensitive;
    property RegisteredPatterns: TArray<string> read FPatterns;
  end;

  TWildCardFilter = record
  public
    class function Create(const ACaseSensitive: Boolean = False): TWildCardFilter; overload; static;
    class function Create(const AIncludePattern, AExcludePattern: string;
      const ACaseSensitive: Boolean = False): TWildCardFilter; overload; static;
    class function Create(const AIncludePatterns, AExcludePatterns: TArray<string>;
      const ACaseSensitive: Boolean = False): TWildCardFilter; overload; static;
    class function Create(const AIncludePatterns, AExcludePatterns: TStrings;
      const ACaseSensitive: Boolean = False): TWildCardFilter; overload; static;

    // True when AInput matches the include stage (or the include list is
    // empty) and does not match any exclude pattern.
    function Accepts(const AInput: string): Boolean;

    property CaseSensitive: Boolean read FCaseSensitive;
    property IncludePatterns: TArray<string> read GetIncludePatterns;
    property ExcludePatterns: TArray<string> read GetExcludePatterns;
  end;
```

The multi-pattern overloads return `True` on the first pattern that matches
and `False` on an empty pattern list. They do not report which pattern
matched - keep the call site simple.

## Requirements

Modern Delphi (records with methods, `class function ... static`, generics).
Tested on Delphi 12.x. No third-party dependencies.

On Win32 the hottest `*`-scan loops use SSE2 kernels **by default** -
roughly 1.3-2.4x faster on backtracking-heavy patterns, no effect on
simple masks. Define `PUREPASCAL` to opt out and compile plain pascal
loops for builds that must run on very old CPUs. All other targets
always compile the pure pascal path. The choice is static (no runtime
CPU detection). SSE2 is sufficient here - newer instruction sets
(SSE4.2, AVX2) would not bring significant gains for file-mask-length
inputs. See [Benchmarks/RESULTS.md](Benchmarks/RESULTS.md) for
measurements.

## Tests

DUnitX-based test suite under `Unittests\`. Open
`Delphi.WildCardMatcher.Tests.dproj` or run from the command line:

```
msbuild Unittests\Delphi.WildCardMatcher.Tests.dproj /t:Build /p:Config=Debug /p:Platform=Win32
Unittests\Win32\Debug\Delphi.WildCardMatcher.Tests.exe
```

## Benchmarks

A standalone performance harness lives under `Benchmarks\`. It first runs
a parity suite (the compiled registered-pattern engine and the
interpreting ad-hoc engine must agree on every edge case, CI and CS) and
then times both engines over realistic scenarios. Build/run instructions
are in the `.dpr` header; historical numbers in
[Benchmarks/RESULTS.md](Benchmarks/RESULTS.md).

## License

See [LICENSE](LICENSE).
