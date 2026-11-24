unit DPFSUnit.Parallel.FileScanner;

// Windows only (FindFirstFileEx, Windows thread priorities), so TThreadPriority being Windows-specific is fine.
{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  System.Classes, System.Generics.Collections, System.IOUtils, System.SyncObjs, System.SysUtils, Delphi.WildCardMatcher;

type
  TDirectoryWalkProc = reference to procedure(const AFileName: string);
  // Streaming ScanFiles callback, two flavours:
  // - TFileFoundCallbackProc: anonymous method / method reference (same signature as the internal walk callback)
  // - TFileFoundCallback: classic "of object" event-handler style method pointer
  TFileFoundCallbackProc = TDirectoryWalkProc;
  TFileFoundCallback = procedure(const AFileName: string) of object;

  TExclusionKind = (ekPathPrefixes, ekPathSuffixes, ekPatterns);

  // What a scan leaves out. The three kinds combine - anything any of them matches is excluded - and all compare
  // case-insensitively; blank entries are ignored, and '/' is read as '\' in all of them.
  // - PathPrefixes: folders, as paths. A prefix excludes that folder and everything under it - whole folder names
  //   only, so 'C:\Code\Lib' does not exclude 'C:\Code\Library'.
  // - PathSuffixes: files whose full path ends with one of them, e.g. '_Generated.pas'.
  // - Patterns: Delphi.WildCardMatcher wildcards matched against full paths, '*' crossing folder boundaries, e.g.
  //   '*\__history\*' or 'C:\MyCode\*<"3rdParty"|"ThirdParty">\*'. A pattern ending in '*' that matches a folder
  //   (its path plus '\') prunes that whole folder from the walk; every pattern is also matched against each file's
  //   full path. A pattern ending in '\' names folders, as in .gitignore: '*\__history\' means '*\__history\*'. A
  //   malformed pattern raises EInOutArgumentException when the scan starts.
  // A root inside an excluded folder is scanned whole, as it was asked for: a prefix it lies under does not apply
  // inside it, and a pattern that matches the root folder itself only applies to the part of a path below the root -
  // so '*\3rdParty\*' still excludes a 3rdParty folder inside it.
  TFileScanExclusions = record
  strict private
    FPathPrefixes: TArray<string>;
    FPathSuffixes: TArray<string>;
    FPatterns: TArray<string>;
    function GetPathPrefixesString: string;
    function GetPathSuffixesString: string;
    function GetPatternsString: string;
  public
    procedure InitArrayFromStrings(const AKind: TExclusionKind; const AArrayData: TStrings);
    property PathPrefixes: TArray<string> read FPathPrefixes write FPathPrefixes;
    property PathPrefixesString: string read GetPathPrefixesString;
    property PathSuffixes: TArray<string> read FPathSuffixes write FPathSuffixes;
    property PathSuffixesString: string read GetPathSuffixesString;
    property Patterns: TArray<string> read FPatterns write FPatterns;
    property PatternsString: string read GetPatternsString;
  end;

  // How a scan's workers - parallel tasks - run: how many (Count, always within MinCount..MaxCount) and at what
  // thread priority. Given to a scanner's constructor, which owns and frees it, so give every scanner an instance of
  // its own; nil means one worker per core at normal priority. Defaults: MinCount 1, MaxCount and Count CoreCount,
  // Priority tpNormal. (TParallelFileScannerOTL never runs more than 56 workers.)
  TFileScanWorkers = class(TObject)
  strict private
    FCount: Integer;
    FMaxCount: Integer;
    FMinCount: Integer;
    FPriority: TThreadPriority;
    procedure SetCount(const AValue: Integer);
    procedure SetMaxCount(const AValue: Integer);
    procedure SetMinCount(const AValue: Integer);
  public
    constructor Create;

    // Logical processors of this computer: the default MaxCount, and what InitializePercentage takes a share of.
    class function CoreCount: Integer; static;
    // Sets the limits - -1 means the default: CoreCount for AMaxCount, 1 for AMinCount - and then Count to
    // ADesiredCount within them. An explicit limit beats a default one: Initialize(8, -1, 4) on a 2-core computer
    // allows 4. Limits below 1 (other than -1), or an explicit AMinCount above an explicit AMaxCount, raise.
    procedure Initialize(const ADesiredCount: Integer; const AMaxCount: Integer = -1; const AMinCount: Integer = -1);
    // As Initialize, with Count APercentage percent of CoreCount, rounded: InitializePercentage(66.666, -1, 4) uses
    // two workers for every three cores, but at least 4. Over 100 is allowed (more workers than cores, e.g. for slow
    // network drives); 0 or less raises. A name of its own, as Initialize(50) would quietly mean 50 workers.
    procedure InitializePercentage(const APercentage: Double; const AMaxCount: Integer = -1; const AMinCount: Integer = -1);

    // Setting it outside MinCount..MaxCount gives the nearest limit.
    property Count: Integer read FCount write SetCount;
    // At least 1. Setting MaxCount below MinCount lowers MinCount with it, and the other way round; Count follows.
    property MaxCount: Integer read FMaxCount write SetMaxCount;
    property MinCount: Integer read FMinCount write SetMinCount;
    // The thread priority the workers run at during a scan.
    property Priority: TThreadPriority read FPriority write FPriority;
  end;

  // Base class of the scanners: pattern matching, the load-balanced parallel walk and the results - everything but
  // running the walk's workers, which is left to a descendant's ExecuteWorkers, so this unit needs nothing outside
  // the RTL and Delphi.WildCardMatcher. TParallelFileScanner (below) runs them on the RTL PPL,
  // TParallelFileScannerOTL (DPFSUnit.Parallel.FileScanner.OTL) on OmniThreadLibrary. Priorities are the RTL's
  // TThreadPriority throughout.
  //
  // The extensions given at Create are include patterns matched against file NAMES (not paths): a plain '*.ext' is a
  // fast allocation-free suffix test, anything else is a Delphi.WildCardMatcher wildcard, e.g. 'Unit?.pas' or
  // '*<"Form"|"Frame">*.pas'. Patterns with a path delimiter, empty ones and malformed ones raise
  // EInOutArgumentException when the scan starts.
  //
  // One scan at a time per scanner: a scan started on a scanner that is already scanning - from another thread, or
  // from one of its own ScanFiles callbacks - raises EInvalidOpException. Use a scanner of its own for each.
  //
  // Directory links - junctions, directory symbolic links, volume mount points - inside the scanned tree are not
  // followed unless FollowDirectoryLinks is set: they lead to folders found elsewhere anyway, or back up the tree. A
  // root that is itself a link is scanned.
  //
  // Paths longer than MAX_PATH are walked too, without the process opting in to long paths: a long folder path is
  // opened with the \\?\ prefix, while the paths returned stay as the walk builds them.
  TParallelFileScannerCustom = class(TObject)
  strict private type
    // What the walk excludes: the scan's exclusions as given, or those for inside one root that lies in an excluded
    // folder. Such a root was asked for explicitly, so the exclusions that cover it - a prefix it lies under, a
    // pattern that matches the root folder itself - do not exclude it: a covering prefix is dropped, and a covering
    // pattern is matched only against the part of a path below the root, so '*\3rdParty\*' still excludes a 3rdParty
    // folder inside it. Read concurrently by the workers; never changed after Create.
    TExclusionRules = class(TObject)
    strict private
      FBelowExcludeMatcher: TWildCard; // covering patterns, against the part of a file path below the root
      FBelowPruneMatcher: TWildCard;   // covering patterns ending in '*', against the part of a folder path below it
      FExcludeMatcher: TWildCard;      // the other patterns, against full file paths
      FHasBelowExclude: Boolean;
      FHasBelowPrune: Boolean;
      FHasExclude: Boolean;
      FHasPrune: Boolean;
      FLiftsAny: Boolean;
      FPrefixes: TArray<string>;       // with trailing delimiters: whole folders
      FPruneMatcher: TWildCard;        // the other patterns ending in '*', against folder paths + '\'
      FRoot: string;
      FSuffixes: TArray<string>;
      function BelowRoot(const APath: string): string;
    public
      // ARoot (ending in a path delimiter) is the root the rules are for; '' gives every exclusion as it is.
      constructor Create(const ARoot: string; const APrefixes, ASuffixes, APatterns: TArray<string>);
      // True when the folder APath (ending in a path delimiter, inside Root) is excluded.
      function ExcludesDirectory(const APath: string): Boolean;
      // True when the file AFileName (a full path, inside Root) is excluded.
      function ExcludesFile(const AFileName: string): Boolean;
      // True when an exclusion covers Root, i.e. these rules differ from the scan's own.
      property LiftsAny: Boolean read FLiftsAny;
      property Root: string read FRoot;
    end;
  strict private
    FSkippedDirectories: TStringList;
    FCachedSkippedDirectoriesFileCount: Integer;
    FConvertRelativePathsToAbsolute: Boolean;
    FFollowDirectoryLinks: Boolean;
    FScanActive: Integer;              // 1 while a walk runs: one scan at a time per scanner
    FScanRoots: TArray<string>;        // the last walk's roots, after PrepareRootDirectories
    FScanCurrentDirectory: string;     // the current directory when the last walk started: what relative roots meant
    FSkippedDirectoriesCounted: Boolean;
    FExcludedPrefixes: TArray<string>; // FExclusions.PathPrefixes with trailing delimiters: whole folders (per scan)
    FExcludedSuffixes: TArray<string>; // FExclusions.PathSuffixes (per scan)
    FExcludedPatterns: TArray<string>; // FExclusions.Patterns, validated and ready to match (per scan)
    FScanRules: TExclusionRules;       // the exclusions as given (per scan)
    FRootRules: TArray<TExclusionRules>; // for the roots that lie in an excluded folder (per scan); usually empty
    FFastExtensions: TArray<string>;   // extensions (with dot, e.g. '.pas') for simple "*.ext" patterns
    FComplexPatterns: TArray<string>;  // include patterns WildCardMatcher matches against the file name
    FComplexMatcher: TWildCard;        // FComplexPatterns, compiled (per scan)
    FWorkers: TFileScanWorkers;        // owned; nil = one worker per core
    function GetFileCounts(const ASkippedDirectories: TStringList): Integer;
    function GetSkippedFilesCount: Integer;
    function IsScanRoot(const APath: string): Boolean;
    // The rules for APath: those of the innermost root with rules of its own that APath lies strictly inside, or the
    // scan's.
    function RulesFor(const APath: string): TExclusionRules;
    procedure AddSkippedDirectories(const APath: string);
    procedure FreeRules;
  strict protected
    FDiskScanTimeForFiles: Double;
    FExclusions: TFileScanExclusions;
    FExtensions: TStringList;
    FLock: TMonitor;
    FSkippedFilesCount: Integer;
    FSortResultList: Boolean;
    // Shared parallel walk behind the list results: returns every matching file, in CompareText order when
    // ASort is set - each worker sorts its own share in parallel, then the shares are merged.
    function CollectFiles(const ADirectories: TArray<string>; const AExclusions: TFileScanExclusions;
      const ASort: Boolean): TArray<string>;
    // True when the folder APath (ending in a path delimiter) is excluded by a prefix or a pruning pattern.
    function ExcludedDirectory(const APath: string): Boolean;
    // True when the file AFileName (a full path) is excluded by a suffix or a pattern.
    function ExcludedFile(const AFileName: string): Boolean;
    // Workers per walk: Workers.Count, or one per core without Workers. Workers that find nothing to do just wait
    // for a hand-off, so using every core costs little.
    function GetWorkerCount: Integer; virtual;
    // The workers' thread priority: Workers.Priority, or tpNormal without Workers.
    function GetWorkerPriority: TThreadPriority;
    function MatchesAnyExtension(const AFileName: PChar; const AFileNameLength: Integer): Boolean;
    function PrepareRootDirectories(const ADirectories: TArray<string>): TArray<string>;
    // The threading library's part: calls AWorker once for every worker index 0..AWorkerCount-1, in parallel at
    // APriority, and returns when all of them have returned; an exception in a worker must be raised here, in the
    // calling thread. Running some of them one after another is fine - the walk completes with however many
    // workers run at the same time.
    procedure ExecuteWorkers(const AWorkerCount: Integer; const AWorker: TProc<Integer>;
      const APriority: TThreadPriority); virtual; abstract;
    procedure PrepareExclusions;
    procedure PrepareExtensions;
    procedure ResetCounters;
    // Load-balanced parallel walk. AWorkerCount workers each walk a subtree depth-first on a private stack
    // and hand the shallowest pending directories (the biggest subtrees) to any worker that has run out of
    // work, so a dominant subtree is spread across all cores instead of pinning one thread.
    // AAcquireFileSink is called once per worker (argument = worker index 0..AWorkerCount-1) to obtain that
    // worker's file callback; it fires on worker threads, so a shared sink must be thread-safe. AFinishWorker
    // (optional) is called on each worker thread once the whole walk is done, with the same index, so per-worker
    // results can be post-processed in parallel. The workers run at GetWorkerPriority.
    // Raises EInvalidOpException when this scanner is already scanning.
    procedure RunParallelWalk(const ADirectories: TArray<string>; const AExclusions: TFileScanExclusions;
      const AWorkerCount: Integer; const AAcquireFileSink: TFunc<Integer, TDirectoryWalkProc>;
      const AFinishWorker: TProc<Integer>);
    // RunParallelWalk's workers, on the prepared roots.
    procedure RunWalkWorkers(const ARoots: TArray<string>; const AWorkerCount: Integer;
      const AAcquireFileSink: TFunc<Integer, TDirectoryWalkProc>; const AFinishWorker: TProc<Integer>);
    // Enumerates one directory (non-recursively) in a single pass: matching files go to AFileFound,
    // non-excluded subdirectories are appended to ASubDirectories. APath and the subdirectories it adds
    // end in a path delimiter.
    procedure ScanDirectory(const APath: string; const AFileFound: TDirectoryWalkProc; const ASubDirectories: TList<string>);
    // TStringList scan core; the Spring4D scanner fills its IList<string> from CollectFiles itself.
    procedure ScanInto(const ADirectories: TArray<string>; const AExclusions: TFileScanExclusions;
      const AResult: TStringList);
  public
    // AWorkers decides how many workers a scan uses and at what thread priority; the scanner owns and frees it, also
    // when Create fails. nil means one worker per core at normal priority. Deliberately not a default parameter, so
    // it is a choice made at every Create.
    constructor Create(const AExtensions: TArray<string>; const AWorkers: TFileScanWorkers;
      const ASortResultList: Boolean = True); overload; virtual;
    constructor Create(const AExtensions: TStringList; const AWorkers: TFileScanWorkers;
      const ASortResultList: Boolean = True); overload;
    destructor Destroy; override;

    // Adds every matching file to AFileNamesList, in CompareText order when SortResultList is set.
    function GetFileList(const ADirectories: TArray<string>; const AExclusions: TFileScanExclusions;
      const AFileNamesList: TStringList): Boolean; overload;
    function GetFileList(const ADirectories: TStringList; const AExclusions: TFileScanExclusions;
      const AFileNamesList: TStringList): Boolean; overload;
    // Streaming scan: AFileFoundCallback fires for every matching file AS IT IS FOUND, from
    // multiple worker threads concurrently - the callback MUST be thread-safe. Each file is
    // delivered once (overlapping roots are merged before the walk), in nondeterministic order;
    // SortResultList does not apply here. Available on every scanner class.
    function ScanFiles(const ADirectories: TArray<string>; const AExclusions: TFileScanExclusions;
      const AFileFoundCallback: TFileFoundCallbackProc; var AFileCount: Integer): Boolean; overload;
    function ScanFiles(const ADirectories: TArray<string>; const AExclusions: TFileScanExclusions;
      const AFileFoundCallback: TFileFoundCallback; var AFileCount: Integer): Boolean; overload;

    property DiskScanTimeForFiles: Double read FDiskScanTimeForFiles; // in milliseconds
    // The files the last scan excluded. Those inside pruned folders are counted when this is first read - one pass
    // over each pruned folder, which a scan never reads otherwise - so they are the files there at that moment.
    property SkippedFilesCount: Integer read GetSkippedFilesCount;
    property SortResultList: Boolean read FSortResultList write FSortResultList;
    // Resolve relative roots against the current directory, so the results are absolute paths. Relative path prefixes,
    // and patterns that start with a relative path, are then resolved the same way. Off: roots, prefixes and patterns
    // are used as written, so write them alike.
    property ConvertRelativePathsToAbsolute: Boolean read FConvertRelativePathsToAbsolute write FConvertRelativePathsToAbsolute;
    // Walk into directory links (junctions, directory symbolic links, volume mount points) too. Off by default: a
    // link leads to a folder found elsewhere anyway - its files would then come twice - or back up the tree, which
    // repeats it until the paths get too long.
    property FollowDirectoryLinks: Boolean read FFollowDirectoryLinks write FFollowDirectoryLinks;
    // The AWorkers given to Create (nil when none was); its settings may be changed between scans.
    property Workers: TFileScanWorkers read FWorkers;
  end;

  TParallelFileScannerClass = class of TParallelFileScannerCustom;

  // Scanner on the RTL alone: the workers run on the RTL PPL (System.Threading).
  TParallelFileScanner = class(TParallelFileScannerCustom)
  strict protected
    procedure ExecuteWorkers(const AWorkerCount: Integer; const AWorker: TProc<Integer>;
      const APriority: TThreadPriority); override;
  end;

  procedure InitArrayDataFromStrings(var AArray: TArray<string>; const AArrayData: TStrings);

implementation

uses
  Winapi.Windows, System.Diagnostics, System.Generics.Defaults, System.Math, System.Threading;

const
  // Not declared in Winapi.Windows: hint FindFirstFileEx to use a larger buffer (fewer kernel round-trips).
  FIND_FIRST_EX_LARGE_FETCH = $00000002;
  // Win32 file calls refuse a path of MAX_PATH characters or more (a folder: MAX_PATH - 12) unless it has the \\?\
  // prefix, or the process opted in to long paths - its manifest and the computer's registry both. A folder path this
  // long or longer gets the prefix.
  LONG_PATH_LENGTH = MAX_PATH - 12;
  // TThreadPriority as SetThreadPriority values - the same mapping TThread.Priority uses.
  THREAD_PRIORITIES: array[TThreadPriority] of Integer = (THREAD_PRIORITY_IDLE, THREAD_PRIORITY_LOWEST,
    THREAD_PRIORITY_BELOW_NORMAL, THREAD_PRIORITY_NORMAL, THREAD_PRIORITY_ABOVE_NORMAL, THREAD_PRIORITY_HIGHEST,
    THREAD_PRIORITY_TIME_CRITICAL);

// True for a path Windows does not resolve against a current directory: 'C:\...', or '\\...' (UNC, \\?\, \\.\).
function IsFullyQualifiedPath(const APath: string): Boolean;
begin
  Result := ((Length(APath) >= 3) and (APath[2] = ':') and (APath[3] = '\'))
    or ((Length(APath) >= 2) and (APath[1] = '\') and (APath[2] = '\'));
end;

// GetFullPathName: APath resolved against the current directory, with '.' and '..' resolved - of any length, unlike
// TPath.GetFullPath before Windows 10 1607, and without raising. APath itself if it fails.
function FullPathName(const APath: string): string;
var
  LFilePart: PChar;
  LLength: Cardinal;
begin
  SetLength(Result, MAX_PATH);
  LLength := GetFullPathName(PChar(APath), Length(Result), PChar(Result), LFilePart);

  if LLength > Cardinal(Length(Result)) then
  begin
    SetLength(Result, LLength);
    LLength := GetFullPathName(PChar(APath), LLength, PChar(Result), LFilePart);
  end;

  if (LLength = 0) or (LLength > Cardinal(Length(Result))) then
    Exit(APath);

  SetLength(Result, LLength);
end;

// APath as the Win32 file calls take it, whatever its length: a relative path resolved against the current directory
// and, when long, with the \\?\ prefix. The prefix also turns off Windows' own path processing, so '.' and '..' are
// resolved here first. A short absolute path is returned as it is, without allocating.
function Win32Path(const APath: string): string;
begin
  if IsFullyQualifiedPath(APath) then
    Result := APath
  else
    Result := FullPathName(APath);

  if (Length(Result) < LONG_PATH_LENGTH) or Result.StartsWith('\\?\') or Result.StartsWith('\\.\') then
    Exit;

  if (Pos('\.\', Result) > 0) or (Pos('\..\', Result) > 0) then
    Result := FullPathName(Result);

  if Result.StartsWith('\\') then
    Result := '\\?\UNC\' + Result.Substring(2)
  else
    Result := '\\?\' + Result;
end;

// True when APath is an existing folder (or a link to one), whatever its length.
function DirectoryExistsAnyLength(const APath: string): Boolean;
var
  LAttributes: Cardinal;
begin
  if Trim(APath) = '' then
    Exit(False);

  LAttributes := GetFileAttributes(PChar(Win32Path(IncludeTrailingPathDelimiter(APath))));
  Result := (LAttributes <> INVALID_FILE_ATTRIBUTES) and (LAttributes and FILE_ATTRIBUTE_DIRECTORY <> 0);
end;

// Opens ADirectory (which ends in a path delimiter) for enumeration via FindFirstFileEx. FindExInfoBasic
// skips 8.3 short-name retrieval and FIND_FIRST_EX_LARGE_FETCH fetches in larger batches - both markedly
// faster than System.SysUtils.FindFirst (which wraps FindFirstFile with short-name generation and small
// buffers) on directory-heavy scans. Paths of any length (Win32Path). Returns INVALID_HANDLE_VALUE if the directory
// cannot be opened (AFindData is then undefined); otherwise iterate with FindNextFile and close with
// Winapi.Windows.FindClose.
function OpenDirectoryEnumeration(const ADirectory: string; var AFindData: TWin32FindData): THandle;
begin
  Result := FindFirstFileEx(PChar(Win32Path(ADirectory) + '*'), FindExInfoBasic, @AFindData, FindExSearchNameMatch,
    nil, FIND_FIRST_EX_LARGE_FETCH);
end;

// True for the '.' and '..' entries every directory enumeration returns.
function IsDotDirectory(const AName: PChar): Boolean; inline;
begin
  Result := (AName[0] = '.') and ((AName[1] = #0) or ((AName[1] = '.') and (AName[2] = #0)));
end;

// True for a directory entry the walk does not enter: '.', '..' and - unless AFollowLinks - a directory link. A link
// is a name-surrogate reparse point (junction, directory symbolic link, volume mount point), whose tag the find data
// holds in dwReserved0; other reparse points, such as cloud-file placeholders (OneDrive), are ordinary folders.
function SkipDirectoryEntry(const AFindData: TWin32FindData; const AFollowLinks: Boolean): Boolean; inline;
const
  REPARSE_TAG_NAME_SURROGATE = $20000000; // IsReparseTagNameSurrogate in winnt.h
begin
  Result := IsDotDirectory(@AFindData.cFileName[0])
    or (not AFollowLinks and (AFindData.dwFileAttributes and FILE_ATTRIBUTE_REPARSE_POINT <> 0)
      and (AFindData.dwReserved0 and REPARSE_TAG_NAME_SURROGATE <> 0));
end;

// Paths, prefixes and patterns are compared with the '\' paths the walk builds, so a '/' in them must become '\'.
function NormalizeDelimiters(const APath: string): string; inline;
begin
  Result := APath.Replace('/', '\');
end;

// Ordinal, case-insensitive comparison of ALength characters - the way Windows compares file names. The RTL's
// EndsWith/StartsWith(IgnoreCase) are locale-aware Win32 calls instead (slow per directory entry, and wrong for
// e.g. "I"/"i" under a Turkish locale). ASCII is folded inline; a difference involving any other character is
// settled by CompareStringOrdinal.
function SameTextOrdinal(const AChars1, AChars2: PChar; const ALength: Integer): Boolean;
var
  LChar1: Char;
  LChar2: Char;
  LLower: Integer;
begin
  for var LIndex := 0 to ALength - 1 do
  begin
    LChar1 := AChars1[LIndex];
    LChar2 := AChars2[LIndex];

    if LChar1 = LChar2 then
      Continue;

    if (Ord(LChar1) > $7F) or (Ord(LChar2) > $7F) then
      Exit(CompareStringOrdinal(@AChars1[LIndex], ALength - LIndex, @AChars2[LIndex], ALength - LIndex, 1) = CSTR_EQUAL);

    // Two different ASCII characters are equal ignoring case only when they are the same letter.
    LLower := Ord(LChar1) or $20;

    if (LLower <> (Ord(LChar2) or $20)) or (LLower < Ord('a')) or (LLower > Ord('z')) then
      Exit(False);
  end;

  Result := True;
end;

function StartsTextOrdinal(const APrefix, AText: string): Boolean;
begin
  Result := (Length(AText) >= Length(APrefix)) and SameTextOrdinal(PChar(AText), PChar(APrefix), Length(APrefix));
end;

function EndsTextOrdinal(const ASuffix, AText: string): Boolean;
var
  LOffset: Integer;
begin
  LOffset := Length(AText) - Length(ASuffix);
  Result := (LOffset >= 0) and SameTextOrdinal(PChar(AText) + LOffset, PChar(ASuffix), Length(ASuffix));
end;

// APath (which ends in a path delimiter) + ANameLength characters of AName, optionally followed by a path
// delimiter, in a single allocation.
function JoinPath(const APath: string; const AName: PChar; const ANameLength: Integer; const AAddDelimiter: Boolean): string;
var
  LPathLength: Integer;
begin
  LPathLength := Length(APath);
  SetLength(Result, LPathLength + ANameLength + Ord(AAddDelimiter));
  Move(PChar(APath)^, PChar(Result)^, LPathLength * SizeOf(Char));
  Move(AName^, PChar(Result)[LPathLength], ANameLength * SizeOf(Char));

  if AAddDelimiter then
    PChar(Result)[LPathLength + ANameLength] := PathDelim;
end;

type
  // Shared state of one load-balanced walk. Each worker walks its own subtree depth-first on a private stack
  // (no locking per directory); directories only pass through this pool when a worker has run out of work
  // and another hands some over, so the lock is taken a few times per subtree rather than per directory.
  // The walk is complete when nothing is queued and no worker still owns a subtree.
  TDirectoryWorkPool = class(TObject)
  strict private
    FAborted: Boolean;
    FActiveWorkers: Integer; // workers currently owning a subtree; their private stacks are pending work
    FIdleWorkers: Integer;   // workers waiting in TakeWork; read unlocked by busy workers as a "hungry" hint
    FQueue: TList<string>;
  public
    constructor Create(const ARoots: TArray<string>);
    destructor Destroy; override;
    function TakeWork(var ADirectory: string): Boolean;
    procedure Abort;
    procedure FinishSubtree;
    procedure ShareWork(const ALocalStack: TList<string>);
    property Aborted: Boolean read FAborted;
  end;

constructor TDirectoryWorkPool.Create(const ARoots: TArray<string>);
begin
  inherited Create;

  FQueue := TList<string>.Create;
  FQueue.AddRange(ARoots);
end;

destructor TDirectoryWorkPool.Destroy;
begin
  FQueue.Free;

  inherited Destroy;
end;

// Blocks until a subtree is available (Result True, the caller now owns it and must call FinishSubtree when
// its private stack runs dry) or the walk is complete / aborted (Result False).
function TDirectoryWorkPool.TakeWork(var ADirectory: string): Boolean;
begin
  Result := False;
  ADirectory := '';

  TMonitor.Enter(Self);
  try
    while not FAborted do
    begin
      if FQueue.Count > 0 then
      begin
        ADirectory := FQueue.Last;
        FQueue.Delete(FQueue.Count - 1);
        Inc(FActiveWorkers);

        Exit(True);
      end;

      // Nothing queued and nobody left who could hand over more: the whole tree has been walked.
      if FActiveWorkers = 0 then
        Exit;

      Inc(FIdleWorkers);
      try
        TMonitor.Wait(Self, INFINITE);
      finally
        Dec(FIdleWorkers);
      end;
    end;
  finally
    TMonitor.Exit(Self);
  end;
end;

procedure TDirectoryWorkPool.Abort;
begin
  TMonitor.Enter(Self);
  try
    FAborted := True;

    TMonitor.PulseAll(Self);
  finally
    TMonitor.Exit(Self);
  end;
end;

procedure TDirectoryWorkPool.FinishSubtree;
begin
  TMonitor.Enter(Self);
  try
    Dec(FActiveWorkers);

    // The last active worker just ran dry with nothing queued: release every waiter so it can finish.
    if (FActiveWorkers = 0) and (FQueue.Count = 0) then
      TMonitor.PulseAll(Self);
  finally
    TMonitor.Exit(Self);
  end;
end;

// Called after every directory a worker scans, so the common case (nobody idle) must not lock.
procedure TDirectoryWorkPool.ShareWork(const ALocalStack: TList<string>);
var
  LCount: Integer;
begin
  if (FIdleWorkers = 0) or (ALocalStack.Count < 2) then
    Exit;

  TMonitor.Enter(Self);
  try
    // Hand over from the bottom of the stack: the shallowest pending directories, i.e. the biggest
    // subtrees. Items already queued but not yet taken count against the idle workers they will feed.
    LCount := Min(FIdleWorkers - FQueue.Count, ALocalStack.Count - 1);

    if LCount <= 0 then
      Exit;

    for var LIndex := 0 to LCount - 1 do
      FQueue.Add(ALocalStack[LIndex]);

    ALocalStack.DeleteRange(0, LCount);

    for var LIndex := 1 to LCount do
      TMonitor.Pulse(Self);
  finally
    TMonitor.Exit(Self);
  end;
end;

procedure InitArrayDataFromStrings(var AArray: TArray<string>; const AArrayData: TStrings);
var
  LIndex: Integer;
begin
  AArray := [];
  SetLength(AArray, AArrayData.Count);

  for LIndex := 0 to AArrayData.Count - 1 do
    AArray[LIndex] := AArrayData[LIndex];
end;

{ TFileScanExclusions }

function TFileScanExclusions.GetPathPrefixesString: string;
begin
  Result := Result.Join(';', FPathPrefixes);
end;

function TFileScanExclusions.GetPathSuffixesString: string;
begin
  Result := Result.Join(';', FPathSuffixes);
end;

function TFileScanExclusions.GetPatternsString: string;
begin
  Result := Result.Join(';', FPatterns);
end;

procedure TFileScanExclusions.InitArrayFromStrings(const AKind: TExclusionKind; const AArrayData: TStrings);
begin
  case AKind of
    ekPathPrefixes: InitArrayDataFromStrings(FPathPrefixes, AArrayData);
    ekPathSuffixes: InitArrayDataFromStrings(FPathSuffixes, AArrayData);
    ekPatterns: InitArrayDataFromStrings(FPatterns, AArrayData);
  end;
end;

{ TFileScanWorkers }

constructor TFileScanWorkers.Create;
begin
  inherited Create;

  FMinCount := 1;
  FMaxCount := CoreCount;
  FCount := FMaxCount;
  FPriority := TThreadPriority.tpNormal;
end;

class function TFileScanWorkers.CoreCount: Integer;
begin
  Result := Max(1, TThread.ProcessorCount);
end;

procedure TFileScanWorkers.Initialize(const ADesiredCount: Integer; const AMaxCount: Integer = -1;
  const AMinCount: Integer = -1);
var
  LMaxCount: Integer;
  LMinCount: Integer;
begin
  if (AMaxCount < 1) and (AMaxCount <> -1) then
    raise EArgumentOutOfRangeException.CreateFmt('Maximum worker count %d: must be at least 1, or -1 for the default',
      [AMaxCount]);

  if (AMinCount < 1) and (AMinCount <> -1) then
    raise EArgumentOutOfRangeException.CreateFmt('Minimum worker count %d: must be at least 1, or -1 for the default',
      [AMinCount]);

  if (AMaxCount <> -1) and (AMinCount > AMaxCount) then
    raise EArgumentException.CreateFmt('Minimum worker count %d is above the maximum %d', [AMinCount, AMaxCount]);

  if AMaxCount = -1 then
    LMaxCount := CoreCount
  else
    LMaxCount := AMaxCount;

  if AMinCount = -1 then
    LMinCount := 1
  else
    LMinCount := AMinCount;

  // An explicit limit beats a default one - the only way the minimum can be above the maximum here is a default
  // maximum (CoreCount) below an explicit minimum.
  FMinCount := LMinCount;
  FMaxCount := Max(LMaxCount, LMinCount);
  SetCount(ADesiredCount);
end;

procedure TFileScanWorkers.InitializePercentage(const APercentage: Double; const AMaxCount: Integer = -1;
  const AMinCount: Integer = -1);
begin
  if IsNan(APercentage) or (APercentage <= 0) then
    raise EArgumentOutOfRangeException.CreateFmt('Worker percentage %g: must be above 0', [APercentage]);

  // Capped before rounding, so a huge percentage cannot overflow.
  Initialize(Integer(Round(Min(CoreCount * APercentage / 100, MaxInt))), AMaxCount, AMinCount);
end;

procedure TFileScanWorkers.SetCount(const AValue: Integer);
begin
  FCount := EnsureRange(AValue, FMinCount, FMaxCount);
end;

procedure TFileScanWorkers.SetMaxCount(const AValue: Integer);
begin
  if AValue < 1 then
    raise EArgumentOutOfRangeException.CreateFmt('Maximum worker count %d: must be at least 1', [AValue]);

  FMaxCount := AValue;
  FMinCount := Min(FMinCount, AValue);
  SetCount(FCount);
end;

procedure TFileScanWorkers.SetMinCount(const AValue: Integer);
begin
  if AValue < 1 then
    raise EArgumentOutOfRangeException.CreateFmt('Minimum worker count %d: must be at least 1', [AValue]);

  FMinCount := AValue;
  FMaxCount := Max(FMaxCount, AValue);
  SetCount(FCount);
end;

{ TParallelFileScannerCustom.TExclusionRules }

constructor TParallelFileScannerCustom.TExclusionRules.Create(const ARoot: string; const APrefixes, ASuffixes,
  APatterns: TArray<string>);
var
  LBelow: TArray<string>;
  LBelowPrune: TArray<string>;
  LOther: TArray<string>;
  LOtherPrune: TArray<string>;
begin
  inherited Create;

  FRoot := ARoot;
  FSuffixes := ASuffixes;
  FPrefixes := [];

  // A prefix the root lies under names a folder above the root (or the root itself): nothing below the root can be
  // told apart by it, so inside the root it is simply dropped.
  for var LIndex := 0 to High(APrefixes) do
    if (ARoot <> '') and StartsTextOrdinal(APrefixes[LIndex], ARoot) then
      FLiftsAny := True
    else
      FPrefixes := FPrefixes + [APrefixes[LIndex]];

  LBelow := [];
  LBelowPrune := [];
  LOther := [];
  LOtherPrune := [];

  // A pattern ending in '*' that matches "Folder\" matches every path under it as well, so it may prune the walk at
  // that folder. Any other pattern only says something about files. A pattern covers the root when it matches the
  // root folder itself.

  for var LIndex := 0 to High(APatterns) do
  begin
    var LPattern := APatterns[LIndex];

    if (ARoot <> '') and TWildCard.Create(LPattern).Match(ARoot) then
    begin
      FLiftsAny := True;
      LBelow := LBelow + [LPattern];

      if LPattern.EndsWith('*') then
        LBelowPrune := LBelowPrune + [LPattern];
    end
    else
    begin
      LOther := LOther + [LPattern];

      if LPattern.EndsWith('*') then
        LOtherPrune := LOtherPrune + [LPattern];
    end;
  end;

  FExcludeMatcher := TWildCard.Create(LOther);
  FHasExclude := Length(LOther) > 0;
  FPruneMatcher := TWildCard.Create(LOtherPrune);
  FHasPrune := Length(LOtherPrune) > 0;
  FBelowExcludeMatcher := TWildCard.Create(LBelow);
  FHasBelowExclude := Length(LBelow) > 0;
  FBelowPruneMatcher := TWildCard.Create(LBelowPrune);
  FHasBelowPrune := Length(LBelowPrune) > 0;
end;

// The part of APath below Root, with the delimiter it starts with: 'C:\Fork\Lib\3rdParty\' in root 'C:\Fork\' is
// '\Lib\3rdParty\', which '*\3rdParty\*' matches.
function TParallelFileScannerCustom.TExclusionRules.BelowRoot(const APath: string): string;
begin
  Result := APath.Substring(Length(FRoot) - 1);
end;

// Called once per subdirectory from every worker thread, on the path the walk builds anyway (ending in a path
// delimiter) - no allocation, except below a root with rules of its own. The prefix array is indexed rather than walked
// with for..in, which would copy each shared string and bump its reference count from all threads at once.
function TParallelFileScannerCustom.TExclusionRules.ExcludesDirectory(const APath: string): Boolean;
begin
  for var LIndex := 0 to High(FPrefixes) do
    if StartsTextOrdinal(FPrefixes[LIndex], APath) then
      Exit(True);

  if FHasPrune and FPruneMatcher.Match(APath) then
    Exit(True);

  Result := FHasBelowPrune and FBelowPruneMatcher.Match(BelowRoot(APath));
end;

// Called for every file that passed the search patterns, on its full path, which is built for it anyway.
function TParallelFileScannerCustom.TExclusionRules.ExcludesFile(const AFileName: string): Boolean;
begin
  for var LIndex := 0 to High(FSuffixes) do
    if EndsTextOrdinal(FSuffixes[LIndex], AFileName) then
      Exit(True);

  if FHasExclude and FExcludeMatcher.Match(AFileName) then
    Exit(True);

  Result := FHasBelowExclude and FBelowExcludeMatcher.Match(BelowRoot(AFileName));
end;

{ TParallelFileScannerCustom }

// Every walk skips its own folders - no root's walk enters another's, and none enters a folder it skipped - so no
// folder is listed twice, and none inside another from the same walk. One walk's skipped folder can hold another root,
// though, with skipped folders of its own: GetFileCounts leaves every root to its own walk's entries.
procedure TParallelFileScannerCustom.AddSkippedDirectories(const APath: string);
begin
  // Called from ScanDirectory, which runs on multiple worker threads, so
  // access to the shared FSkippedDirectories list must be serialized.
  FLock.Enter(FSkippedDirectories);
  try
    FSkippedDirectories.Add(APath);
  finally
    FLock.Exit(FSkippedDirectories);
  end;
end;

procedure TParallelFileScannerCustom.FreeRules;
begin
  for var LRules in FRootRules do
    LRules.Free;

  FRootRules := [];
  FreeAndNil(FScanRules);
end;

function TParallelFileScannerCustom.RulesFor(const APath: string): TExclusionRules;
var
  LLength: Integer;
begin
  Result := FScanRules;
  LLength := 0;

  // Strictly inside: a root folder itself is judged by the walk around it, which must skip it - or that walk would
  // enter the root too, and its files would come twice.
  for var LIndex := 0 to High(FRootRules) do
    if (Length(FRootRules[LIndex].Root) > LLength) and (Length(FRootRules[LIndex].Root) < Length(APath))
      and StartsTextOrdinal(FRootRules[LIndex].Root, APath) then
    begin
      Result := FRootRules[LIndex];
      LLength := Length(Result.Root);
    end;
end;

// Also builds the rules of the roots that lie in an excluded folder (FRootRules), which the walk then uses inside them.
function TParallelFileScannerCustom.PrepareRootDirectories(const ADirectories: TArray<string>): TArray<string>;

  // The innermost kept root that holds ARoot (or is the same folder): '' when none does.
  function KeptAncestor(const ARoots: TList<string>; const AKept: TArray<Boolean>; const ARoot: string): string;
  begin
    Result := '';

    for var LIndex := 0 to ARoots.Count - 1 do
      if AKept[LIndex] and (Length(ARoots[LIndex]) > Length(Result))
        and StartsTextOrdinal(ARoots[LIndex], ARoot) then
        Result := ARoots[LIndex];
  end;

var
  LKept: TArray<Boolean>;
  LOrder: TArray<Integer>;
  LRoots: TList<string>;
begin
  LRoots := TList<string>.Create;
  try
    for var LRoot in ADirectories do
    begin
      var LRootPath := NormalizeDelimiters(LRoot);

      if not DirectoryExistsAnyLength(LRootPath) then
        Continue;

      // Resolve the root to an absolute path once, so every file produced from it is already
      // absolute and no per-file conversion pass is needed afterwards. The walk keeps directories with a
      // trailing delimiter, so entry names can be appended directly.
      if FConvertRelativePathsToAbsolute then
        LRoots.Add(IncludeTrailingPathDelimiter(TPath.GetFullPath(LRootPath)))
      else
        LRoots.Add(IncludeTrailingPathDelimiter(LRootPath));
    end;

    // Drop every root another root's walk reaches anyway, so no file is produced twice and the results need no
    // de-duplication pass: the same folder given again (the first spelling is kept), or one inside a kept root that
    // its walk does not skip. One inside an excluded folder stays a root of its own - it was asked for - and gets
    // rules of its own. Decided outermost first, so each root is judged by the walk that would actually reach it: the
    // innermost kept root around it, with the rules that walk uses there. Checking the root alone is enough - a
    // folder on the way excluded means the root is excluded too, as prefixes and pruning patterns take in everything
    // under what they match. The roots keep the caller's order.
    SetLength(LOrder, LRoots.Count);
    SetLength(LKept, LRoots.Count);

    for var LIndex := 0 to High(LOrder) do
      LOrder[LIndex] := LIndex;

    TArray.Sort<Integer>(LOrder, TComparer<Integer>.Construct(
      function(const ALeft, ARight: Integer): Integer
      begin
        Result := Length(LRoots[ALeft]) - Length(LRoots[ARight]);

        if Result = 0 then
          Result := ALeft - ARight;
      end));

    for var LIndex in LOrder do
    begin
      var LRoot := LRoots[LIndex];
      var LAncestor := KeptAncestor(LRoots, LKept, LRoot);

      if LAncestor <> '' then
      begin
        if Length(LAncestor) = Length(LRoot) then
          Continue;

        if not RulesFor(LRoot).ExcludesDirectory(LRoot) then
          Continue;
      end;

      LKept[LIndex] := True;

      var LRules := TExclusionRules.Create(LRoot, FExcludedPrefixes, FExcludedSuffixes, FExcludedPatterns);

      if LRules.LiftsAny then
        FRootRules := FRootRules + [LRules]
      else
        LRules.Free;
    end;

    SetLength(Result, 0);

    for var LIndex := 0 to LRoots.Count - 1 do
      if LKept[LIndex] then
        Result := Result + [LRoots[LIndex]];
  finally
    LRoots.Free;
  end;
end;

procedure TParallelFileScannerCustom.PrepareExclusions;

  // The entries that are not blank, with '/' read as '\'.
  function NonBlank(const AValues: TArray<string>): TArray<string>;
  begin
    Result := [];

    for var LIndex := 0 to High(AValues) do
      if Trim(AValues[LIndex]) <> '' then
        Result := Result + [NormalizeDelimiters(AValues[LIndex])];
  end;

  // With ConvertRelativePathsToAbsolute the roots - and so every path the walk builds - are absolute, so a relative
  // prefix is made absolute the same way, or it could never match. Without it, it is compared as written.
  function AbsolutePrefix(const APrefix: string): string;
  begin
    if FConvertRelativePathsToAbsolute and not IsFullyQualifiedPath(APrefix) then
      Result := FullPathName(APrefix)
    else
      Result := APrefix;
  end;

  // The same for a pattern that starts with a relative path: its folder part - up to the last delimiter before the
  // first wildcard - is made absolute. A pattern that starts with a wildcard ('*\.git\*') matches anywhere anyway.
  // The current directory holds no wildcard characters: Windows forbids them all in names.
  function AbsolutePattern(const APattern: string): string;
  var
    LDelimiter: Integer;
    LWildcard: Integer;
  begin
    if not FConvertRelativePathsToAbsolute or IsFullyQualifiedPath(APattern) then
      Exit(APattern);

    LWildcard := APattern.IndexOfAny(['*', '?', '<']);

    if LWildcard = 0 then
      Exit(APattern);

    if LWildcard < 0 then
      Exit(FullPathName(APattern));

    LDelimiter := APattern.LastIndexOf('\', LWildcard - 1);

    if LDelimiter < 0 then
      Result := IncludeTrailingPathDelimiter(GetCurrentDir) + APattern
    else
      Result := IncludeTrailingPathDelimiter(FullPathName(APattern.Substring(0, LDelimiter + 1)))
        + APattern.Substring(LDelimiter + 1);
  end;

var
  LErrorMessage: string;
  LPatterns: TArray<string>;
begin
  // Normalized once per scan, so the per-directory checks neither allocate nor re-normalize. Blank entries are
  // dropped: an empty prefix or suffix would otherwise match - and exclude - everything. '/' becomes '\', or the entry
  // could never match the walk's paths. Prefixes get a trailing delimiter, so they match whole folder names only,
  // like the folder paths they are compared with.
  FExcludedPrefixes := NonBlank(FExclusions.PathPrefixes);

  for var LIndex := 0 to High(FExcludedPrefixes) do
    FExcludedPrefixes[LIndex] := IncludeTrailingPathDelimiter(AbsolutePrefix(FExcludedPrefixes[LIndex]));

  FExcludedSuffixes := NonBlank(FExclusions.PathSuffixes);

  LPatterns := NonBlank(FExclusions.Patterns);

  for var LIndex := 0 to High(LPatterns) do
  begin
    // A pattern ending in a path delimiter names folders, as in .gitignore: 'C:\Code\3rdParty\' is read as
    // 'C:\Code\3rdParty\*'. Taken literally it could never match a file path, and would exclude nothing.
    if LPatterns[LIndex].EndsWith('\') then
      LPatterns[LIndex] := LPatterns[LIndex] + '*';

    // A malformed pattern would silently match nothing - and so exclude nothing.
    if not TWildCard.ValidatePattern(LPatterns[LIndex], LErrorMessage) then
      raise EInOutArgumentException.CreateFmt('Exclusion pattern ''%s'': %s', [LPatterns[LIndex], LErrorMessage]);

    LPatterns[LIndex] := AbsolutePattern(LPatterns[LIndex]);
  end;

  FExcludedPatterns := LPatterns;
  FScanRules := TExclusionRules.Create('', FExcludedPrefixes, FExcludedSuffixes, FExcludedPatterns);
end;

procedure TParallelFileScannerCustom.PrepareExtensions;
var
  LComplex: TList<string>;
  LErrorMessage: string;
  LFast: TList<string>;
begin
  LFast := TList<string>.Create;
  LComplex := TList<string>.Create;
  try
    for var LIndex := 0 to FExtensions.Count - 1 do
    begin
      var LPattern := FExtensions[LIndex];

      if Trim(LPattern) = '' then
        raise EInOutArgumentException.Create('Empty search pattern')
      else if LPattern.IndexOfAny(['\', '/']) >= 0 then
        raise EInOutArgumentException.CreateFmt('Search pattern ''%s'': search patterns match file names, so they '
          + 'cannot contain a path delimiter - exclude by path with TFileScanExclusions', [LPattern])
      else if not TWildCard.ValidatePattern(LPattern, LErrorMessage) then
        raise EInOutArgumentException.CreateFmt('Search pattern ''%s'': %s', [LPattern, LErrorMessage]);

      // A plain "*.ext" pattern (no further wildcards) can be matched with a fast, allocation-free
      // extension compare instead of the wildcard matcher, which would need the name as a string.
      if LPattern.StartsWith('*.') then
      begin
        var LExtension := LPattern.Substring(1); // ".ext" from "*.ext"

        if (Length(LExtension) > 1) and not TWildCard.IsWildCard(LExtension) then
          LFast.Add(LExtension)
        else
          LComplex.Add(LPattern);
      end
      else
        LComplex.Add(LPattern);
    end;

    FFastExtensions := LFast.ToArray;
    FComplexPatterns := LComplex.ToArray;
    FComplexMatcher := TWildCard.Create(FComplexPatterns);
  finally
    LComplex.Free;
    LFast.Free;
  end;
end;

constructor TParallelFileScannerCustom.Create(const AExtensions: TArray<string>; const AWorkers: TFileScanWorkers;
  const ASortResultList: Boolean = True);
begin
  // Owned from the first statement on, so the destructor frees it even if the rest of Create raises.
  FWorkers := AWorkers;

  inherited Create;

  FCachedSkippedDirectoriesFileCount := 0;
  FSkippedDirectories := TStringList.Create;
  FSkippedDirectories.Sorted := True;
  FSortResultList := ASortResultList;
  FExtensions := TStringList.Create;
  FExtensions.AddStrings(AExtensions);
end;

destructor TParallelFileScannerCustom.Destroy;
begin
  FreeRules;
  FSkippedDirectories.Free;
  FExtensions.Free;
  FWorkers.Free;

  inherited Destroy;
end;

procedure TParallelFileScannerCustom.ResetCounters;
begin
  FSkippedFilesCount := 0;
  FSkippedDirectories.Clear;
  FCachedSkippedDirectoriesFileCount := 0;
  FSkippedDirectoriesCounted := False;
  FScanRoots := [];
end;

// Called once per subdirectory from every worker thread, on the path the walk builds anyway.
function TParallelFileScannerCustom.ExcludedDirectory(const APath: string): Boolean;
begin
  Result := RulesFor(APath).ExcludesDirectory(APath);
end;

// Called for every file that passed the search patterns, on its full path, which is built for it anyway.
function TParallelFileScannerCustom.ExcludedFile(const AFileName: string): Boolean;
begin
  Result := RulesFor(AFileName).ExcludesFile(AFileName);
end;

// Called for every file entry of every directory, on the raw name in the find-data buffer (no string yet).
function TParallelFileScannerCustom.MatchesAnyExtension(const AFileName: PChar; const AFileNameLength: Integer): Boolean;
begin
  // Fast path: a plain "*.ext" pattern is an ordinal case-insensitive suffix test on the name buffer, no
  // allocation. Indexed rather than for..in for the same reason as ExcludedDirectory.
  for var LIndex := 0 to High(FFastExtensions) do
  begin
    var LLength := Length(FFastExtensions[LIndex]);

    if (AFileNameLength >= LLength)
      and SameTextOrdinal(AFileName + (AFileNameLength - LLength), PChar(FFastExtensions[LIndex]), LLength) then
      Exit(True);
  end;

  if Length(FComplexPatterns) > 0 then
  begin
    var LFileName: string;

    SetString(LFileName, AFileName, AFileNameLength);

    if FComplexMatcher.Match(LFileName) then
      Exit(True);
  end;

  Result := False;
end;

// True when APath (ending in a path delimiter) is one of the last walk's roots.
function TParallelFileScannerCustom.IsScanRoot(const APath: string): Boolean;
begin
  for var LIndex := 0 to High(FScanRoots) do
    if (Length(FScanRoots[LIndex]) = Length(APath)) and StartsTextOrdinal(FScanRoots[LIndex], APath) then
      Exit(True);

  Result := False;
end;

// Counts the files the search patterns match in the skipped folders - each file once, by the scan's own matching,
// in one pass over each folder and with the walk's rules for links. A root that lies inside a skipped folder is left
// out, all of it: its own walk has returned its files, counted the excluded ones and listed its own skipped folders.
// Runs when
// SkippedFilesCount is first read, so a relative folder is resolved against the current directory the scan had, not
// today's.
function TParallelFileScannerCustom.GetFileCounts(const ASkippedDirectories: TStringList): Integer;

  // ADirectory as the scan meant it. Only a path relative to the current directory is resolved: one relative to a
  // drive's root ('\Code\') or to a drive's own current directory ('D:Code\') is left as it is.
  function ScanTimePath(const ADirectory: string): string;
  begin
    if (FScanCurrentDirectory = '') or IsFullyQualifiedPath(ADirectory) or ADirectory.StartsWith('\')
      or ((Length(ADirectory) >= 2) and (ADirectory[2] = ':')) then
      Result := ADirectory
    else
      Result := IncludeTrailingPathDelimiter(FScanCurrentDirectory) + ADirectory;
  end;

var
  LDirectories: TList<string>;
  LFindData: TWin32FindData;
  LFindHandle: THandle;
  LName: PChar;
begin
  Result := 0;

  LDirectories := TList<string>.Create;
  try
    LDirectories.AddRange(ASkippedDirectories.ToStringArray);

    while LDirectories.Count > 0 do
    begin
      var LDirectory := LDirectories.Last;
      LDirectories.Delete(LDirectories.Count - 1);

      if IsScanRoot(LDirectory) then
        Continue;

      LFindHandle := OpenDirectoryEnumeration(ScanTimePath(LDirectory), LFindData);
      if LFindHandle <> INVALID_HANDLE_VALUE then
      try
        repeat
          LName := @LFindData.cFileName[0];

          if LFindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY <> 0 then
          begin
            if not SkipDirectoryEntry(LFindData, FFollowDirectoryLinks) then
              LDirectories.Add(JoinPath(LDirectory, LName, StrLen(LName), True));
          end
          else if MatchesAnyExtension(LName, StrLen(LName)) then
            Inc(Result);
        until not FindNextFile(LFindHandle, LFindData);
      finally
        Winapi.Windows.FindClose(LFindHandle);
      end;
    end;
  finally
    LDirectories.Free;
  end;
end;

function ComparePathsCI(AList: TStringList; AIndex1, AIndex2: Integer): Integer;
begin
  // Case-insensitive ordinal compare (CompareText) instead of the locale-aware AnsiCompareText
  // that TStringList.Sort uses by default - the latter dominates the scan time on large results.
  Result := CompareText(AList[AIndex1], AList[AIndex2]);
end;

type
  // CompareText order - ordinal, ASCII case-insensitive - the order SortResultList has always produced.
  // (TIStringComparer.Ordinal would allocate two lower-cased copies per comparison.)
  TPathComparer = class(TComparer<string>)
  public
    function Compare(const ALeft, ARight: string): Integer; override;
  end;

function TPathComparer.Compare(const ALeft, ARight: string): Integer;
begin
  Result := CompareText(ALeft, ARight);
end;

function ConcatenateLists(const ALists: TArray<TList<string>>): TArray<string>;
var
  LCount: Integer;
begin
  LCount := 0;

  for var LList in ALists do
    Inc(LCount, LList.Count);

  SetLength(Result, LCount);
  LCount := 0;

  for var LList in ALists do
  begin
    TArray.Copy<string>(LList.List, Result, 0, LCount, LList.Count);
    Inc(LCount, LList.Count);
  end;
end;

// k-way merge of lists that are each already sorted by AComparer, through a binary min-heap of list indexes
// keyed by each list's next item: about log2(k) comparisons per item instead of a full re-sort.
function MergeSortedLists(const ALists: TArray<TList<string>>; const AComparer: IComparer<string>): TArray<string>;
var
  LCounts: TArray<Integer>;
  LHeap: TArray<Integer>;
  LHeapSize: Integer;
  LItems: TArray<TArray<string>>;
  LPositions: TArray<Integer>;
  LTotal: Integer;

  function HeadIsLess(const AList1, AList2: Integer): Boolean;
  begin
    Result := AComparer.Compare(LItems[AList1][LPositions[AList1]], LItems[AList2][LPositions[AList2]]) < 0;
  end;

  procedure SiftDown(AHeapIndex: Integer);
  var
    LChild: Integer;
    LList: Integer;
  begin
    LList := LHeap[AHeapIndex];

    while True do
    begin
      LChild := 2 * AHeapIndex + 1;

      if LChild >= LHeapSize then
        Break;

      if (LChild + 1 < LHeapSize) and HeadIsLess(LHeap[LChild + 1], LHeap[LChild]) then
        Inc(LChild);

      if not HeadIsLess(LHeap[LChild], LList) then
        Break;

      LHeap[AHeapIndex] := LHeap[LChild];
      AHeapIndex := LChild;
    end;

    LHeap[AHeapIndex] := LList;
  end;

begin
  SetLength(LCounts, Length(ALists));
  SetLength(LHeap, Length(ALists));
  SetLength(LItems, Length(ALists));
  SetLength(LPositions, Length(ALists));
  LHeapSize := 0;
  LTotal := 0;

  for var LIndex := 0 to High(ALists) do
  begin
    LItems[LIndex] := ALists[LIndex].List; // the backing arrays: compared in place, no copies
    LCounts[LIndex] := ALists[LIndex].Count;
    Inc(LTotal, LCounts[LIndex]);

    if LCounts[LIndex] > 0 then
    begin
      LHeap[LHeapSize] := LIndex;
      Inc(LHeapSize);
    end;
  end;

  for var LHeapIndex := LHeapSize div 2 - 1 downto 0 do
    SiftDown(LHeapIndex);

  SetLength(Result, LTotal);

  for var LResultIndex := 0 to LTotal - 1 do
  begin
    var LList := LHeap[0];

    Result[LResultIndex] := LItems[LList][LPositions[LList]];
    Inc(LPositions[LList]);

    // That list is used up: its heap slot goes to the last heap entry.
    if LPositions[LList] = LCounts[LList] then
    begin
      Dec(LHeapSize);
      LHeap[0] := LHeap[LHeapSize];
    end;

    if LHeapSize > 0 then
      SiftDown(0);
  end;
end;

function TParallelFileScannerCustom.CollectFiles(const ADirectories: TArray<string>;
  const AExclusions: TFileScanExclusions; const ASort: Boolean): TArray<string>;
var
  LComparer: IComparer<string>;
  LWorkerCount: Integer;
  LWorkerFiles: TArray<TList<string>>;
begin
  LComparer := TPathComparer.Create;
  LWorkerCount := GetWorkerCount;
  SetLength(LWorkerFiles, LWorkerCount);
  try
    for var LIndex := 0 to LWorkerCount - 1 do
      LWorkerFiles[LIndex] := TList<string>.Create;

    // Each worker appends to its own list, so collecting needs no locking at all. When sorting, each worker
    // also sorts its own list as soon as the walk is done - in parallel - leaving only a merge for this thread.
    RunParallelWalk(ADirectories, AExclusions, LWorkerCount,
      function(AWorkerIndex: Integer): TDirectoryWalkProc
      begin
        var LFiles := LWorkerFiles[AWorkerIndex];

        Result :=
          procedure(const AFileName: string)
          begin
            LFiles.Add(AFileName);
          end;
      end,
      procedure(AWorkerIndex: Integer)
      begin
        if ASort then
          LWorkerFiles[AWorkerIndex].Sort(LComparer);
      end);

    if ASort then
      Result := MergeSortedLists(LWorkerFiles, LComparer)
    else
      Result := ConcatenateLists(LWorkerFiles);
  finally
    for var LFiles in LWorkerFiles do
      LFiles.Free;
  end;
end;

procedure TParallelFileScannerCustom.ScanInto(const ADirectories: TArray<string>; const AExclusions: TFileScanExclusions;
  const AResult: TStringList);
var
  LFiles: TArray<string>;
  LFileScanStopWatch: TStopwatch;
  LMergeSorted: Boolean;
begin
  LFileScanStopWatch := TStopwatch.StartNew;

  // The walk hands the files over already sorted when they go into an empty list; items the caller added
  // earlier still need the full sort below, and a Sorted list keeps its own order anyway. No de-duplication
  // pass: overlapping roots are merged before the walk (PrepareRootDirectories).
  LMergeSorted := FSortResultList and (AResult.Count = 0) and not AResult.Sorted;

  LFiles := CollectFiles(ADirectories, AExclusions, LMergeSorted);

  AResult.Capacity := AResult.Count + Length(LFiles);
  AResult.AddStrings(LFiles);

  if FSortResultList and not LMergeSorted then
    AResult.CustomSort(ComparePathsCI);

  LFileScanStopWatch.Stop;
  FDiskScanTimeForFiles := LFileScanStopWatch.Elapsed.TotalMilliseconds;
end;

function TParallelFileScannerCustom.ScanFiles(const ADirectories: TArray<string>; const AExclusions: TFileScanExclusions;
  const AFileFoundCallback: TFileFoundCallbackProc; var AFileCount: Integer): Boolean;
var
  LFileScanStopWatch: TStopwatch;
  LFileCount: Integer;
begin
  LFileScanStopWatch := TStopwatch.StartNew;

  LFileCount := 0; // local because anonymous methods cannot capture var parameters

  // The callback is created inside the factory rather than kept in a local: a local that holds an
  // anonymous method and is itself captured by another one makes the shared closure frame reference
  // itself, and it is never freed.
  RunParallelWalk(ADirectories, AExclusions, GetWorkerCount,
    function(AWorkerIndex: Integer): TDirectoryWalkProc
    begin
      Result :=
        procedure(const AFileName: string)
        begin
          // Fires on worker threads as files are found; the callback's own thread
          // safety is the caller's responsibility.
          AFileFoundCallback(AFileName);
          AtomicIncrement(LFileCount);
        end;
    end,
    nil);

  AFileCount := LFileCount;
  Result := AFileCount > 0;

  LFileScanStopWatch.Stop;
  FDiskScanTimeForFiles := LFileScanStopWatch.Elapsed.TotalMilliseconds;
end;

function TParallelFileScannerCustom.ScanFiles(const ADirectories: TArray<string>; const AExclusions: TFileScanExclusions;
  const AFileFoundCallback: TFileFoundCallback; var AFileCount: Integer): Boolean;
var
  LCallbackProc: TFileFoundCallbackProc;
begin
  // Wrap the "of object" method pointer into a method reference and delegate. The explicit
  // local forces overload resolution to the method-reference overload (passing the parameter
  // straight through would resolve right back into this overload).
  LCallbackProc := AFileFoundCallback;

  Result := ScanFiles(ADirectories, AExclusions, LCallbackProc, AFileCount);
end;

function TParallelFileScannerCustom.GetFileList(const ADirectories: TArray<string>; const AExclusions: TFileScanExclusions;
  const AFileNamesList: TStringList): Boolean;
begin
  ScanInto(ADirectories, AExclusions, AFileNamesList);

  Result := AFileNamesList.Count > 0;
end;

function TParallelFileScannerCustom.GetFileList(const ADirectories: TStringList; const AExclusions: TFileScanExclusions;
  const AFileNamesList: TStringList): Boolean;
begin
  Result := GetFileList(ADirectories.ToStringArray, AExclusions, AFileNamesList);
end;

function TParallelFileScannerCustom.GetWorkerCount: Integer;
begin
  if Assigned(FWorkers) then
    Result := FWorkers.Count
  else
    Result := TFileScanWorkers.CoreCount;
end;

function TParallelFileScannerCustom.GetWorkerPriority: TThreadPriority;
begin
  if Assigned(FWorkers) then
    Result := FWorkers.Priority
  else
    Result := TThreadPriority.tpNormal;
end;

function TParallelFileScannerCustom.GetSkippedFilesCount: Integer;
begin
  // Counted once per scan - a flag of its own, as a count of 0 is a result too.
  if not FSkippedDirectoriesCounted then
  begin
    FCachedSkippedDirectoriesFileCount := GetFileCounts(FSkippedDirectories);
    FSkippedDirectoriesCounted := True;
  end;

  Result := FSkippedFilesCount + FCachedSkippedDirectoriesFileCount;
end;

procedure TParallelFileScannerCustom.ScanDirectory(const APath: string; const AFileFound: TDirectoryWalkProc;
  const ASubDirectories: TList<string>);
var
  LFindData: TWin32FindData;
  LFindHandle: THandle;
  LName: PChar;
  LNameLength: Integer;
begin
  // Entries are examined in the find-data buffer; a string is only built - in one allocation, straight from
  // APath and the name - for a matching file or a subdirectory to walk, never for a skipped entry.
  LFindHandle := OpenDirectoryEnumeration(APath, LFindData);
  if LFindHandle <> INVALID_HANDLE_VALUE then
  try
    repeat
      LName := @LFindData.cFileName[0];
      LNameLength := StrLen(LName);

      if LFindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY <> 0 then
      begin
        if SkipDirectoryEntry(LFindData, FFollowDirectoryLinks) then
          Continue;

        var LSubDirectory := JoinPath(APath, LName, LNameLength, True);

        if ExcludedDirectory(LSubDirectory) then
          AddSkippedDirectories(LSubDirectory)
        else
          ASubDirectories.Add(LSubDirectory);
      end
      else if MatchesAnyExtension(LName, LNameLength) then
      begin
        var LFileName := JoinPath(APath, LName, LNameLength, False);

        // Runs on multiple worker threads, so the skipped counter must be incremented atomically.
        if ExcludedFile(LFileName) then
          AtomicIncrement(FSkippedFilesCount)
        else
          AFileFound(LFileName);
      end;
    until not FindNextFile(LFindHandle, LFindData);
  finally
    Winapi.Windows.FindClose(LFindHandle);
  end;
end;

procedure TParallelFileScannerCustom.RunParallelWalk(const ADirectories: TArray<string>;
  const AExclusions: TFileScanExclusions; const AWorkerCount: Integer;
  const AAcquireFileSink: TFunc<Integer, TDirectoryWalkProc>; const AFinishWorker: TProc<Integer>);
var
  LRoots: TArray<string>;
begin
  // The walk's state - exclusions, matchers, counters - lives in the scanner, so a second walk would replace it under
  // the running one's workers. Refused before anything is touched.
  if AtomicCmpExchange(FScanActive, 1, 0) <> 0 then
    raise EInvalidOpException.Create('This scanner is already scanning: use a scanner of its own for each scan that '
      + 'runs at the same time, also for one started from a ScanFiles callback');
  try
    FExclusions := AExclusions;

    ResetCounters;
    FScanCurrentDirectory := GetCurrentDir;
    FreeRules;
    PrepareExclusions;
    PrepareExtensions;

    LRoots := PrepareRootDirectories(ADirectories);
    FScanRoots := LRoots;

    if Length(LRoots) = 0 then
      Exit;

    RunWalkWorkers(LRoots, AWorkerCount, AAcquireFileSink, AFinishWorker);
  finally
    AtomicExchange(FScanActive, 0);
  end;
end;

procedure TParallelFileScannerCustom.RunWalkWorkers(const ARoots: TArray<string>; const AWorkerCount: Integer;
  const AAcquireFileSink: TFunc<Integer, TDirectoryWalkProc>; const AFinishWorker: TProc<Integer>);
var
  LWorker: TProc<Integer>;
  LWorkPool: TDirectoryWorkPool;
begin
  LWorkPool := TDirectoryWorkPool.Create(ARoots);
  try
    // One worker; AWorkerIndex (0..AWorkerCount-1) doubles as its slot for a private file sink.
    LWorker :=
      procedure(AWorkerIndex: Integer)
      var
        LDirectory: string;
        LFileFound: TDirectoryWalkProc;
        LLocalStack: TList<string>;
      begin
        LFileFound := AAcquireFileSink(AWorkerIndex);
        LLocalStack := TList<string>.Create;
        try
          try
            // Take a subtree, walk it depth-first on the private stack and, after each directory, hand
            // the shallowest pending directories to workers that ran dry; repeat until the tree is done.
            while LWorkPool.TakeWork(LDirectory) do
            begin
              LLocalStack.Add(LDirectory);

              while (LLocalStack.Count > 0) and not LWorkPool.Aborted do
              begin
                LDirectory := LLocalStack.Last;
                LLocalStack.Delete(LLocalStack.Count - 1);

                ScanDirectory(LDirectory, LFileFound, LLocalStack);
                LWorkPool.ShareWork(LLocalStack);
              end;

              LLocalStack.Clear; // only non-empty when another worker aborted the walk
              LWorkPool.FinishSubtree;
            end;

            // The whole walk is done: let the caller post-process this worker's share, in parallel.
            if Assigned(AFinishWorker) and not LWorkPool.Aborted then
              AFinishWorker(AWorkerIndex);
          except
            // Without this the other workers would wait forever for this one to finish its subtree.
            LWorkPool.Abort;

            raise;
          end;
        finally
          LLocalStack.Free;
        end;
      end;

    ExecuteWorkers(AWorkerCount, LWorker, GetWorkerPriority);
  finally
    LWorkPool.Free;
  end;
end;

constructor TParallelFileScannerCustom.Create(const AExtensions: TStringList; const AWorkers: TFileScanWorkers;
  const ASortResultList: Boolean = True);
begin
  // Owned before anything here can raise - AExtensions.ToStringArray included.
  FWorkers := AWorkers;

  Create(AExtensions.ToStringArray, AWorkers, ASortResultList);
end;

{ TParallelFileScanner }

procedure TParallelFileScanner.ExecuteWorkers(const AWorkerCount: Integer; const AWorker: TProc<Integer>;
  const APriority: TThreadPriority);
begin
  // TParallel.For raises a worker's exception (wrapped in EAggregateException) in the calling thread.
  TParallel.&For(0, AWorkerCount - 1,
    procedure(AWorkerIndex: Integer)
    var
      LPreviousPriority: Integer;
    begin
      // PPL threads are shared by the whole process, so the priority is only borrowed for the worker.
      LPreviousPriority := GetThreadPriority(GetCurrentThread);

      if LPreviousPriority <> THREAD_PRIORITIES[APriority] then
        SetThreadPriority(GetCurrentThread, THREAD_PRIORITIES[APriority]);
      try
        AWorker(AWorkerIndex);
      finally
        if LPreviousPriority <> THREAD_PRIORITIES[APriority] then
          SetThreadPriority(GetCurrentThread, LPreviousPriority);
      end;
    end);
end;

end.
