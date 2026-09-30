unit DPFSUnit.Parallel.FileScanner;

// Windows only (FindFirstFileEx, Windows thread priorities), so TThreadPriority being Windows-specific is fine.
{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  System.Classes, System.Generics.Collections, System.IOUtils, System.SyncObjs, System.SysUtils;

type
  TDirectoryWalkProc = reference to procedure(const AFileName: string);
  // Streaming ScanFiles callback, two flavours:
  // - TFileFoundCallbackProc: anonymous method / method reference (same signature as the internal walk callback)
  // - TFileFoundCallback: classic "of object" event-handler style method pointer
  TFileFoundCallbackProc = TDirectoryWalkProc;
  TFileFoundCallback = procedure(const AFileName: string) of object;

  TExclusionKind = (ekPathPrefixes, ekPathSuffixes);

  TFileScanExclusions = record
  strict private
    FPathPrefixes: TArray<string>;
    FPathSuffixes: TArray<string>;
    function GetPathPrefixesString: string;
    function GetPathSuffixesString: string;
  public
    procedure InitArrayFromStrings(const AKind: TExclusionKind; const AArrayData: TStrings);
    property PathPrefixes: TArray<string> read FPathPrefixes write FPathPrefixes;
    property PathPrefixesString: string read GetPathPrefixesString;
    property PathSuffixes: TArray<string> read FPathSuffixes write FPathSuffixes;
    property PathSuffixesString: string read GetPathSuffixesString;
  end;

  // Base class of the scanners: pattern matching, the load-balanced parallel walk and the results - everything but
  // running the walk's workers, which is left to a descendant's ExecuteWorkers, so this unit needs nothing outside
  // the RTL. TParallelFileScanner (below) runs them on the RTL PPL, TParallelFileScannerOTL
  // (DPFSUnit.Parallel.FileScanner.OTL) on OmniThreadLibrary. Priorities are the RTL's TThreadPriority throughout.
  TParallelFileScannerCustom = class(TObject)
  strict private
    FSkippedDirectories: TStringList;
    FCachedSkippedDirectoriesFileCount: Integer;
    FConvertRelativePathsToAbsolute: Boolean;
    FExcludedPrefixes: TArray<string>; // FExclusions.PathPrefixes without trailing delimiters (per scan)
    FExcludedSuffixes: TArray<string>; // FExclusions.PathSuffixes (per scan)
    FFastExtensions: TArray<string>;  // extensions (with dot, e.g. '.pas') for simple "*.ext" patterns
    FComplexPatterns: TArray<string>; // patterns that still need full TPath.MatchesPattern
    function GetFileCounts(const ASkippedDirectories: TStringList): Integer;
    function GetSkippedFilesCount: Integer;
    procedure AddSkippedDirectories(const APath: string);
  strict protected
    FDiskScanTimeForFiles: Double;
    FExclusions: TFileScanExclusions;
    FExtensions: TStringList;
    FLock: TMonitor;
    FSkippedFilesCount: Integer;
    FSortResultList: Boolean;
    // Shared parallel walk behind the list results: returns every matching file, in CompareText order when
    // ASort is set - each worker sorts its own share in parallel, then the shares are merged.
    function CollectFiles(const ADirectories: TArray<string>; const AExclusions: TFileScanExclusions; const ASort: Boolean;
      const APriority: TThreadPriority): TArray<string>;
    function ExcludedFileNameBySuffix(const AFileName: string): Boolean;
    function ExcludedPathByPrefix(const APath: string): Boolean;
    // Workers per walk: one per core. Workers that find nothing to do just wait for a hand-off, so using every
    // core costs little.
    function GetWorkerCount: Integer; virtual;
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
    // results can be post-processed in parallel.
    procedure RunParallelWalk(const ADirectories: TArray<string>; const AExclusions: TFileScanExclusions;
      const AWorkerCount: Integer; const AAcquireFileSink: TFunc<Integer, TDirectoryWalkProc>;
      const AFinishWorker: TProc<Integer>; const APriority: TThreadPriority);
    // Enumerates one directory (non-recursively) in a single pass: matching files go to AFileFound,
    // non-excluded subdirectories are appended to ASubDirectories. APath and the subdirectories it adds
    // end in a path delimiter.
    procedure ScanDirectory(const APath: string; const AFileFound: TDirectoryWalkProc; const ASubDirectories: TList<string>);
    // TStringList scan core; the Spring4D scanner fills its IList<string> from CollectFiles itself.
    procedure ScanInto(const ADirectories: TArray<string>; const AExclusions: TFileScanExclusions;
      const AResult: TStringList; const APriority: TThreadPriority);
  public
    constructor Create(const AExtensions: TArray<string>; const ASortResultList: Boolean = True); overload; virtual;
    constructor Create(const AExtensions: TStringList; const ASortResultList: Boolean = True); overload;
    destructor Destroy; override;

    // Adds every matching file to AFileNamesList, in CompareText order when SortResultList is set. APriority is
    // the priority of the worker threads during the scan.
    function GetFileList(const ADirectories: TArray<string>; const AExclusions: TFileScanExclusions;
      const AFileNamesList: TStringList; const APriority: TThreadPriority = tpNormal): Boolean; overload;
    function GetFileList(const ADirectories: TStringList; const AExclusions: TFileScanExclusions;
      const AFileNamesList: TStringList; const APriority: TThreadPriority = tpNormal): Boolean; overload;
    // Streaming scan: AFileFoundCallback fires for every matching file AS IT IS FOUND, from
    // multiple worker threads concurrently - the callback MUST be thread-safe. Each file is
    // delivered once (overlapping roots are merged before the walk), in nondeterministic order;
    // SortResultList does not apply here. Available on every scanner class.
    function ScanFiles(const ADirectories: TArray<string>; const AExclusions: TFileScanExclusions;
      const AFileFoundCallback: TFileFoundCallbackProc; var AFileCount: Integer;
      const APriority: TThreadPriority = tpNormal): Boolean; overload;
    function ScanFiles(const ADirectories: TArray<string>; const AExclusions: TFileScanExclusions;
      const AFileFoundCallback: TFileFoundCallback; var AFileCount: Integer;
      const APriority: TThreadPriority = tpNormal): Boolean; overload;

    property DiskScanTimeForFiles: Double read FDiskScanTimeForFiles; // in milliseconds
    property SkippedFilesCount: Integer read GetSkippedFilesCount;
    property SortResultList: Boolean read FSortResultList write FSortResultList;
    property ConvertRelativePathsToAbsolute: Boolean read FConvertRelativePathsToAbsolute write FConvertRelativePathsToAbsolute;
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
  // TThreadPriority as SetThreadPriority values - the same mapping TThread.Priority uses.
  THREAD_PRIORITIES: array[TThreadPriority] of Integer = (THREAD_PRIORITY_IDLE, THREAD_PRIORITY_LOWEST,
    THREAD_PRIORITY_BELOW_NORMAL, THREAD_PRIORITY_NORMAL, THREAD_PRIORITY_ABOVE_NORMAL, THREAD_PRIORITY_HIGHEST,
    THREAD_PRIORITY_TIME_CRITICAL);

// Opens ADirectory (which ends in a path delimiter) for enumeration via FindFirstFileEx. FindExInfoBasic
// skips 8.3 short-name retrieval and FIND_FIRST_EX_LARGE_FETCH fetches in larger batches - both markedly
// faster than System.SysUtils.FindFirst (which wraps FindFirstFile with short-name generation and small
// buffers) on directory-heavy scans. Returns INVALID_HANDLE_VALUE if the directory cannot be opened
// (AFindData is then undefined); otherwise iterate with FindNextFile and close with Winapi.Windows.FindClose.
function OpenDirectoryEnumeration(const ADirectory: string; var AFindData: TWin32FindData): THandle;
begin
  Result := FindFirstFileEx(PChar(ADirectory + '*'), FindExInfoBasic, @AFindData, FindExSearchNameMatch, nil,
    FIND_FIRST_EX_LARGE_FETCH);
end;

// True for the '.' and '..' entries every directory enumeration returns.
function IsDotDirectory(const AName: PChar): Boolean; inline;
begin
  Result := (AName[0] = '.') and ((AName[1] = #0) or ((AName[1] = '.') and (AName[2] = #0)));
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

procedure TFileScanExclusions.InitArrayFromStrings(const AKind: TExclusionKind; const AArrayData: TStrings);
begin
  case AKind of
    ekPathPrefixes: InitArrayDataFromStrings(FPathPrefixes, AArrayData);
    ekPathSuffixes: InitArrayDataFromStrings(FPathSuffixes, AArrayData);
  end;
end;

{ TParallelFileScannerCustom }

procedure TParallelFileScannerCustom.AddSkippedDirectories(const APath: string);
begin
  // Called from ScanDirectory, which runs on multiple worker threads, so
  // access to the shared FSkippedDirectories list must be serialized.
  FLock.Enter(FSkippedDirectories);
  try
    for var LIndex := 0 to FSkippedDirectories.Count - 1 do
    begin
      if APath.StartsWith(FSkippedDirectories[LIndex]) then
        Exit;
    end;

    if FSkippedDirectories.IndexOf(APath) = -1 then
      FSkippedDirectories.Add(APath);
  finally
    FLock.Exit(FSkippedDirectories);
  end;
end;

function TParallelFileScannerCustom.PrepareRootDirectories(const ADirectories: TArray<string>): TArray<string>;

  // True when another root already covers root AIndex: the same directory given again (the first spelling
  // is kept), or an ancestor whose walk reaches it - i.e. unless it is under an excluded prefix, in which
  // case the ancestor's walk skips it and it stays a root of its own, as before.
  function IsCoveredRoot(const ARoots: TList<string>; const AIndex: Integer): Boolean;
  begin
    for var LOtherIndex := 0 to ARoots.Count - 1 do
      if (LOtherIndex <> AIndex) and StartsTextOrdinal(ARoots[LOtherIndex], ARoots[AIndex]) then
      begin
        if Length(ARoots[LOtherIndex]) = Length(ARoots[AIndex]) then
        begin
          if LOtherIndex < AIndex then
            Exit(True);
        end
        else if not ExcludedPathByPrefix(ARoots[AIndex]) then
          Exit(True);
      end;

    Result := False;
  end;

var
  LRoots: TList<string>;
begin
  LRoots := TList<string>.Create;
  try
    for var LRootPath in ADirectories do
    begin
      if not TDirectory.Exists(LRootPath) then
        Continue;

      // Resolve the root to an absolute path once, so every file produced from it is already
      // absolute and no per-file conversion pass is needed afterwards. The walk keeps directories with a
      // trailing delimiter, so entry names can be appended directly.
      if FConvertRelativePathsToAbsolute then
        LRoots.Add(IncludeTrailingPathDelimiter(TPath.GetFullPath(LRootPath)))
      else
        LRoots.Add(IncludeTrailingPathDelimiter(LRootPath));
    end;

    // Drop every root another root already covers, so no file is produced twice and the results need no
    // de-duplication pass. Decided against the full list, so the outcome does not depend on the order in
    // which covered roots are dropped, and the roots keep the caller's order.
    SetLength(Result, 0);

    for var LIndex := 0 to LRoots.Count - 1 do
      if not IsCoveredRoot(LRoots, LIndex) then
        Result := Result + [LRoots[LIndex]];
  finally
    LRoots.Free;
  end;
end;

procedure TParallelFileScannerCustom.PrepareExclusions;
begin
  // Normalised once per scan, so the per-directory checks neither allocate nor re-normalise. Prefixes are
  // compared without a trailing delimiter, as before.
  SetLength(FExcludedPrefixes, Length(FExclusions.PathPrefixes));

  for var LIndex := 0 to High(FExcludedPrefixes) do
    FExcludedPrefixes[LIndex] := ExcludeTrailingPathDelimiter(FExclusions.PathPrefixes[LIndex]);

  FExcludedSuffixes := Copy(FExclusions.PathSuffixes);
end;

procedure TParallelFileScannerCustom.PrepareExtensions;
var
  LFast: TList<string>;
  LComplex: TList<string>;
begin
  LFast := TList<string>.Create;
  LComplex := TList<string>.Create;
  try
    for var LIndex := 0 to FExtensions.Count - 1 do
    begin
      var LPattern := FExtensions[LIndex];

      if Trim(LPattern) = '' then
        raise EInOutArgumentException.Create('Empty search pattern')
      else if not TPath.HasValidFileNameChars(LPattern, True) then
        raise EInOutArgumentException.Create('Search pattern has invalid characters');

      // A plain "*.ext" pattern (no further wildcards) can be matched with a fast, allocation-free
      // extension compare instead of TPath.MatchesPattern, which would run once per file scanned.
      if LPattern.StartsWith('*.') then
      begin
        var LExtension := LPattern.Substring(1); // ".ext" from "*.ext"

        if (Length(LExtension) > 1) and (LExtension.IndexOfAny(['*', '?']) < 0) then
          LFast.Add(LExtension)
        else
          LComplex.Add(LPattern);
      end
      else
        LComplex.Add(LPattern);
    end;

    FFastExtensions := LFast.ToArray;
    FComplexPatterns := LComplex.ToArray;
  finally
    LComplex.Free;
    LFast.Free;
  end;
end;

constructor TParallelFileScannerCustom.Create(const AExtensions: TArray<string>; const ASortResultList: Boolean = True);
begin
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
  FSkippedDirectories.Free;
  FExtensions.Free;

  inherited Destroy;
end;

procedure TParallelFileScannerCustom.ResetCounters;
begin
  FSkippedFilesCount := 0;
  FSkippedDirectories.Clear;
  FCachedSkippedDirectoriesFileCount := 0;
end;

// Called once per subdirectory from every worker thread. The prefix arrays are indexed rather than walked
// with for..in, which would copy each shared string and bump its reference count from all threads at once.
function TParallelFileScannerCustom.ExcludedPathByPrefix(const APath: string): Boolean;
begin
  for var LIndex := 0 to High(FExcludedPrefixes) do
    if StartsTextOrdinal(FExcludedPrefixes[LIndex], APath) then
      Exit(True);

  Result := False;
end;

function TParallelFileScannerCustom.ExcludedFileNameBySuffix(const AFileName: string): Boolean;
begin
  for var LIndex := 0 to High(FExcludedSuffixes) do
    if EndsTextOrdinal(FExcludedSuffixes[LIndex], AFileName) then
      Exit(True);

  Result := False;
end;

// Called for every file entry of every directory, on the raw name in the find-data buffer (no string yet).
function TParallelFileScannerCustom.MatchesAnyExtension(const AFileName: PChar; const AFileNameLength: Integer): Boolean;
begin
  // Fast path: a plain "*.ext" pattern is an ordinal case-insensitive suffix test on the name buffer, no
  // allocation. Indexed rather than for..in for the same reason as ExcludedPathByPrefix.
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

    for var LIndex := 0 to High(FComplexPatterns) do
      if TPath.MatchesPattern(LFileName, FComplexPatterns[LIndex], False) then
        Exit(True);
  end;

  Result := False;
end;

function TParallelFileScannerCustom.GetFileCounts(const ASkippedDirectories: TStringList): Integer;
var
  LCurrentDir: string;
begin
  Result := 0;

  for var LDirectoryIndex := 0 to ASkippedDirectories.Count - 1 do
  begin
    LCurrentDir := ASkippedDirectories[LDirectoryIndex];

    if not LCurrentDir.Trim.IsEmpty then
      for var LExtensionIndex := 0 to FExtensions.Count - 1 do
        Inc(Result, Length(TDirectory.GetFiles(LCurrentDir, FExtensions[LExtensionIndex], TSearchOption.soAllDirectories)));
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
  const AExclusions: TFileScanExclusions; const ASort: Boolean; const APriority: TThreadPriority): TArray<string>;
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
      end,
      APriority);

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
  const AResult: TStringList; const APriority: TThreadPriority);
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

  LFiles := CollectFiles(ADirectories, AExclusions, LMergeSorted, APriority);

  AResult.Capacity := AResult.Count + Length(LFiles);
  AResult.AddStrings(LFiles);

  if FSortResultList and not LMergeSorted then
    AResult.CustomSort(ComparePathsCI);

  LFileScanStopWatch.Stop;
  FDiskScanTimeForFiles := LFileScanStopWatch.Elapsed.TotalMilliseconds;
end;

function TParallelFileScannerCustom.ScanFiles(const ADirectories: TArray<string>; const AExclusions: TFileScanExclusions;
  const AFileFoundCallback: TFileFoundCallbackProc; var AFileCount: Integer;
  const APriority: TThreadPriority = tpNormal): Boolean;
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
    nil, APriority);

  AFileCount := LFileCount;
  Result := AFileCount > 0;

  LFileScanStopWatch.Stop;
  FDiskScanTimeForFiles := LFileScanStopWatch.Elapsed.TotalMilliseconds;
end;

function TParallelFileScannerCustom.ScanFiles(const ADirectories: TArray<string>; const AExclusions: TFileScanExclusions;
  const AFileFoundCallback: TFileFoundCallback; var AFileCount: Integer;
  const APriority: TThreadPriority = tpNormal): Boolean;
var
  LCallbackProc: TFileFoundCallbackProc;
begin
  // Wrap the "of object" method pointer into a method reference and delegate. The explicit
  // local forces overload resolution to the method-reference overload (passing the parameter
  // straight through would resolve right back into this overload).
  LCallbackProc := AFileFoundCallback;

  Result := ScanFiles(ADirectories, AExclusions, LCallbackProc, AFileCount, APriority);
end;

function TParallelFileScannerCustom.GetFileList(const ADirectories: TArray<string>; const AExclusions: TFileScanExclusions;
  const AFileNamesList: TStringList; const APriority: TThreadPriority = tpNormal): Boolean;
begin
  ScanInto(ADirectories, AExclusions, AFileNamesList, APriority);

  Result := AFileNamesList.Count > 0;
end;

function TParallelFileScannerCustom.GetFileList(const ADirectories: TStringList; const AExclusions: TFileScanExclusions;
  const AFileNamesList: TStringList; const APriority: TThreadPriority = tpNormal): Boolean;
begin
  Result := GetFileList(ADirectories.ToStringArray, AExclusions, AFileNamesList, APriority);
end;

function TParallelFileScannerCustom.GetWorkerCount: Integer;
begin
  Result := Max(1, TThread.ProcessorCount);
end;

function TParallelFileScannerCustom.GetSkippedFilesCount: Integer;
begin
  if (FSkippedDirectories.Count > 0) and (FCachedSkippedDirectoriesFileCount = 0) then
    FCachedSkippedDirectoriesFileCount := GetFileCounts(FSkippedDirectories);

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
        if IsDotDirectory(LName) then
          Continue;

        var LSubDirectory := JoinPath(APath, LName, LNameLength, True);

        if ExcludedPathByPrefix(LSubDirectory) then
          AddSkippedDirectories(ExcludeTrailingPathDelimiter(LSubDirectory))
        else
          ASubDirectories.Add(LSubDirectory);
      end
      else if MatchesAnyExtension(LName, LNameLength) then
      begin
        var LFileName := JoinPath(APath, LName, LNameLength, False);

        // Runs on multiple worker threads, so the skipped counter must be incremented atomically.
        if ExcludedFileNameBySuffix(LFileName) then
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
  const AAcquireFileSink: TFunc<Integer, TDirectoryWalkProc>; const AFinishWorker: TProc<Integer>;
  const APriority: TThreadPriority);
var
  LRoots: TArray<string>;
  LWorker: TProc<Integer>;
  LWorkPool: TDirectoryWorkPool;
begin
  FExclusions := AExclusions;

  ResetCounters;
  PrepareExclusions;
  PrepareExtensions;

  LRoots := PrepareRootDirectories(ADirectories);

  if Length(LRoots) = 0 then
    Exit;

  LWorkPool := TDirectoryWorkPool.Create(LRoots);
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

    ExecuteWorkers(AWorkerCount, LWorker, APriority);
  finally
    LWorkPool.Free;
  end;
end;

constructor TParallelFileScannerCustom.Create(const AExtensions: TStringList; const ASortResultList: Boolean = True);
begin
  Create(AExtensions.ToStringArray, ASortResultList);
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
