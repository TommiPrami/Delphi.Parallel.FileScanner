unit DPFSUnit.Parallel.FileScanner.Tests;

// Windows only, like the scanners, so TThreadPriority being Windows-specific is fine.
{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  System.Classes, System.SysUtils, DUnitX.TestFramework, DPFSUnit.Parallel.FileScanner,
  DPFSUnit.Parallel.FileScanner.Tests.Common;

type
  // The tests every scanner class must pass, whatever runs its workers. Not a fixture itself: each scanner class has
  // a [TestFixture] descendant that names it (ScannerClass), and DUnitX runs these inherited tests on that.
  TParallelFileScannerTestsCustom = class abstract(TObject)
  strict private
    FBaseline: TFileSet;
    FScanRoots: TArray<string>;
  strict protected
    // The baseline files AKeep returns True for (it gets each file's lower-case full path).
    function BaselineWhere(const AKeep: TFunc<string, Boolean>): TFileSet;
    function CreateScanner(const ASortResultList: Boolean = True): TParallelFileScannerCustom;
    function ScannerClass: TParallelFileScannerClass; virtual; abstract;
    function ScanToStringList(const AScanner: TParallelFileScannerCustom; const ARoots: TArray<string>;
      const AExclusions: TFileScanExclusions): TArray<string>;
    // Scans the tree with AExclusions and asserts the result is AExpected (freed here).
    procedure AssertExclusionsLeave(const AExclusions: TFileScanExclusions; const AExpected: TFileSet;
      const AWhat: string);
    property Baseline: TFileSet read FBaseline;
    property ScanRoots: TArray<string> read FScanRoots;
  public
    [SetupFixture] procedure SetupFixture;
    [TearDownFixture] procedure TearDownFixture;

    [Test] procedure StringListMatchesBaseline;
    [Test] procedure StreamingCallbackMatchesBaseline;
    [Test] procedure ObjectCallbackMatchesBaseline;
    [Test] procedure OverlappingRootsListEachFileOnce;
    [Test] procedure OverlappingRootsStreamEachFileOnce;
    [Test] procedure NestedRootUnderExcludedPrefixIsScanned;
    [Test] procedure PrefixExclusionKeepsSubtreeOut;
    [Test] procedure SortedResultIsInCompareTextOrder;
    [Test] procedure WorkerExceptionReachesCaller;
    [Test] procedure ScanFromCallbackCompletes;
    [Test] procedure WorkersRunAtRequestedPriority;
    [Test] procedure WorkersLeaveBorrowedThreadsAsFound;
    [Test] procedure BackToBackScansPileUpNothing;

    [Test] procedure PatternExcludesFolderAnywhere;
    [Test] procedure QuotedAlternationExcludesEveryListedFolder;
    [Test] procedure PatternWithoutTrailingStarExcludesFilesOnly;
    [Test] procedure PruningPatternExcludesFoldersAndFiles;
    [Test] procedure PatternEndingInDelimiterExcludesFolders;
    [Test] procedure FolderPathMatchAloneDoesNotPrune;
    [Test] procedure MalformedExclusionPatternRaises;
    [Test] procedure WildcardSearchPatternMatchesFileNames;
    [Test] procedure SearchPatternWithPathDelimiterRaises;
    [Test] procedure PrefixExcludesWholeFoldersOnly;
    [Test] procedure SkippedFilesCountCountsPrunedFolders;
    [Test] procedure BlankExclusionsAreIgnored;
    [Test] procedure AlternationExtensionPatternIsAWildcard;
    [Test] procedure ScanOnBusyScannerRaises;
    [Test] procedure DirectoryLinksAreNotFollowed;
    [Test] procedure SkippedFilesCountCountsNestedRootFilesOnce;
    [Test] procedure ForwardSlashesAreReadAsBackslashes;

    [Test] procedure UnsortedResultHoldsEveryFile;
    [Test] procedure AppendingKeepsEarlierItemsAndSortsAll;
    [Test] procedure SortedStringListKeepsItsOwnOrder;
    [Test] procedure StringListRootsOverloadMatchesBaseline;
    [Test] procedure PathSuffixesExcludeFilesCaseInsensitively;
    [Test] procedure MissingAndFileRootsAreIgnored;
    [Test] procedure NoRootsFindNothing;
    [Test] procedure InvalidSearchPatternsRaise;
    [Test] procedure RelativeRootsFollowConvertSetting;
    [Test] procedure CountersAreResetBetweenScans;
    [Test] procedure ExtensionPatternMatchesWholeExtensionOnly;
    [Test] procedure WildcardSearchPatternsAreCaseInsensitive;
    [Test] procedure NonAsciiNamesCompareCaseInsensitively;
    [Test] procedure DominantSubtreeIsWalkedCompletely;
    [Test] procedure TreeWithoutMatchesFindsNothing;
    [Test] procedure RelativeExclusionsFollowConvertSetting;
    [Test] procedure PathsLongerThanMaxPathAreWalked;
    [Test] procedure SkippedFilesCountUsesTheScansCurrentDirectory;
    [Test] procedure RootInsideExcludedFolderIsScannedWhole;

    [Test] procedure WorkersCountLimitsWorkerThreads;
    [Test] procedure MoreWorkersThanCoresScanCorrectly;
  end;

  [TestFixture]
  TParallelFileScannerTests = class(TParallelFileScannerTestsCustom)
  strict protected
    function ScannerClass: TParallelFileScannerClass; override;
  end;

  // TFileScanExclusions' helpers - no scanning.
  [TestFixture]
  TFileScanExclusionsTests = class(TObject)
  public
    [Test] procedure InitArrayFromStringsFillsOnlyThatKind;
    [Test] procedure InitArrayDataFromStringsReplacesTheContent;
    [Test] procedure StringPropertiesJoinWithSemicolons;
  end;

implementation

uses
  Winapi.Windows, System.Generics.Collections, System.IOUtils, System.Threading, FastMM5;

{ TParallelFileScannerTestsCustom }

function TParallelFileScannerTestsCustom.BaselineWhere(const AKeep: TFunc<string, Boolean>): TFileSet;
begin
  Result := TFileSet.Create;

  for var LIndex := 0 to FBaseline.Count - 1 do
    if AKeep(FBaseline.Files[LIndex]) then
      Result.Add(FBaseline.Files[LIndex]);
end;

procedure TParallelFileScannerTestsCustom.AssertExclusionsLeave(const AExclusions: TFileScanExclusions;
  const AExpected: TFileSet; const AWhat: string);
var
  LScanner: TParallelFileScannerCustom;
begin
  try
    LScanner := CreateScanner;
    try
      AssertSameFiles(AExpected, ScanToStringList(LScanner, FScanRoots, AExclusions), AWhat);
    finally
      LScanner.Free;
    end;
  finally
    AExpected.Free;
  end;
end;

function TParallelFileScannerTestsCustom.CreateScanner(const ASortResultList: Boolean = True): TParallelFileScannerCustom;
begin
  Result := ScannerClass.Create(TScanTree.Extensions, nil, ASortResultList);
  Result.ConvertRelativePathsToAbsolute := True;
end;

function TParallelFileScannerTestsCustom.ScanToStringList(const AScanner: TParallelFileScannerCustom;
  const ARoots: TArray<string>; const AExclusions: TFileScanExclusions): TArray<string>;
var
  LFiles: TStringList;
begin
  LFiles := TStringList.Create;
  try
    AScanner.GetFileList(ARoots, AExclusions, LFiles);

    Result := LFiles.ToStringArray;
  finally
    LFiles.Free;
  end;
end;

procedure TParallelFileScannerTestsCustom.SetupFixture;
begin
  FScanRoots := [TScanTree.SourceRoot];
  FBaseline := TScanTree.BaselineFiles(FScanRoots);
end;

procedure TParallelFileScannerTestsCustom.TearDownFixture;
begin
  FreeAndNil(FBaseline);
end;

procedure TParallelFileScannerTestsCustom.StringListMatchesBaseline;
var
  LExclusions: TFileScanExclusions;
  LScanner: TParallelFileScannerCustom;
begin
  LScanner := CreateScanner;
  try
    AssertSameFiles(FBaseline, ScanToStringList(LScanner, FScanRoots, LExclusions), 'GetFileList');
  finally
    LScanner.Free;
  end;
end;

// Files are delivered from worker threads as they are found, so the callback locks the shared list - exactly what
// real calling code must do.
procedure TParallelFileScannerTestsCustom.StreamingCallbackMatchesBaseline;
var
  LExclusions: TFileScanExclusions;
  LFileCount: Integer;
  LFiles: TStringList;
  LScanner: TParallelFileScannerCustom;
begin
  LScanner := CreateScanner;
  LFiles := TStringList.Create;
  try
    LFileCount := 0;

    LScanner.ScanFiles(FScanRoots, LExclusions,
      procedure(const AFileName: string)
      begin
        TMonitor.Enter(LFiles);
        try
          LFiles.Add(AFileName);
        finally
          TMonitor.Exit(LFiles);
        end;
      end,
      LFileCount);

    Assert.AreEqual(LFiles.Count, LFileCount, 'AFileCount must equal the number of files delivered');
    AssertSameFiles(FBaseline, LFiles.ToStringArray, 'ScanFiles');
  finally
    LFiles.Free;
    LScanner.Free;
  end;
end;

procedure TParallelFileScannerTestsCustom.ObjectCallbackMatchesBaseline;
var
  LCollector: TCallbackCollector;
  LExclusions: TFileScanExclusions;
  LFileCount: Integer;
  LScanner: TParallelFileScannerCustom;
begin
  LScanner := CreateScanner;
  LCollector := TCallbackCollector.Create;
  try
    LFileCount := 0;

    LScanner.ScanFiles(FScanRoots, LExclusions, LCollector.FileFound, LFileCount);

    Assert.AreEqual(LCollector.Files.Count, LFileCount, 'AFileCount must equal the number of files delivered');
    AssertSameFiles(FBaseline, LCollector.Files.ToStringArray, 'ScanFiles (of object)');
  finally
    LCollector.Free;
    LScanner.Free;
  end;
end;

// Overlapping roots - the root again, spelled with a trailing delimiter and in upper case, plus a nested one - must be
// walked once: every file exactly once, and the same set as scanning the root alone.
procedure TParallelFileScannerTestsCustom.OverlappingRootsListEachFileOnce;
var
  LExclusions: TFileScanExclusions;
  LScanner: TParallelFileScannerCustom;
begin
  LScanner := CreateScanner;
  try
    AssertSameFiles(FBaseline, ScanToStringList(LScanner, [FScanRoots[0], TPath.Combine(FScanRoots[0], 'Units'),
      IncludeTrailingPathDelimiter(FScanRoots[0]), FScanRoots[0].ToUpper], LExclusions), 'GetFileList');
  finally
    LScanner.Free;
  end;
end;

procedure TParallelFileScannerTestsCustom.OverlappingRootsStreamEachFileOnce;
var
  LExclusions: TFileScanExclusions;
  LFileCount: Integer;
  LFiles: TStringList;
  LScanner: TParallelFileScannerCustom;
begin
  LScanner := CreateScanner;
  LFiles := TStringList.Create;
  try
    LFileCount := 0;

    LScanner.ScanFiles([FScanRoots[0], TPath.Combine(FScanRoots[0], 'Units'),
      IncludeTrailingPathDelimiter(FScanRoots[0]), FScanRoots[0].ToUpper], LExclusions,
      procedure(const AFileName: string)
      begin
        TMonitor.Enter(LFiles);
        try
          LFiles.Add(AFileName);
        finally
          TMonitor.Exit(LFiles);
        end;
      end,
      LFileCount);

    AssertSameFiles(FBaseline, LFiles.ToStringArray, 'ScanFiles');
  finally
    LFiles.Free;
    LScanner.Free;
  end;
end;

// A root nested under an excluded prefix of another root is not reached by that root's walk, so it must stay a root
// of its own - and, as it was asked for, the prefix it lies under does not apply inside it: all of it is scanned.
procedure TParallelFileScannerTestsCustom.NestedRootUnderExcludedPrefixIsScanned;
var
  LExcludedPrefix: string;
  LExclusions: TFileScanExclusions;
  LExpected: TFileSet;
  LNestedRoot: string;
  LScanner: TParallelFileScannerCustom;
begin
  LExcludedPrefix := TPath.GetFullPath(TPath.Combine(FScanRoots[0], '3rdPartyLibraries'));
  LNestedRoot := TPath.Combine(LExcludedPrefix, 'FastMM5');

  // Everything outside the excluded prefix, plus everything in the nested root.
  LExpected := TFileSet.Create;
  try
    for var LIndex := 0 to FBaseline.Count - 1 do
      if not FBaseline.Files[LIndex].StartsWith(LExcludedPrefix.ToLower)
        or FBaseline.Files[LIndex].StartsWith(IncludeTrailingPathDelimiter(LNestedRoot).ToLower) then
        LExpected.Add(FBaseline.Files[LIndex]);

    LScanner := CreateScanner;
    try
      LExclusions.PathPrefixes := [LExcludedPrefix];

      AssertSameFiles(LExpected, ScanToStringList(LScanner, [FScanRoots[0], LNestedRoot], LExclusions),
        'GetFileList');
    finally
      LScanner.Free;
    end;
  finally
    LExpected.Free;
  end;
end;

// The prefix is given in lower case on purpose: prefixes are compared case-insensitively.
procedure TParallelFileScannerTestsCustom.PrefixExclusionKeepsSubtreeOut;
var
  LExcludedPrefix: string;
  LExclusions: TFileScanExclusions;
  LExpected: TFileSet;
  LScanner: TParallelFileScannerCustom;
begin
  LExcludedPrefix := TPath.GetFullPath(TPath.Combine(FScanRoots[0], '3rdPartyLibraries')).ToLower;

  LExpected := TFileSet.Create;
  try
    for var LIndex := 0 to FBaseline.Count - 1 do
      if not FBaseline.Files[LIndex].StartsWith(LExcludedPrefix) then
        LExpected.Add(FBaseline.Files[LIndex]);

    Assert.IsTrue(LExpected.Count > 0, 'The tree must hold files outside the excluded subtree');

    LScanner := CreateScanner;
    try
      LExclusions.PathPrefixes := [LExcludedPrefix];

      AssertSameFiles(LExpected, ScanToStringList(LScanner, FScanRoots, LExclusions), 'GetFileList');
    finally
      LScanner.Free;
    end;
  finally
    LExpected.Free;
  end;
end;

// With SortResultList (the default) the result must come back in CompareText order.
procedure TParallelFileScannerTestsCustom.SortedResultIsInCompareTextOrder;
var
  LDisorders: Integer;
  LExclusions: TFileScanExclusions;
  LFiles: TArray<string>;
  LScanner: TParallelFileScannerCustom;
begin
  LScanner := CreateScanner;
  try
    LFiles := ScanToStringList(LScanner, FScanRoots, LExclusions);
  finally
    LScanner.Free;
  end;

  LDisorders := 0;

  for var LIndex := 1 to High(LFiles) do
    if CompareText(LFiles[LIndex - 1], LFiles[LIndex]) > 0 then
      Inc(LDisorders);

  Assert.IsTrue(Length(LFiles) > 1, 'The scan must find files to sort');
  Assert.AreEqual(0, LDisorders, 'Neighbours out of CompareText order');
end;

// An exception in a worker - here raised by the caller's own callback - must reach the caller instead of being
// swallowed on a pool thread, and the scanner must still work afterwards.
procedure TParallelFileScannerTestsCustom.WorkerExceptionReachesCaller;
var
  LExclusions: TFileScanExclusions;
  LScanner: TParallelFileScannerCustom;
begin
  LScanner := CreateScanner;
  try
    Assert.WillRaiseAny(
      procedure
      var
        LFileCount: Integer;
      begin
        LFileCount := 0;

        LScanner.ScanFiles(FScanRoots, LExclusions,
          procedure(const AFileName: string)
          begin
            raise EScannerTestCallbackFailure.Create('Callback failure');
          end,
          LFileCount);
      end,
      'An exception raised in a worker must be raised in the caller');

    AssertSameFiles(FBaseline, ScanToStringList(LScanner, FScanRoots, LExclusions), 'GetFileList after the exception');
  finally
    LScanner.Free;
  end;
end;

// A scan started from inside a ScanFiles callback - on a worker thread, while the outer walk runs - must finish and
// return the right files instead of waiting forever for pool threads the outer walk holds.
procedure TParallelFileScannerTestsCustom.ScanFromCallbackCompletes;
var
  LExclusions: TFileScanExclusions;
  LFileCount: Integer;
  LNestedFiles: TArray<string>;
  LNestedStarted: Integer;
  LOuterFiles: TStringList;
  LScanner: TParallelFileScannerCustom;
begin
  LScanner := CreateScanner;
  LOuterFiles := TStringList.Create;
  try
    LFileCount := 0;
    LNestedStarted := 0;

    LScanner.ScanFiles(FScanRoots, LExclusions,
      procedure(const AFileName: string)
      begin
        if AtomicCmpExchange(LNestedStarted, 1, 0) = 0 then
        begin
          var LNestedScanner := CreateScanner;
          try
            LNestedFiles := ScanToStringList(LNestedScanner, FScanRoots, LExclusions);
          finally
            LNestedScanner.Free;
          end;
        end;

        TMonitor.Enter(LOuterFiles);
        try
          LOuterFiles.Add(AFileName);
        finally
          TMonitor.Exit(LOuterFiles);
        end;
      end,
      LFileCount);

    AssertSameFiles(FBaseline, LOuterFiles.ToStringArray, 'Outer ScanFiles');
    AssertSameFiles(FBaseline, LNestedFiles, 'GetFileList from inside the callback');
  finally
    LOuterFiles.Free;
    LScanner.Free;
  end;
end;

// The workers must run at Workers.Priority. Pool threads are reused, so a scan after a tpLower one must run at normal
// priority again: with Workers.Priority tpNormal, and without Workers.
procedure TParallelFileScannerTestsCustom.WorkersRunAtRequestedPriority;

  // Scans with AScanner and asserts every file was found on a thread at AExpectedPriority.
  procedure AssertScanPriority(const AScanner: TParallelFileScannerCustom; const AExpectedPriority: Integer;
    const AWhat: string);
  var
    LExclusions: TFileScanExclusions;
    LFileCount: Integer;
    LWrongPriorityCount: Integer;
  begin
    LFileCount := 0;
    LWrongPriorityCount := 0;

    AScanner.ScanFiles(FScanRoots, LExclusions,
      procedure(const AFileName: string)
      begin
        if GetThreadPriority(GetCurrentThread) <> AExpectedPriority then
          AtomicIncrement(LWrongPriorityCount);
      end,
      LFileCount);

    Assert.IsTrue(LFileCount > 0, AWhat + ': the scan must find files');
    Assert.AreEqual(0, LWrongPriorityCount, AWhat + ': files found on a thread at another priority');
  end;

var
  LScanner: TParallelFileScannerCustom;
  LUnsetScanner: TParallelFileScannerCustom;
begin
  LScanner := ScannerClass.Create(TScanTree.Extensions, TFileScanWorkers.Create);
  LUnsetScanner := ScannerClass.Create(TScanTree.Extensions, nil);
  try
    LScanner.Workers.Priority := TThreadPriority.tpLower;
    AssertScanPriority(LScanner, THREAD_PRIORITY_BELOW_NORMAL, 'Workers.Priority tpLower');

    AssertScanPriority(LUnsetScanner, THREAD_PRIORITY_NORMAL, 'no Workers, after a tpLower scan');

    AssertScanPriority(LScanner, THREAD_PRIORITY_BELOW_NORMAL, 'Workers.Priority tpLower again');

    LScanner.Workers.Priority := TThreadPriority.tpNormal;
    AssertScanPriority(LScanner, THREAD_PRIORITY_NORMAL, 'Workers.Priority tpNormal, after a tpLower scan');
  finally
    LUnsetScanner.Free;
    LScanner.Free;
  end;
end;

// A scan at another priority must leave the threads it borrows as it found them: TParallelFileScanner's workers run
// on the process-wide RTL thread pool, so anyone's next TParallel.For must find those threads at their own priority.
// (A scan resets the priority itself, so WorkersRunAtRequestedPriority cannot tell.)
procedure TParallelFileScannerTestsCustom.WorkersLeaveBorrowedThreadsAsFound;
const
  // Enough short iterations that the pool spreads them over its threads - the ones the scan used among them.
  ITERATIONS_PER_CORE = 8;
var
  LExclusions: TFileScanExclusions;
  LFileCount: Integer;
  LScanner: TParallelFileScannerCustom;
  LThreads: TDictionary<TThreadID, Boolean>;
  LWrongPriorityCount: Integer;
begin
  LScanner := ScannerClass.Create(TScanTree.Extensions, TFileScanWorkers.Create);
  try
    LScanner.Workers.Priority := TThreadPriority.tpLower;
    LFileCount := 0;
    LScanner.ScanFiles(FScanRoots, LExclusions, procedure(const AFileName: string) begin end, LFileCount);
    Assert.IsTrue(LFileCount > 0, 'the scan must find files');
  finally
    LScanner.Free;
  end;

  Assert.AreEqual(THREAD_PRIORITY_NORMAL, GetThreadPriority(GetCurrentThread), 'the calling thread after the scan');

  LWrongPriorityCount := 0;
  LThreads := TDictionary<TThreadID, Boolean>.Create;
  try
    TParallel.&For(0, ITERATIONS_PER_CORE * TFileScanWorkers.CoreCount - 1,
      procedure(AIndex: Integer)
      begin
        if GetThreadPriority(GetCurrentThread) <> THREAD_PRIORITY_NORMAL then
          AtomicIncrement(LWrongPriorityCount);

        TMonitor.Enter(LThreads);
        try
          LThreads.AddOrSetValue(TThread.Current.ThreadID, True);
        finally
          TMonitor.Exit(LThreads);
        end;

        Sleep(2);
      end);

    Assert.IsTrue((LThreads.Count > 1) or (TFileScanWorkers.CoreCount = 1), 'the iterations must spread over threads');
    Assert.AreEqual(0, LWrongPriorityCount, 'TParallel.For iterations on a thread left at another priority');
  finally
    LThreads.Free;
  end;
end;

// Back-to-back scans must not pile up handles or memory - with no message loop in between, which is where OTL's
// Unobserved Parallel.For tasks were never released (~17 MB and 200 handles per scan, until out of memory).
procedure TParallelFileScannerTestsCustom.BackToBackScansPileUpNothing;
const
  MAX_ALLOCATED_GROWTH_MB = 16;
  MAX_HANDLE_GROWTH = 200;
  SCAN_COUNT = 40;
var
  LAllocatedBefore: NativeUInt;
  LAllocatedGrowthMB: Double;
  LExclusions: TFileScanExclusions;
  LHandlesAfter: Cardinal;
  LHandlesBefore: Cardinal;
  LScanner: TParallelFileScannerCustom;
begin
  LScanner := CreateScanner;
  try
    ScanToStringList(LScanner, FScanRoots, LExclusions); // warm-up: pools starting their threads is not a pile-up

    GetProcessHandleCount(GetCurrentProcess, LHandlesBefore);
    LAllocatedBefore := FastMM_GetUsageSummary.AllocatedBytes;

    for var LIndex := 1 to SCAN_COUNT do
      ScanToStringList(LScanner, FScanRoots, LExclusions);

    GetProcessHandleCount(GetCurrentProcess, LHandlesAfter);
    LAllocatedGrowthMB := (Int64(FastMM_GetUsageSummary.AllocatedBytes) - Int64(LAllocatedBefore)) / (1024 * 1024);
  finally
    LScanner.Free;
  end;

  Assert.IsTrue(Integer(LHandlesAfter) - Integer(LHandlesBefore) <= MAX_HANDLE_GROWTH,
    Format('%d scans: handle count grew by %d', [SCAN_COUNT, Integer(LHandlesAfter) - Integer(LHandlesBefore)]));
  Assert.IsTrue(LAllocatedGrowthMB <= MAX_ALLOCATED_GROWTH_MB,
    Format('%d scans: allocated memory grew by %.1f MB', [SCAN_COUNT, LAllocatedGrowthMB]));
end;

// A pattern ending in '*' prunes every folder it matches, wherever it is.
procedure TParallelFileScannerTestsCustom.PatternExcludesFolderAnywhere;
var
  LExclusions: TFileScanExclusions;
begin
  LExclusions.Patterns := ['*\3rdPartyLibraries\*'];

  AssertExclusionsLeave(LExclusions,
    BaselineWhere(
      function(AFile: string): Boolean
      begin
        Result := not AFile.Contains('\3rdpartylibraries\');
      end),
    'GetFileList excluding *\3rdPartyLibraries\*');
end;

// Quoted alternation: one pattern excludes folders of either name, at any depth, case-insensitively.
procedure TParallelFileScannerTestsCustom.QuotedAlternationExcludesEveryListedFolder;
var
  LExclusions: TFileScanExclusions;
begin
  LExclusions.Patterns := ['*\<"FastMM5"|"SPRING4D">\*'];

  AssertExclusionsLeave(LExclusions,
    BaselineWhere(
      function(AFile: string): Boolean
      begin
        Result := not (AFile.Contains('\fastmm5\') or AFile.Contains('\spring4d\'));
      end),
    'GetFileList excluding *\<"FastMM5"|"SPRING4D">\*');
end;

// A pattern that does not end in '*' cannot prune folders, but still excludes the files it matches.
procedure TParallelFileScannerTestsCustom.PatternWithoutTrailingStarExcludesFilesOnly;
var
  LExclusions: TFileScanExclusions;
begin
  LExclusions.Patterns := ['*.inc'];

  AssertExclusionsLeave(LExclusions,
    BaselineWhere(
      function(AFile: string): Boolean
      begin
        Result := not AFile.EndsWith('.inc');
      end),
    'GetFileList excluding *.inc');
end;

// '*Spring*' prunes every folder whose path contains "spring" and excludes every other file whose path does - which
// together must be exactly the files whose path contains it: pruning never drops more than the file check would.
procedure TParallelFileScannerTestsCustom.PruningPatternExcludesFoldersAndFiles;
var
  LExclusions: TFileScanExclusions;
begin
  LExclusions.Patterns := ['*Spring*'];

  AssertExclusionsLeave(LExclusions,
    BaselineWhere(
      function(AFile: string): Boolean
      begin
        Result := not AFile.Contains('spring');
      end),
    'GetFileList excluding *Spring*');
end;

// A pattern ending in a path delimiter names folders, as in .gitignore: '*\FastMM5\' means '*\FastMM5\*'.
procedure TParallelFileScannerTestsCustom.PatternEndingInDelimiterExcludesFolders;
var
  LExclusions: TFileScanExclusions;
begin
  LExclusions.Patterns := ['*\FastMM5\'];

  AssertExclusionsLeave(LExclusions,
    BaselineWhere(
      function(AFile: string): Boolean
      begin
        Result := not AFile.Contains('\fastmm5\');
      end),
    'GetFileList excluding *\FastMM5\');
end;

// '*\Units?' matches the folder path "...\Units\" ('?' matching the '\') but no file under it, so the folder must not
// be pruned: only patterns ending in '*' may prune.
procedure TParallelFileScannerTestsCustom.FolderPathMatchAloneDoesNotPrune;
var
  LExclusions: TFileScanExclusions;
begin
  LExclusions.Patterns := ['*\Units?'];

  AssertExclusionsLeave(LExclusions,
    BaselineWhere(
      function(AFile: string): Boolean
      begin
        Result := True;
      end),
    'GetFileList excluding *\Units?');
end;

// A malformed pattern would silently match nothing, so it must be rejected instead.
procedure TParallelFileScannerTestsCustom.MalformedExclusionPatternRaises;
var
  LExclusions: TFileScanExclusions;
  LScanner: TParallelFileScannerCustom;
begin
  LExclusions.Patterns := ['*\<3rdParty\*'];
  LScanner := CreateScanner;
  try
    Assert.WillRaise(
      procedure
      begin
        ScanToStringList(LScanner, FScanRoots, LExclusions);
      end,
      EInOutArgumentException, 'An unterminated < in an exclusion pattern must raise');
  finally
    LScanner.Free;
  end;
end;

// Search patterns other than '*.ext' are WildCardMatcher wildcards on the file name.
procedure TParallelFileScannerTestsCustom.WildcardSearchPatternMatchesFileNames;
var
  LExclusions: TFileScanExclusions;
  LExpected: TFileSet;
  LScanner: TParallelFileScannerCustom;
begin
  LExpected := BaselineWhere(
    function(AFile: string): Boolean
    begin
      var LName := ExtractFileName(AFile);

      Result := LName.EndsWith('.pas') and (LName.Contains('scanner') or LName.Contains('matcher'));
    end);
  try
    Assert.IsTrue(LExpected.Count > 0, 'The tree must hold files the pattern matches');

    LScanner := ScannerClass.Create(['*<"Scanner"|"Matcher">*.pas'], nil);
    try
      LScanner.ConvertRelativePathsToAbsolute := True;

      AssertSameFiles(LExpected, ScanToStringList(LScanner, FScanRoots, LExclusions),
        'GetFileList with *<"Scanner"|"Matcher">*.pas');
    finally
      LScanner.Free;
    end;
  finally
    LExpected.Free;
  end;
end;

// Search patterns match file names only; one with a path in it could never match, so it is rejected.
procedure TParallelFileScannerTestsCustom.SearchPatternWithPathDelimiterRaises;
var
  LExclusions: TFileScanExclusions;
  LScanner: TParallelFileScannerCustom;
begin
  LScanner := ScannerClass.Create(['Units\*.pas'], nil);
  try
    Assert.WillRaise(
      procedure
      begin
        ScanToStringList(LScanner, FScanRoots, LExclusions);
      end,
      EInOutArgumentException, 'A search pattern with a path delimiter must raise');
  finally
    LScanner.Free;
  end;
end;

// '...\3rdPartyLibraries\FastMM' names no folder (the folder is FastMM5), so it must exclude nothing - a prefix is a
// whole folder, not the start of any folder name.
procedure TParallelFileScannerTestsCustom.PrefixExcludesWholeFoldersOnly;
var
  LExclusions: TFileScanExclusions;
begin
  LExclusions.PathPrefixes := [TPath.Combine(FScanRoots[0], '3rdPartyLibraries\FastMM')];

  AssertExclusionsLeave(LExclusions,
    BaselineWhere(
      function(AFile: string): Boolean
      begin
        Result := True;
      end),
    'GetFileList excluding the prefix ...\3rdPartyLibraries\FastMM');
end;

// SkippedFilesCount covers the files the search patterns match inside pruned folders.
procedure TParallelFileScannerTestsCustom.SkippedFilesCountCountsPrunedFolders;
var
  LExclusions: TFileScanExclusions;
  LExpected: TFileSet;
  LScanner: TParallelFileScannerCustom;
begin
  LExclusions.Patterns := ['*\FastMM5\*'];
  LExpected := BaselineWhere(
    function(AFile: string): Boolean
    begin
      Result := AFile.Contains('\fastmm5\');
    end);
  try
    Assert.IsTrue(LExpected.Count > 0, 'The tree must hold files in FastMM5 folders');

    LScanner := CreateScanner;
    try
      ScanToStringList(LScanner, FScanRoots, LExclusions);

      Assert.AreEqual(LExpected.Count, LScanner.SkippedFilesCount, 'SkippedFilesCount');
    finally
      LScanner.Free;
    end;
  finally
    LExpected.Free;
  end;
end;

// Blank entries - e.g. empty lines of a settings memo - must exclude nothing. An empty prefix or suffix used to match,
// and so exclude, everything.
procedure TParallelFileScannerTestsCustom.BlankExclusionsAreIgnored;
var
  LExclusions: TFileScanExclusions;
begin
  LExclusions.PathPrefixes := [''];
  LExclusions.PathSuffixes := ['', ' '];
  LExclusions.Patterns := ['', '  '];

  AssertExclusionsLeave(LExclusions,
    BaselineWhere(
      function(AFile: string): Boolean
      begin
        Result := True;
      end),
    'GetFileList with blank exclusions');
end;

// '*.<"pas"|"inc">' starts like a plain '*.ext' pattern but is a wildcard: it must not be taken for the literal
// extension '.<"pas"|"inc">', which no file has.
procedure TParallelFileScannerTestsCustom.AlternationExtensionPatternIsAWildcard;
var
  LExclusions: TFileScanExclusions;
  LExpected: TFileSet;
  LScanner: TParallelFileScannerCustom;
begin
  LExpected := BaselineWhere(
    function(AFile: string): Boolean
    begin
      Result := AFile.EndsWith('.pas') or AFile.EndsWith('.inc');
    end);
  try
    Assert.IsTrue(LExpected.Count > 0, 'The tree must hold .pas and .inc files');

    LScanner := ScannerClass.Create(['*.<"pas"|"inc">'], nil);
    try
      LScanner.ConvertRelativePathsToAbsolute := True;

      AssertSameFiles(LExpected, ScanToStringList(LScanner, FScanRoots, LExclusions),
        'GetFileList with *.<"pas"|"inc">');
    finally
      LScanner.Free;
    end;
  finally
    LExpected.Free;
  end;
end;

// A scan on a scanner that is already scanning - here from its own callback - would replace the running walk's state
// under its workers, so it must be refused; the running scan must finish normally, and the scanner work afterwards.
procedure TParallelFileScannerTestsCustom.ScanOnBusyScannerRaises;
var
  LExclusions: TFileScanExclusions;
  LFileCount: Integer;
  LNestedOutcome: string;
  LNestedStarted: Integer;
  LScanner: TParallelFileScannerCustom;
begin
  LScanner := CreateScanner;
  try
    LFileCount := 0;
    LNestedStarted := 0;
    LNestedOutcome := '';

    LScanner.ScanFiles(FScanRoots, LExclusions,
      procedure(const AFileName: string)
      begin
        if AtomicCmpExchange(LNestedStarted, 1, 0) = 0 then
        try
          ScanToStringList(LScanner, FScanRoots, LExclusions);
          LNestedOutcome := 'no exception';
        except
          on E: Exception do
            LNestedOutcome := E.ClassName;
        end;
      end,
      LFileCount);

    Assert.AreEqual(EInvalidOpException.ClassName, LNestedOutcome, 'A scan on a busy scanner');
    Assert.AreEqual(FBaseline.Count, LFileCount, 'Files of the running scan');
    AssertSameFiles(FBaseline, ScanToStringList(LScanner, FScanRoots, LExclusions), 'GetFileList afterwards');
  finally
    LScanner.Free;
  end;
end;

// Root\Sub\LoopBack is a junction back to Root - a cycle - and Root\Linked one to a folder outside the tree. Neither
// is followed by default, by the walk or by SkippedFilesCount; with FollowDirectoryLinks both are.
procedure TParallelFileScannerTestsCustom.DirectoryLinksAreNotFollowed;
var
  LBase: string;
  LExclusions: TFileScanExclusions;
  LExpected: TFileSet;
  LLinked: string;
  LLoopBack: string;
  LRoot: string;
  LScanner: TParallelFileScannerCustom;
  LTarget: string;
begin
  LBase := TPath.Combine(TPath.GetTempPath, Format('DPFSLinkTest_%d_%d', [GetCurrentProcessId, GetTickCount64]));
  LRoot := TPath.Combine(LBase, 'Root');
  LTarget := TPath.Combine(LBase, 'Target');
  LLoopBack := TPath.Combine(LRoot, 'Sub\LoopBack');
  LLinked := TPath.Combine(LRoot, 'Linked');

  TDirectory.CreateDirectory(TPath.Combine(LRoot, 'Sub'));
  TDirectory.CreateDirectory(LTarget);
  try
    TFile.WriteAllText(TPath.Combine(LRoot, 'Sub\Unit1.pas'), 'unit Unit1;');
    TFile.WriteAllText(TPath.Combine(LTarget, 'Unit2.pas'), 'unit Unit2;');
    CreateDirectoryJunction(LLoopBack, LRoot);
    CreateDirectoryJunction(LLinked, LTarget);

    LScanner := CreateScanner;
    try
      LExpected := TFileSet.Create([TPath.Combine(LRoot, 'Sub\Unit1.pas')]);
      try
        AssertSameFiles(LExpected, ScanToStringList(LScanner, [LRoot], LExclusions), 'GetFileList over junctions');
      finally
        LExpected.Free;
      end;

      LExclusions.PathPrefixes := [TPath.Combine(LRoot, 'Sub')];
      ScanToStringList(LScanner, [LRoot], LExclusions);
      Assert.AreEqual(1, LScanner.SkippedFilesCount, 'SkippedFilesCount of a pruned folder holding a junction loop');

      // Followed, with the loop pruned by a pattern.
      LScanner.FollowDirectoryLinks := True;
      LExclusions.PathPrefixes := [];
      LExclusions.Patterns := ['*\LoopBack\'];
      LExpected := TFileSet.Create([TPath.Combine(LRoot, 'Sub\Unit1.pas'), TPath.Combine(LLinked, 'Unit2.pas')]);
      try
        AssertSameFiles(LExpected, ScanToStringList(LScanner, [LRoot], LExclusions),
          'GetFileList with FollowDirectoryLinks');
      finally
        LExpected.Free;
      end;
    finally
      LScanner.Free;
    end;
  finally
    // The junctions themselves first, so deleting the tree cannot reach through them.
    RemoveDirectory(PChar(LLoopBack));
    RemoveDirectory(PChar(LLinked));
    TDirectory.Delete(LBase, True);
  end;
end;

// Roots Source and Source\3rdPartyLibraries\FastMM5 with 3rdPartyLibraries excluded: the nested root's files are all
// returned, so SkippedFilesCount counts every other file under 3rdPartyLibraries exactly once - whatever order the
// workers met the folders in, so the scan is repeated.
procedure TParallelFileScannerTestsCustom.SkippedFilesCountCountsNestedRootFilesOnce;
const
  SCAN_COUNT = 5;
var
  LExcludedPrefix: string;
  LExclusions: TFileScanExclusions;
  LExpectedCount: Integer;
  LNestedRoot: string;
  LScanner: TParallelFileScannerCustom;
begin
  LExcludedPrefix := IncludeTrailingPathDelimiter(TPath.GetFullPath(TPath.Combine(FScanRoots[0], '3rdPartyLibraries')));
  LNestedRoot := IncludeTrailingPathDelimiter(TPath.Combine(LExcludedPrefix, 'FastMM5'));
  LExpectedCount := 0;

  for var LIndex := 0 to FBaseline.Count - 1 do
    if FBaseline.Files[LIndex].StartsWith(LExcludedPrefix.ToLower)
      and not FBaseline.Files[LIndex].StartsWith(LNestedRoot.ToLower) then
      Inc(LExpectedCount);

  Assert.IsTrue(LExpectedCount > 0, 'The tree must hold files under 3rdPartyLibraries');
  LExclusions.PathPrefixes := [LExcludedPrefix];

  for var LScan := 1 to SCAN_COUNT do
  begin
    LScanner := CreateScanner;
    try
      ScanToStringList(LScanner, [FScanRoots[0], LNestedRoot], LExclusions);

      Assert.AreEqual(LExpectedCount, LScanner.SkippedFilesCount, Format('SkippedFilesCount, scan %d', [LScan]));
    finally
      LScanner.Free;
    end;
  end;
end;

// The walk builds its paths with '\', so '/' in roots, prefixes and patterns must be read as '\' - or they could never
// match, and would silently exclude nothing.
procedure TParallelFileScannerTestsCustom.ForwardSlashesAreReadAsBackslashes;
var
  LExcludedPrefix: string;
  LExclusions: TFileScanExclusions;
  LExpected: TFileSet;
  LFiles: TArray<string>;
  LScanner: TParallelFileScannerCustom;
begin
  LExcludedPrefix := TPath.GetFullPath(TPath.Combine(FScanRoots[0], 'Units'));
  LExclusions.PathPrefixes := [LExcludedPrefix.Replace('\', '/')];
  LExclusions.Patterns := ['*/FastMM5/'];

  LExpected := BaselineWhere(
    function(AFile: string): Boolean
    begin
      Result := not (AFile.Contains('\fastmm5\') or AFile.StartsWith(LExcludedPrefix.ToLower + '\'));
    end);
  try
    LScanner := CreateScanner;
    try
      LScanner.ConvertRelativePathsToAbsolute := False; // the root is used as given
      LFiles := ScanToStringList(LScanner, [TPath.GetFullPath(FScanRoots[0]).Replace('\', '/')], LExclusions);

      for var LFile in LFiles do
        Assert.IsFalse(LFile.Contains('/'), 'A path with a ''/'': ' + LFile);

      AssertSameFiles(LExpected, LFiles, 'GetFileList with ''/'' in the root and the exclusions');
    finally
      LScanner.Free;
    end;
  finally
    LExpected.Free;
  end;
end;

// With Workers.Count N a scan runs at most N workers - so its files arrive on at most N threads - and still finds every
// file: with 1 worker and with 2.
procedure TParallelFileScannerTestsCustom.WorkersCountLimitsWorkerThreads;
var
  LExclusions: TFileScanExclusions;
  LFileCount: Integer;
  LFiles: TStringList;
  LScanner: TParallelFileScannerCustom;
  LThreadIds: TList<Cardinal>;
  LWorkers: TFileScanWorkers;
begin
  LFiles := TStringList.Create;
  LThreadIds := TList<Cardinal>.Create;
  try
    for var LCount := 1 to 2 do
    begin
      LFiles.Clear;
      LThreadIds.Clear;
      LWorkers := TFileScanWorkers.Create;
      LWorkers.Initialize(LCount, LCount);
      LScanner := ScannerClass.Create(TScanTree.Extensions, LWorkers);
      try
        LScanner.ConvertRelativePathsToAbsolute := True;
        LFileCount := 0;

        LScanner.ScanFiles(FScanRoots, LExclusions,
          procedure(const AFileName: string)
          begin
            TMonitor.Enter(LFiles);
            try
              LFiles.Add(AFileName);

              if not LThreadIds.Contains(GetCurrentThreadId) then
                LThreadIds.Add(GetCurrentThreadId);
            finally
              TMonitor.Exit(LFiles);
            end;
          end,
          LFileCount);
      finally
        LScanner.Free;
      end;

      Assert.IsTrue(LThreadIds.Count <= LCount, Format('%d worker(s): files arrived on %d threads',
        [LCount, LThreadIds.Count]));
      AssertSameFiles(FBaseline, LFiles.ToStringArray, Format('ScanFiles with %d worker(s)', [LCount]));
    end;
  finally
    LThreadIds.Free;
    LFiles.Free;
  end;
end;

// More workers than cores, e.g. for slow network drives: the scan must still find every file (TParallelFileScannerOTL
// grows its pool for them).
procedure TParallelFileScannerTestsCustom.MoreWorkersThanCoresScanCorrectly;
var
  LExclusions: TFileScanExclusions;
  LScanner: TParallelFileScannerCustom;
  LWorkers: TFileScanWorkers;
begin
  LWorkers := TFileScanWorkers.Create;
  LWorkers.Initialize(TFileScanWorkers.CoreCount + 4, TFileScanWorkers.CoreCount + 4);
  LScanner := ScannerClass.Create(TScanTree.Extensions, LWorkers);
  try
    LScanner.ConvertRelativePathsToAbsolute := True;

    AssertSameFiles(FBaseline, ScanToStringList(LScanner, FScanRoots, LExclusions),
      Format('GetFileList with %d workers', [LWorkers.Count]));
  finally
    LScanner.Free;
  end;
end;

// SortResultList False: the workers' lists are concatenated instead of merged - the same files, each once.
procedure TParallelFileScannerTestsCustom.UnsortedResultHoldsEveryFile;
var
  LExclusions: TFileScanExclusions;
  LScanner: TParallelFileScannerCustom;
begin
  LScanner := CreateScanner(False);
  try
    AssertSameFiles(FBaseline, ScanToStringList(LScanner, FScanRoots, LExclusions), 'GetFileList, SortResultList False');
  finally
    LScanner.Free;
  end;
end;

// Items already in the list stay, and with SortResultList the whole list - theirs and the scan's - ends up in
// CompareText order (the full-sort path, not the merge).
procedure TParallelFileScannerTestsCustom.AppendingKeepsEarlierItemsAndSortsAll;
var
  LExclusions: TFileScanExclusions;
  LFiles: TStringList;
  LScanner: TParallelFileScannerCustom;
begin
  LScanner := CreateScanner;
  LFiles := TStringList.Create;
  try
    LFiles.Add('ZZZ earlier item');
    LFiles.Add('AAA earlier item');

    Assert.IsTrue(LScanner.GetFileList(FScanRoots, LExclusions, LFiles), 'GetFileList found nothing');

    Assert.AreEqual(FBaseline.Count + 2, LFiles.Count, 'Earlier items plus the files');
    Assert.AreEqual('AAA earlier item', LFiles[0], 'First item');
    Assert.AreEqual('ZZZ earlier item', LFiles[LFiles.Count - 1], 'Last item');

    for var LIndex := 1 to LFiles.Count - 1 do
      Assert.IsTrue(CompareText(LFiles[LIndex - 1], LFiles[LIndex]) <= 0,
        Format('Items %d and %d out of CompareText order', [LIndex - 1, LIndex]));

    LFiles.Delete(LFiles.Count - 1);
    LFiles.Delete(0);
    AssertSameFiles(FBaseline, LFiles.ToStringArray, 'GetFileList into a non-empty list');
  finally
    LFiles.Free;
    LScanner.Free;
  end;
end;

// A Sorted TStringList keeps its own order (AnsiCompareText); the scanner must neither sort it again nor break it.
procedure TParallelFileScannerTestsCustom.SortedStringListKeepsItsOwnOrder;
var
  LExclusions: TFileScanExclusions;
  LFiles: TStringList;
  LScanner: TParallelFileScannerCustom;
begin
  LScanner := CreateScanner;
  LFiles := TStringList.Create;
  try
    LFiles.Sorted := True;

    Assert.IsTrue(LScanner.GetFileList(FScanRoots, LExclusions, LFiles), 'GetFileList found nothing');

    for var LIndex := 1 to LFiles.Count - 1 do
      Assert.IsTrue(AnsiCompareText(LFiles[LIndex - 1], LFiles[LIndex]) <= 0,
        Format('Items %d and %d out of the sorted list''s order', [LIndex - 1, LIndex]));

    AssertSameFiles(FBaseline, LFiles.ToStringArray, 'GetFileList into a Sorted list');
  finally
    LFiles.Free;
    LScanner.Free;
  end;
end;

procedure TParallelFileScannerTestsCustom.StringListRootsOverloadMatchesBaseline;
var
  LExclusions: TFileScanExclusions;
  LFiles: TStringList;
  LRoots: TStringList;
  LScanner: TParallelFileScannerCustom;
begin
  LScanner := CreateScanner;
  LRoots := TStringList.Create;
  LFiles := TStringList.Create;
  try
    LRoots.AddStrings(FScanRoots);

    Assert.IsTrue(LScanner.GetFileList(LRoots, LExclusions, LFiles), 'GetFileList found nothing');
    AssertSameFiles(FBaseline, LFiles.ToStringArray, 'GetFileList with TStringList roots');
  finally
    LFiles.Free;
    LRoots.Free;
    LScanner.Free;
  end;
end;

// Suffixes are compared case-insensitively against the full path, and the files they exclude are counted as skipped.
procedure TParallelFileScannerTestsCustom.PathSuffixesExcludeFilesCaseInsensitively;
var
  LExclusions: TFileScanExclusions;
  LExpected: TFileSet;
  LExpectedSkipped: Integer;
  LScanner: TParallelFileScannerCustom;
begin
  LExclusions.PathSuffixes := ['.INC', 'Scanner.PAS'];

  LExpected := BaselineWhere(
    function(AFile: string): Boolean
    begin
      Result := not (AFile.EndsWith('.inc') or AFile.EndsWith('scanner.pas'));
    end);
  try
    LExpectedSkipped := FBaseline.Count - LExpected.Count;
    Assert.IsTrue(LExpectedSkipped > 0, 'The tree must hold files the suffixes exclude');

    LScanner := CreateScanner;
    try
      AssertSameFiles(LExpected, ScanToStringList(LScanner, FScanRoots, LExclusions), 'GetFileList excluding suffixes');
      Assert.AreEqual(LExpectedSkipped, LScanner.SkippedFilesCount, 'SkippedFilesCount');
    finally
      LScanner.Free;
    end;
  finally
    LExpected.Free;
  end;
end;

// A root that does not exist, or that is a file, is skipped; the other roots are scanned as usual.
procedure TParallelFileScannerTestsCustom.MissingAndFileRootsAreIgnored;
var
  LExclusions: TFileScanExclusions;
  LScanner: TParallelFileScannerCustom;
begin
  LScanner := CreateScanner;
  try
    AssertSameFiles(FBaseline,
      ScanToStringList(LScanner, [TPath.Combine(FScanRoots[0], 'NoSuchFolder'),
        TPath.Combine(FScanRoots[0], 'Units\DPFSUnit.Parallel.FileScanner.pas'), FScanRoots[0]], LExclusions),
      'GetFileList with a missing and a file root');
  finally
    LScanner.Free;
  end;
end;

// No usable root: every API says "nothing found" - no exception, no callback, nothing skipped.
procedure TParallelFileScannerTestsCustom.NoRootsFindNothing;
var
  LCallbacks: Integer;
  LExclusions: TFileScanExclusions;
  LFileCount: Integer;
  LFiles: TStringList;
  LRootSets: TArray<TArray<string>>;
  LScanner: TParallelFileScannerCustom;
begin
  SetLength(LRootSets, 2);
  LRootSets[0] := [];
  LRootSets[1] := [TPath.Combine(FScanRoots[0], 'NoSuchFolder')];

  LScanner := CreateScanner;
  LFiles := TStringList.Create;
  try
    for var LRoots in LRootSets do
    begin
      LFiles.Clear;
      Assert.IsFalse(LScanner.GetFileList(LRoots, LExclusions, LFiles), 'GetFileList result');
      Assert.AreEqual(0, LFiles.Count, 'GetFileList files');

      LCallbacks := 0;
      LFileCount := -1;
      Assert.IsFalse(LScanner.ScanFiles(LRoots, LExclusions,
        procedure(const AFileName: string)
        begin
          AtomicIncrement(LCallbacks);
        end,
        LFileCount), 'ScanFiles result');
      Assert.AreEqual(0, LFileCount, 'ScanFiles AFileCount');
      Assert.AreEqual(0, LCallbacks, 'ScanFiles callbacks');
      Assert.AreEqual(0, LScanner.SkippedFilesCount, 'SkippedFilesCount');
    end;
  finally
    LFiles.Free;
    LScanner.Free;
  end;
end;

// Empty and malformed search patterns raise when the scan starts - every time: the failed scan must not leave the
// scanner marked as busy.
procedure TParallelFileScannerTestsCustom.InvalidSearchPatternsRaise;
var
  LExclusions: TFileScanExclusions;
  LScanner: TParallelFileScannerCustom;
begin
  for var LPattern in TArray<string>.Create('', '   ', '*.<pas', 'Unit<"a"|"b') do
  begin
    LScanner := ScannerClass.Create(['*.pas', LPattern], nil);
    try
      for var LAttempt := 1 to 2 do
        Assert.WillRaise(
          procedure
          begin
            ScanToStringList(LScanner, FScanRoots, LExclusions);
          end,
          EInOutArgumentException, Format('Search pattern ''%s'', attempt %d', [LPattern, LAttempt]));
    finally
      LScanner.Free;
    end;
  end;
end;

// A relative root is resolved against the current directory: to absolute paths with ConvertRelativePathsToAbsolute,
// left relative without it.
procedure TParallelFileScannerTestsCustom.RelativeRootsFollowConvertSetting;
var
  LExclusions: TFileScanExclusions;
  LFiles: TArray<string>;
  LSavedDirectory: string;
  LScanner: TParallelFileScannerCustom;
begin
  LSavedDirectory := GetCurrentDir;
  LScanner := CreateScanner;
  try
    Assert.IsTrue(SetCurrentDir(ExtractFileDir(ExcludeTrailingPathDelimiter(FScanRoots[0]))), 'SetCurrentDir');

    LFiles := ScanToStringList(LScanner, ['Source'], LExclusions);
    for var LFile in LFiles do
      Assert.IsTrue(TPath.IsPathRooted(LFile) and not LFile.StartsWith('Source\'), 'Not absolute: ' + LFile);
    AssertSameFiles(FBaseline, LFiles, 'GetFileList, relative root, ConvertRelativePathsToAbsolute');

    LScanner.ConvertRelativePathsToAbsolute := False;
    LFiles := ScanToStringList(LScanner, ['Source'], LExclusions);
    for var LFile in LFiles do
      Assert.IsTrue(LFile.StartsWith('Source\'), 'Not relative: ' + LFile);
    AssertSameFiles(FBaseline, LFiles, 'GetFileList, relative root as given');
  finally
    SetCurrentDir(LSavedDirectory);
    LScanner.Free;
  end;
end;

// Every scan starts from zero: what an earlier scan skipped does not carry over - whether or not it was read.
procedure TParallelFileScannerTestsCustom.CountersAreResetBetweenScans;
var
  LExclusions: TFileScanExclusions;
  LNoExclusions: TFileScanExclusions;
  LScanner: TParallelFileScannerCustom;
begin
  LExclusions.Patterns := ['*\FastMM5\*'];
  LExclusions.PathSuffixes := ['.inc'];
  LScanner := CreateScanner;
  try
    ScanToStringList(LScanner, FScanRoots, LExclusions);
    Assert.IsTrue(LScanner.SkippedFilesCount > 0, 'SkippedFilesCount after a scan with exclusions');
    Assert.IsTrue(LScanner.DiskScanTimeForFiles > 0, 'DiskScanTimeForFiles');

    ScanToStringList(LScanner, FScanRoots, LNoExclusions);
    Assert.AreEqual(0, LScanner.SkippedFilesCount, 'SkippedFilesCount after a scan without exclusions');

    // The same, without reading the count in between.
    ScanToStringList(LScanner, FScanRoots, LExclusions);
    ScanToStringList(LScanner, FScanRoots, LNoExclusions);
    Assert.AreEqual(0, LScanner.SkippedFilesCount, 'SkippedFilesCount, not read after the first scan');
  finally
    LScanner.Free;
  end;
end;

// '*.pas' is the extension '.pas', case-insensitively: not a longer one, not a name merely ending in "pas", and a name
// that is only the extension counts.
procedure TParallelFileScannerTestsCustom.ExtensionPatternMatchesWholeExtensionOnly;
var
  LExclusions: TFileScanExclusions;
  LExpected: TFileSet;
  LScanner: TParallelFileScannerCustom;
  LTree: TTempTree;
begin
  LTree := TTempTree.Create(['a.pas', 'B.PAS', 'c.pas.bak', 'd.b.pas', '.pas', 'noext', 'e.pa', 'f.xpas', 'fpas',
    'Sub\g.Pas', 'Sub.pas\h.txt']);
  try
    LExpected := LTree.FileSet(['a.pas', 'B.PAS', 'd.b.pas', '.pas', 'Sub\g.Pas']);
    try
      LScanner := ScannerClass.Create(['*.pas'], nil);
      try
        LScanner.ConvertRelativePathsToAbsolute := True;

        AssertSameFiles(LExpected, ScanToStringList(LScanner, [LTree.Root], LExclusions), 'GetFileList with *.pas');
      finally
        LScanner.Free;
      end;
    finally
      LExpected.Free;
    end;
  finally
    LTree.Free;
  end;
end;

// Wildcard search patterns (the matcher, not the '*.ext' fast path) compare case-insensitively too.
procedure TParallelFileScannerTestsCustom.WildcardSearchPatternsAreCaseInsensitive;

  procedure AssertPatternFinds(const ATree: TTempTree; const APattern: string; const AExpected: array of string);
  var
    LExclusions: TFileScanExclusions;
    LExpected: TFileSet;
    LScanner: TParallelFileScannerCustom;
  begin
    LExpected := ATree.FileSet(AExpected);
    try
      LScanner := ScannerClass.Create([APattern], nil);
      try
        LScanner.ConvertRelativePathsToAbsolute := True;

        AssertSameFiles(LExpected, ScanToStringList(LScanner, [ATree.Root], LExclusions), 'GetFileList with ' + APattern);
      finally
        LScanner.Free;
      end;
    finally
      LExpected.Free;
    end;
  end;

var
  LTree: TTempTree;
begin
  LTree := TTempTree.Create(['unit1.pas', 'UNIT2.PAS', 'Unit12.pas', 'unitx.pas', 'xunit1.pas', 'unit1.dfm']);
  try
    AssertPatternFinds(LTree, 'UNIT<#>.PAS', ['unit1.pas', 'UNIT2.PAS']);
    AssertPatternFinds(LTree, 'unit?.pas', ['unit1.pas', 'UNIT2.PAS', 'unitx.pas']);
    AssertPatternFinds(LTree, 'Unit1.*', ['unit1.pas', 'unit1.dfm']);
  finally
    LTree.Free;
  end;
end;

// Letters outside ASCII fold case too - the CompareStringOrdinal path - in search patterns and path prefixes.
procedure TParallelFileScannerTestsCustom.NonAsciiNamesCompareCaseInsensitively;
var
  LExclusions: TFileScanExclusions;
  LExpected: TFileSet;
  LScanner: TParallelFileScannerCustom;
  LTree: TTempTree;
begin
  LTree := TTempTree.Create(['Äiti\Öljy\x.pas', 'Äiti\y.pas', 'Muu\z.PÄS', 'Muu\w.päs', 'Muu\v.pås']);
  try
    LExpected := LTree.FileSet(['Äiti\y.pas', 'Muu\z.PÄS', 'Muu\w.päs']);
    try
      LScanner := ScannerClass.Create(['*.pas', '*.PäS'], nil);
      try
        LScanner.ConvertRelativePathsToAbsolute := True;
        LExclusions.PathPrefixes := [LTree.Path('äITI\öLJY')];

        AssertSameFiles(LExpected, ScanToStringList(LScanner, [LTree.Root], LExclusions), 'GetFileList');
        Assert.AreEqual(1, LScanner.SkippedFilesCount, 'SkippedFilesCount');
      finally
        LScanner.Free;
      end;
    finally
      LExpected.Free;
    end;
  finally
    LTree.Free;
  end;
end;

// One subtree far bigger than the rest, plus a deep chain, walked by 1, 3 and 8 workers - repeatedly, as the hand-offs
// between workers differ from run to run: every file exactly once.
procedure TParallelFileScannerTestsCustom.DominantSubtreeIsWalkedCompletely;
const
  CHAIN_DEPTH = 30;
  RUN_COUNT = 3;
var
  LChain: string;
  LExclusions: TFileScanExclusions;
  LExpected: TFileSet;
  LFiles: TList<string>;
  LScanner: TParallelFileScannerCustom;
  LTree: TTempTree;
  LWorkers: TFileScanWorkers;
begin
  LFiles := TList<string>.Create;
  try
    LFiles.Add('Small\s.pas');

    for var LDirectory := 0 to 11 do
      for var LSubDirectory := 0 to 9 do
        for var LFile := 0 to 2 do
          LFiles.Add(Format('Big\D%d\S%d\f%d.pas', [LDirectory, LSubDirectory, LFile]));

    LChain := 'Deep';
    for var LLevel := 1 to CHAIN_DEPTH do
    begin
      LChain := LChain + '\' + LLevel.ToString;
      LFiles.Add(LChain + '\l.pas');
    end;

    LTree := TTempTree.Create(LFiles.ToArray);
    try
      LTree.Add('Empty\');
      LTree.Add('Big\D0\NoMatches\readme.txt');
      LExpected := LTree.FileSet(LFiles.ToArray);
      try
        for var LWorkerCount in TArray<Integer>.Create(1, 3, 8) do
        begin
          LWorkers := TFileScanWorkers.Create;
          LWorkers.Initialize(LWorkerCount, LWorkerCount);
          LScanner := ScannerClass.Create(TScanTree.Extensions, LWorkers);
          try
            LScanner.ConvertRelativePathsToAbsolute := True;

            for var LRun := 1 to RUN_COUNT do
              AssertSameFiles(LExpected, ScanToStringList(LScanner, [LTree.Root], LExclusions),
                Format('GetFileList, %d worker(s), run %d', [LWorkerCount, LRun]));
          finally
            LScanner.Free;
          end;
        end;
      finally
        LExpected.Free;
      end;
    finally
      LTree.Free;
    end;
  finally
    LFiles.Free;
  end;
end;

// Folders, but no file the search patterns match: "nothing found" from every API.
procedure TParallelFileScannerTestsCustom.TreeWithoutMatchesFindsNothing;
var
  LExclusions: TFileScanExclusions;
  LFileCount: Integer;
  LScanner: TParallelFileScannerCustom;
  LTree: TTempTree;
begin
  LTree := TTempTree.Create(['a.txt', 'Sub\', 'Sub\Deeper\', 'Sub\b.pas.txt', 'Sub\Deeper\pas']);
  try
    LScanner := CreateScanner;
    try
      Assert.AreEqual(0, Integer(Length(ScanToStringList(LScanner, [LTree.Root], LExclusions))), 'GetFileList files');

      LFileCount := -1;
      Assert.IsFalse(LScanner.ScanFiles([LTree.Root], LExclusions,
        procedure(const AFileName: string)
        begin
        end,
        LFileCount), 'ScanFiles result');
      Assert.AreEqual(0, LFileCount, 'ScanFiles AFileCount');
    finally
      LScanner.Free;
    end;
  finally
    LTree.Free;
  end;
end;

// Relative prefixes and patterns, from the tree's root as the current directory. With ConvertRelativePathsToAbsolute
// they are resolved like the root - '..' included - so they exclude what they name. Without it they are compared as
// written, so only the ones written like the root ('A\...') match.
procedure TParallelFileScannerTestsCustom.RelativeExclusionsFollowConvertSetting;
var
  LExclusions: TFileScanExclusions;
  LExpected: TFileSet;
  LSavedDirectory: string;
  LScanner: TParallelFileScannerCustom;
  LTree: TTempTree;
begin
  LTree := TTempTree.Create(['A\keep.pas', 'A\Skip\s.pas', 'A\Pat\p.pas', 'A\Pat\Sub\q.pas', 'A\Up\u.pas',
    'A\Wild\w.pas', 'A\Files\f.pas', 'A\Files\g.pas']);
  try
    LSavedDirectory := GetCurrentDir;
    LScanner := CreateScanner;
    try
      Assert.IsTrue(SetCurrentDir(LTree.Root), 'SetCurrentDir');
      LExclusions.PathPrefixes := ['A\Skip'];
      LExclusions.Patterns := ['A\Pat\*', '..\' + ExtractFileName(LTree.Root) + '\A\Up\', 'A\W*\*', 'A\Files\f.pas'];

      LExpected := LTree.FileSet(['A\keep.pas', 'A\Files\g.pas']);
      try
        AssertSameFiles(LExpected, ScanToStringList(LScanner, ['A'], LExclusions),
          'GetFileList, relative exclusions, ConvertRelativePathsToAbsolute');
        Assert.AreEqual(6, LScanner.SkippedFilesCount, 'SkippedFilesCount, ConvertRelativePathsToAbsolute');
      finally
        LExpected.Free;
      end;

      LScanner.ConvertRelativePathsToAbsolute := False;
      LExpected := LTree.FileSet(['A\keep.pas', 'A\Up\u.pas', 'A\Files\g.pas']);
      try
        AssertSameFiles(LExpected, ScanToStringList(LScanner, ['A'], LExclusions), 'GetFileList, everything as written');
        Assert.AreEqual(5, LScanner.SkippedFilesCount, 'SkippedFilesCount, everything as written');
      finally
        LExpected.Free;
      end;
    finally
      SetCurrentDir(LSavedDirectory);
      LScanner.Free;
    end;
  finally
    LTree.Free;
  end;
end;

// Folders deeper than MAX_PATH characters: walked, counted when pruned, and usable as a root - absolute or relative.
procedure TParallelFileScannerTestsCustom.PathsLongerThanMaxPathAreWalked;
const
  LEVEL_COUNT = 8;
var
  LDeep: string;
  LExclusions: TFileScanExclusions;
  LExpected: TFileSet;
  LMiddle: string;
  LNoExclusions: TFileScanExclusions;
  LSavedDirectory: string;
  LScanner: TParallelFileScannerCustom;
  LTree: TTempTree;
begin
  LDeep := '';
  for var LLevel := 1 to LEVEL_COUNT do
  begin
    LDeep := LDeep + Format('Level%d_%s\', [LLevel, StringOfChar('x', 40)]);

    if LLevel = 3 then
      LMiddle := LDeep;
  end;

  LTree := TTempTree.Create([LMiddle + 'middle.pas', LDeep + 'deep.pas', LDeep + 'Pruned\a.pas', LDeep + 'Pruned\b.pas']);
  try
    Assert.IsTrue(Length(LTree.Path(LDeep)) > MAX_PATH, 'The deep folder must be longer than MAX_PATH');
    LSavedDirectory := GetCurrentDir;
    LScanner := CreateScanner;
    try
      LExclusions.PathPrefixes := [LTree.Path(LDeep + 'Pruned')];
      LExpected := LTree.FileSet([LMiddle + 'middle.pas', LDeep + 'deep.pas']);
      try
        AssertSameFiles(LExpected, ScanToStringList(LScanner, [LTree.Root], LExclusions), 'GetFileList, long paths');
        Assert.AreEqual(2, LScanner.SkippedFilesCount, 'SkippedFilesCount of a long pruned folder');
      finally
        LExpected.Free;
      end;

      LExpected := LTree.FileSet([LDeep + 'deep.pas', LDeep + 'Pruned\a.pas', LDeep + 'Pruned\b.pas']);
      try
        AssertSameFiles(LExpected, ScanToStringList(LScanner, [LTree.Path(LDeep)], LNoExclusions),
          'GetFileList, a root longer than MAX_PATH');
      finally
        LExpected.Free;
      end;

      Assert.IsTrue(SetCurrentDir(LTree.Root), 'SetCurrentDir');
      LScanner.ConvertRelativePathsToAbsolute := False;
      LExpected := LTree.FileSet([LMiddle + 'middle.pas', LDeep + 'deep.pas', LDeep + 'Pruned\a.pas',
        LDeep + 'Pruned\b.pas']);
      try
        AssertSameFiles(LExpected, ScanToStringList(LScanner, ['.'], LNoExclusions),
          'GetFileList, relative root, long paths');
      finally
        LExpected.Free;
      end;
    finally
      SetCurrentDir(LSavedDirectory);
      LScanner.Free;
    end;
  finally
    LTree.Free;
  end;
end;

// The files in pruned folders are counted when SkippedFilesCount is read: a relative root still means the folder it
// meant during the scan, also after the current directory changed - here to a folder holding a look-alike tree.
procedure TParallelFileScannerTestsCustom.SkippedFilesCountUsesTheScansCurrentDirectory;
var
  LExclusions: TFileScanExclusions;
  LSavedDirectory: string;
  LScanner: TParallelFileScannerCustom;
  LTree: TTempTree;
begin
  LTree := TTempTree.Create(['A\keep.pas', 'A\Skip\s1.pas', 'A\Skip\Sub\s2.pas', 'Other\A\Skip\o1.pas',
    'Other\A\Skip\o2.pas', 'Other\A\Skip\o3.pas']);
  try
    LSavedDirectory := GetCurrentDir;
    LScanner := CreateScanner;
    try
      LScanner.ConvertRelativePathsToAbsolute := False;
      LExclusions.PathPrefixes := ['A\Skip'];
      Assert.IsTrue(SetCurrentDir(LTree.Root), 'SetCurrentDir');

      Assert.AreEqual(1, Integer(Length(ScanToStringList(LScanner, ['A'], LExclusions))), 'Files found');

      Assert.IsTrue(SetCurrentDir(LTree.Path('Other')), 'SetCurrentDir to the look-alike');
      Assert.AreEqual(2, LScanner.SkippedFilesCount, 'SkippedFilesCount after the current directory changed');
    finally
      SetCurrentDir(LSavedDirectory);
      LScanner.Free;
    end;
  finally
    LTree.Free;
  end;
end;

// A root inside an excluded folder was asked for, so the exclusions that cover it do not exclude it - whether 3rdParty
// is excluded by prefix or by pattern. The other exclusions still apply inside it, and a covering pattern still
// applies below it: '*\3rdParty\*' excludes OurFork\Lib\3rdParty, while the prefix, one fixed folder, does not.
// SkippedFilesCount counts each skipped file once, and nested roots produce no file twice.
procedure TParallelFileScannerTestsCustom.RootInsideExcludedFolderIsScannedWhole;
const
  FORK = 'App\3rdParty\OurFork\';
var
  LTree: TTempTree;

  procedure AssertScan(const ARoots, APrefixes, APatterns, AExpected: array of string; const AExpectedSkipped: Integer;
    const AWhat: string);
  var
    LExclusions: TFileScanExclusions;
    LExpected: TFileSet;
    LRoots: TArray<string>;
    LScanner: TParallelFileScannerCustom;
  begin
    LRoots := [];
    for var LRoot in ARoots do
      LRoots := LRoots + [LTree.Path(LRoot)];

    LExclusions.PathPrefixes := [];
    for var LPrefix in APrefixes do
      LExclusions.PathPrefixes := LExclusions.PathPrefixes + [LTree.Path(LPrefix)];

    LExclusions.Patterns := [];
    for var LPattern in APatterns do
      LExclusions.Patterns := LExclusions.Patterns + [LPattern];

    LExpected := LTree.FileSet(AExpected);
    try
      LScanner := CreateScanner;
      try
        AssertSameFiles(LExpected, ScanToStringList(LScanner, LRoots, LExclusions), AWhat);
        Assert.AreEqual(AExpectedSkipped, LScanner.SkippedFilesCount, AWhat + ': SkippedFilesCount');
      finally
        LScanner.Free;
      end;
    finally
      LExpected.Free;
    end;
  end;

begin
  LTree := TTempTree.Create(['App\Main.pas', 'App\3rdParty\Other\Other.pas', FORK + 'Fork.pas',
    FORK + 'Source\ForkCore.pas', FORK + 'Demos\Demo1.pas', FORK + 'Lib\3rdParty\X.pas']);
  try
    AssertScan(['App', FORK], [], ['*\Demos\*', '*\3rdParty\*'],
      ['App\Main.pas', FORK + 'Fork.pas', FORK + 'Source\ForkCore.pas'], 3, 'Pattern');
    AssertScan(['App', FORK], ['App\3rdParty'], ['*\Demos\*'],
      ['App\Main.pas', FORK + 'Fork.pas', FORK + 'Source\ForkCore.pas', FORK + 'Lib\3rdParty\X.pas'], 2, 'Prefix');
    AssertScan(['App', FORK], ['App\3rdParty'], ['*\Demos\*', '*\3rdParty\*'],
      ['App\Main.pas', FORK + 'Fork.pas', FORK + 'Source\ForkCore.pas'], 3, 'Prefix and pattern');

    // The fork alone: the same, without App.
    AssertScan([FORK], [], ['*\Demos\*', '*\3rdParty\*'], [FORK + 'Fork.pas', FORK + 'Source\ForkCore.pas'], 2,
      'Pattern, the fork alone');
    AssertScan([FORK], ['App\3rdParty'], ['*\Demos\*'],
      [FORK + 'Fork.pas', FORK + 'Source\ForkCore.pas', FORK + 'Lib\3rdParty\X.pas'], 1, 'Prefix, the fork alone');

    // A root inside the fork that the fork's walk reaches anyway is dropped; one it skips is scanned whole.
    AssertScan(['App', FORK, FORK + 'Source'], [], ['*\Demos\*', '*\3rdParty\*'],
      ['App\Main.pas', FORK + 'Fork.pas', FORK + 'Source\ForkCore.pas'], 3, 'Pattern, a root the fork reaches');
    AssertScan(['App', FORK, FORK + 'Lib\3rdParty'], [], ['*\Demos\*', '*\3rdParty\*'],
      ['App\Main.pas', FORK + 'Fork.pas', FORK + 'Source\ForkCore.pas', FORK + 'Lib\3rdParty\X.pas'], 2,
      'Pattern, a root the fork skips');
  finally
    LTree.Free;
  end;
end;

{ TParallelFileScannerTests }

function TParallelFileScannerTests.ScannerClass: TParallelFileScannerClass;
begin
  Result := TParallelFileScanner;
end;

{ TFileScanExclusionsTests }

procedure TFileScanExclusionsTests.InitArrayFromStringsFillsOnlyThatKind;
var
  LExclusions: TFileScanExclusions;
  LStrings: TStringList;
begin
  LStrings := TStringList.Create;
  try
    LExclusions.PathSuffixes := ['.old'];
    LStrings.AddStrings(['C:\A', 'C:\B']);

    LExclusions.InitArrayFromStrings(ekPathPrefixes, LStrings);

    Assert.AreEqual('C:\A;C:\B', LExclusions.PathPrefixesString, 'PathPrefixes');
    Assert.AreEqual('.old', LExclusions.PathSuffixesString, 'PathSuffixes, untouched');
    Assert.AreEqual(0, Integer(Length(LExclusions.Patterns)), 'Patterns, untouched');

    LStrings.Clear;
    LStrings.Add('*\__history\*');
    LExclusions.InitArrayFromStrings(ekPatterns, LStrings);
    LExclusions.InitArrayFromStrings(ekPathSuffixes, LStrings);

    Assert.AreEqual('*\__history\*', LExclusions.PatternsString, 'Patterns');
    Assert.AreEqual('*\__history\*', LExclusions.PathSuffixesString, 'PathSuffixes');
    Assert.AreEqual('C:\A;C:\B', LExclusions.PathPrefixesString, 'PathPrefixes, untouched');
  finally
    LStrings.Free;
  end;
end;

procedure TFileScanExclusionsTests.InitArrayDataFromStringsReplacesTheContent;
var
  LArray: TArray<string>;
  LStrings: TStringList;
begin
  LStrings := TStringList.Create;
  try
    LArray := ['x', 'y', 'z'];
    LStrings.Add('q');

    InitArrayDataFromStrings(LArray, LStrings);
    Assert.AreEqual(1, Integer(Length(LArray)), 'Length');
    Assert.AreEqual('q', LArray[0], 'Item');

    LStrings.Clear;
    InitArrayDataFromStrings(LArray, LStrings);
    Assert.AreEqual(0, Integer(Length(LArray)), 'Length from an empty list');
  finally
    LStrings.Free;
  end;
end;

procedure TFileScanExclusionsTests.StringPropertiesJoinWithSemicolons;
var
  LExclusions: TFileScanExclusions;
begin
  Assert.AreEqual('', LExclusions.PathPrefixesString, 'Empty PathPrefixes');
  Assert.AreEqual('', LExclusions.PathSuffixesString, 'Empty PathSuffixes');
  Assert.AreEqual('', LExclusions.PatternsString, 'Empty Patterns');

  LExclusions.PathPrefixes := ['a'];
  LExclusions.PathSuffixes := ['b', 'c'];
  LExclusions.Patterns := ['d', 'e', 'f'];

  Assert.AreEqual('a', LExclusions.PathPrefixesString, 'PathPrefixes');
  Assert.AreEqual('b;c', LExclusions.PathSuffixesString, 'PathSuffixes');
  Assert.AreEqual('d;e;f', LExclusions.PatternsString, 'Patterns');
end;

end.
