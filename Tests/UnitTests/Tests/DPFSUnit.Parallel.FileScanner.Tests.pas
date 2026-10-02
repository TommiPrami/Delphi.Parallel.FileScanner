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
  end;

  [TestFixture]
  TParallelFileScannerTests = class(TParallelFileScannerTestsCustom)
  strict protected
    function ScannerClass: TParallelFileScannerClass; override;
  end;

implementation

uses
  Winapi.Windows, System.IOUtils, FastMM5;

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
  Result := ScannerClass.Create(TScanTree.Extensions, ASortResultList);
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
// of its own: its own files are scanned (its subdirectories fall under the prefix too).
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

  // Everything outside the excluded prefix, plus the nested root's own files.
  LExpected := TFileSet.Create;
  try
    for var LIndex := 0 to FBaseline.Count - 1 do
      if not FBaseline.Files[LIndex].StartsWith(LExcludedPrefix.ToLower) then
        LExpected.Add(FBaseline.Files[LIndex]);

    for var LFile in TDirectory.GetFiles(LNestedRoot, '*', TSearchOption.soTopDirectoryOnly) do
      if TScanTree.MatchesAnyExtension(TPath.GetFileName(LFile)) then
        LExpected.Add(LFile);

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

// The workers must run at the priority asked for - and at normal priority again in the next scan, as pool threads
// are reused.
procedure TParallelFileScannerTestsCustom.WorkersRunAtRequestedPriority;
const
  PRIORITIES: array[0..1] of TThreadPriority = (TThreadPriority.tpLower, TThreadPriority.tpNormal);
  PRIORITY_NAMES: array[0..1] of string = ('tpLower', 'tpNormal');
  WINDOWS_PRIORITIES: array[0..1] of Integer = (THREAD_PRIORITY_BELOW_NORMAL, THREAD_PRIORITY_NORMAL);
var
  LExclusions: TFileScanExclusions;
  LExpectedPriority: Integer;
  LFileCount: Integer;
  LScanner: TParallelFileScannerCustom;
  LWrongPriorityCount: Integer;
begin
  LScanner := CreateScanner;
  try
    for var LIndex := 0 to High(PRIORITIES) do
    begin
      LExpectedPriority := WINDOWS_PRIORITIES[LIndex];
      LFileCount := 0;
      LWrongPriorityCount := 0;

      LScanner.ScanFiles(FScanRoots, LExclusions,
        procedure(const AFileName: string)
        begin
          if GetThreadPriority(GetCurrentThread) <> LExpectedPriority then
            AtomicIncrement(LWrongPriorityCount);
        end,
        LFileCount, PRIORITIES[LIndex]);

      Assert.IsTrue(LFileCount > 0, PRIORITY_NAMES[LIndex] + ': the scan must find files');
      Assert.AreEqual(0, LWrongPriorityCount, PRIORITY_NAMES[LIndex] + ': files found on a thread at another priority');
    end;
  finally
    LScanner.Free;
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
  LExclusions.Patterns := ['*\["FastMM5"|"SPRING4D"]\*'];

  AssertExclusionsLeave(LExclusions,
    BaselineWhere(
      function(AFile: string): Boolean
      begin
        Result := not (AFile.Contains('\fastmm5\') or AFile.Contains('\spring4d\'));
      end),
    'GetFileList excluding *\["FastMM5"|"SPRING4D"]\*');
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
  LExclusions.Patterns := ['*\[3rdParty\*'];
  LScanner := CreateScanner;
  try
    Assert.WillRaise(
      procedure
      begin
        ScanToStringList(LScanner, FScanRoots, LExclusions);
      end,
      EInOutArgumentException, 'An unterminated [ in an exclusion pattern must raise');
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

    LScanner := ScannerClass.Create(['*["Scanner"|"Matcher"]*.pas']);
    try
      LScanner.ConvertRelativePathsToAbsolute := True;

      AssertSameFiles(LExpected, ScanToStringList(LScanner, FScanRoots, LExclusions),
        'GetFileList with *["Scanner"|"Matcher"]*.pas');
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
  LScanner := ScannerClass.Create(['Units\*.pas']);
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

{ TParallelFileScannerTests }

function TParallelFileScannerTests.ScannerClass: TParallelFileScannerClass;
begin
  Result := TParallelFileScanner;
end;

end.
