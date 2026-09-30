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
    function CreateScanner(const ASortResultList: Boolean = True): TParallelFileScannerCustom;
    function ScannerClass: TParallelFileScannerClass; virtual; abstract;
    function ScanToStringList(const AScanner: TParallelFileScannerCustom; const ARoots: TArray<string>;
      const AExclusions: TFileScanExclusions): TArray<string>;
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

{ TParallelFileScannerTests }

function TParallelFileScannerTests.ScannerClass: TParallelFileScannerClass;
begin
  Result := TParallelFileScanner;
end;

end.
