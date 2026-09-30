unit DPFSUnit.Parallel.FileScanner.OTL.Tests;

// Windows only, like the scanners, so TThreadPriority being Windows-specific is fine.
{$WARN SYMBOL_PLATFORM OFF}

interface

{$INCLUDE ..\..\..\Source\Units\DPFSUnit.Parallel.FileScanner.inc}

{$IF DEFINED(USE_OMNI_THREAD_LIBRARY)}
uses
  DUnitX.TestFramework, DPFSUnit.Parallel.FileScanner, DPFSUnit.Parallel.FileScanner.Tests;

type
  // Every shared scanner test (inherited) on TParallelFileScannerOTL, plus what only the OTL scanner has.
  [TestFixture]
  TParallelFileScannerOTLTests = class(TParallelFileScannerTestsCustom)
  strict protected
    function ScannerClass: TParallelFileScannerClass; override;
  public
    [Test] procedure ValueQueueMatchesBaseline;
    [Test] procedure ToOTLThreadPriorityMapsEveryLevel;
  end;
{$IFEND}

implementation

{$IF DEFINED(USE_OMNI_THREAD_LIBRARY)}
uses
  System.Classes, System.SysUtils, System.TypInfo, OtlCommon, OtlContainers, OtlTaskControl,
  DPFSUnit.Parallel.FileScanner.OTL, DPFSUnit.Parallel.FileScanner.Tests.Common;

// OtlTaskControl's TOTLThreadPriority reuses tpIdle, tpLowest, tpNormal and tpHighest, so the priority values are
// written qualified here.

{ TParallelFileScannerOTLTests }

function TParallelFileScannerOTLTests.ScannerClass: TParallelFileScannerClass;
begin
  Result := TParallelFileScannerOTL;
end;

procedure TParallelFileScannerOTLTests.ValueQueueMatchesBaseline;
var
  LExclusions: TFileScanExclusions;
  LFileCount: Integer;
  LFiles: TStringList;
  LQueue: TOmniQueue;
  LScanner: TParallelFileScannerOTL;
  LValue: TOmniValue;
begin
  LScanner := CreateScanner as TParallelFileScannerOTL;
  LFiles := TStringList.Create;
  try
    LQueue := TOmniQueue.Create;
    try
      LFileCount := 0;

      Assert.IsTrue(LScanner.GetFileList(ScanRoots, LExclusions, LQueue, LFileCount), 'GetFileList found nothing');

      while LQueue.TryDequeue(LValue) do
        LFiles.Add(LValue.AsString);
    finally
      LQueue.Free;
    end;

    Assert.AreEqual(LFiles.Count, LFileCount, 'AFileCount must equal the number of files queued');
    AssertSameFiles(Baseline, LFiles.ToStringArray, 'GetFileList (OTL value queue)');
  finally
    LFiles.Free;
    LScanner.Free;
  end;
end;

// One to one where OTL has the level; OTL has no time-critical priority, so tpTimeCritical becomes tpHighest.
procedure TParallelFileScannerOTLTests.ToOTLThreadPriorityMapsEveryLevel;
const
  EXPECTED: array[TThreadPriority] of TOTLThreadPriority = (TOTLThreadPriority.tpIdle, TOTLThreadPriority.tpLowest,
    TOTLThreadPriority.tpBelowNormal, TOTLThreadPriority.tpNormal, TOTLThreadPriority.tpAboveNormal,
    TOTLThreadPriority.tpHighest, TOTLThreadPriority.tpHighest);
begin
  for var LPriority := Low(TThreadPriority) to High(TThreadPriority) do
    Assert.AreEqual(GetEnumName(TypeInfo(TOTLThreadPriority), Ord(EXPECTED[LPriority])),
      GetEnumName(TypeInfo(TOTLThreadPriority), Ord(TParallelFileScannerOTL.ToOTLThreadPriority(LPriority))),
      GetEnumName(TypeInfo(TThreadPriority), Ord(LPriority)));
end;
{$IFEND}

end.
