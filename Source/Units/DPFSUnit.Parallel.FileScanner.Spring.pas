unit DPFSUnit.Parallel.FileScanner.Spring;

interface

{$INCLUDE DPFSUnit.Parallel.FileScanner.inc}

{$IFDEF USE_SPRING4D}
uses
  System.Classes, System.SysUtils, System.Diagnostics, System.Generics.Collections,
  DPFSUnit.Parallel.FileScanner, Spring.Collections
{$IFDEF USE_OMNI_THREAD_LIBRARY}
  , OtlTaskControl
{$ENDIF};

type
  // Spring4D-flavoured scanner: shares the parallel walk (RunScanJobs) with the base class,
  // then dedups + sorts natively into an IList<string> using Spring4D collections.
  TParallelFileScannerSpring = class(TParallelFileScannerCustom)
  public
    function GetFileList(const ADirectories: TArray<string>; const AExclusions: TFileScanExclusions; const AFileNamesList: IList<string>
      {$IFDEF USE_OMNI_THREAD_LIBRARY}
      ; const APriority: TOTLThreadPriority = tpNormal
      {$ENDIF}): Boolean; reintroduce; overload;
    function GetFileList(const ADirectories: TStringList; const AExclusions: TFileScanExclusions; const AFileNamesList: IList<string>
      {$IFDEF USE_OMNI_THREAD_LIBRARY}
      ; const APriority: TOTLThreadPriority = tpNormal
      {$ENDIF}): Boolean; reintroduce; overload;
  end;
{$ENDIF}

implementation

{$IFDEF USE_SPRING4D}

{ TParallelFileScannerSpring }

function TParallelFileScannerSpring.GetFileList(const ADirectories: TArray<string>; const AExclusions: TFileScanExclusions;
  const AFileNamesList: IList<string>
{$IFDEF USE_OMNI_THREAD_LIBRARY}
  ; const APriority: TOTLThreadPriority = tpNormal
{$ENDIF}): Boolean;
const
  MERGE_INITIAL_CAPACITY = 2000;
var
  LListOfFileLists: TObjectList<TStringList>;
  LUniqueFiles: ISet<string>;
  LFileScanStopWatch: TStopwatch;
begin
  LFileScanStopWatch := TStopwatch.StartNew;

  LListOfFileLists := TObjectList<TStringList>.Create(True);
  try
    // Shared parallel walk (same as the RTL path); merge natively into the Spring4D list below.
    RunScanJobs(ADirectories, AExclusions, LListOfFileLists
{$IFDEF USE_OMNI_THREAD_LIBRARY}
      , APriority
{$ENDIF});

    // Deduplicate with a Spring4D hash set and fill AFileNamesList directly (no RTL round-trip),
    // preserving first-seen order; sorting is applied only when SortResultList is set.
    LUniqueFiles := TCollections.CreateSet<string>(MERGE_INITIAL_CAPACITY);

    for var LList in LListOfFileLists do
      for var LFileName in LList do
        if LUniqueFiles.Add(LFileName) then
          AFileNamesList.Add(LFileName);

    if FSortResultList then
      AFileNamesList.Sort(
        function(const ALeft, ARight: string): Integer
        begin
          Result := CompareText(ALeft, ARight); // case-insensitive, matching the RTL variant
        end);
  finally
    LListOfFileLists.Free;
  end;

  LFileScanStopWatch.Stop;
  FDiskScanTimeForFiles := LFileScanStopWatch.Elapsed.TotalMilliseconds;

  Result := AFileNamesList.Count > 0;
end;

function TParallelFileScannerSpring.GetFileList(const ADirectories: TStringList; const AExclusions: TFileScanExclusions;
  const AFileNamesList: IList<string>
{$IFDEF USE_OMNI_THREAD_LIBRARY}
  ; const APriority: TOTLThreadPriority = tpNormal
{$ENDIF}): Boolean;
begin
  Result := GetFileList(ADirectories.ToStringArray, AExclusions, AFileNamesList
{$IFDEF USE_OMNI_THREAD_LIBRARY}
  , APriority
{$ENDIF});
end;

{$ENDIF}

end.
