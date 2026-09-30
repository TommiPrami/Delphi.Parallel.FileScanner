unit DPFSUnit.Parallel.FileScanner.Spring;

interface

{$INCLUDE DPFSUnit.Parallel.FileScanner.inc}

{$IFDEF USE_SPRING4D}
uses
  System.Classes, System.SysUtils, System.Diagnostics,
  DPFSUnit.Parallel.FileScanner, Spring.Collections
{$IFDEF USE_OMNI_THREAD_LIBRARY}
  , OtlTaskControl
{$ENDIF};

type
  // Spring4D-flavoured scanner: shares the parallel walk (CollectFiles) with the base class and
  // fills an IList<string> directly.
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
var
  LFileScanStopWatch: TStopwatch;
  LMergeSorted: Boolean;
begin
  LFileScanStopWatch := TStopwatch.StartNew;

  // Shared parallel walk (same as the RTL path), filled straight into AFileNamesList. The files arrive already
  // sorted when they go into an empty list; items the caller added earlier still need the full sort below.
  LMergeSorted := FSortResultList and (AFileNamesList.Count = 0);

  AFileNamesList.AddRange(CollectFiles(ADirectories, AExclusions, LMergeSorted
{$IFDEF USE_OMNI_THREAD_LIBRARY}
    , APriority
{$ENDIF}));

  if FSortResultList and not LMergeSorted then
    AFileNamesList.Sort(
      function(const ALeft, ARight: string): Integer
      begin
        Result := CompareText(ALeft, ARight); // case-insensitive, matching the RTL variant
      end);

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
