unit DPFSUnit.Parallel.FileScanner.Spring;

interface

{$INCLUDE DPFSUnit.Parallel.FileScanner.inc}

{$IFDEF USE_SPRING4D}
uses
  System.Classes, System.SysUtils, System.Diagnostics,
  DPFSUnit.Parallel.FileScanner, Spring.Collections;

type
  // Spring4D-flavoured scanner: the RTL scanner (workers on the RTL PPL), with GetFileList overloads that fill an
  // IList<string> directly from the shared walk (CollectFiles).
  TParallelFileScannerSpring = class(TParallelFileScanner)
  public
    function GetFileList(const ADirectories: TArray<string>; const AExclusions: TFileScanExclusions;
      const AFileNamesList: IList<string>): Boolean; overload;
    function GetFileList(const ADirectories: TStringList; const AExclusions: TFileScanExclusions;
      const AFileNamesList: IList<string>): Boolean; overload;
  end;
{$ENDIF}

implementation

{$IFDEF USE_SPRING4D}

{ TParallelFileScannerSpring }

function TParallelFileScannerSpring.GetFileList(const ADirectories: TArray<string>; const AExclusions: TFileScanExclusions;
  const AFileNamesList: IList<string>): Boolean;
var
  LFileScanStopWatch: TStopwatch;
  LMergeSorted: Boolean;
begin
  LFileScanStopWatch := TStopwatch.StartNew;

  // Shared parallel walk (same as the RTL path), filled straight into AFileNamesList. The files arrive already
  // sorted when they go into an empty list; items the caller added earlier still need the full sort below.
  LMergeSorted := FSortResultList and (AFileNamesList.Count = 0);

  AFileNamesList.AddRange(CollectFiles(ADirectories, AExclusions, LMergeSorted));

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
  const AFileNamesList: IList<string>): Boolean;
begin
  Result := GetFileList(ADirectories.ToStringArray, AExclusions, AFileNamesList);
end;

{$ENDIF}

end.
