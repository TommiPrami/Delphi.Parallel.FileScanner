unit DPFSUnit.Parallel.FileScanner.Spring;

interface

{$INCLUDE DPFSUnit.Parallel.FileScanner.inc}

{$IFDEF USE_SPRING4D}
uses
  System.Classes, DPFSUnit.Parallel.FileScanner, Spring.Collections
{$IFDEF USE_OMNI_THREAD_LIBRARY}
  , OtlTaskControl
{$ENDIF};

type
  // Spring4D-flavoured result container. The actual scanning/merging lives in the base
  // class (ScanToArray); these overloads only adapt the result into an IList<string>.
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
begin
  AFileNamesList.AddRange(ScanToArray(ADirectories, AExclusions
{$IFDEF USE_OMNI_THREAD_LIBRARY}
    , APriority
{$ENDIF}));

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
