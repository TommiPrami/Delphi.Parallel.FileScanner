unit DPFSUnit.Parallel.FileScanner.Spring;

interface

{$INCLUDE DPFSUnit.Parallel.FileScanner.inc}

uses
  System.Classes, System.IOUtils, DPFSUnit.Parallel.FileScanner, Spring.Collections;

type
  TParallelFileScannerSpring = class(TParallelFileScannerCustom)
  strict protected
    procedure GetFiles(const APath, ASearchPattern: string; const ASearchOption: TSearchOption; const AFiles: IList<string>); reintroduce;
    procedure MergeResultLists(const AResultList: IList<IList<string>>; const AResult: IList<string>); reintroduce;
  public
    function GetFileList(const ADirectories: TArray<string>; const AExcludes: TFileScanExcludes; var AFileNamesList: IList<string>): Boolean; reintroduce; overload;
    function GetFileList(const ADirectories: TStringList; const AExcludes: TFileScanExcludes; var AFileNamesList: IList<string>): Boolean; reintroduce; overload;
  end;

implementation

uses
  System.Diagnostics, System.SysUtils
{$IFDEF USE_OMNI_THREAD_LIBRARY}
  , OtlCollections, OtlComm, OtlCommon, OtlParallel, OtlTask, OtlTaskControl
{$ELSE}
  , System.Threading
{$ENDIF};

{ TParallelFileScannerSpring }

function TParallelFileScannerSpring.GetFileList(const ADirectories: TArray<string>; const AExcludes: TFileScanExcludes;
  var AFileNamesList: IList<string>): Boolean;
var
  LCurrentRootPath: string;
  LListsOfFilelists: IList<IList<string>>;
  LFileScanStopWatch: TStopwatch;
begin
  LFileScanStopWatch := TStopwatch.StartNew;

  FExclusions := AExcludes;
  FSkippedFilesCount := 0;
  AFileNamesList := TCollections.CreateList<string>;

  LListsOfFilelists := TCollections.CreateList<IList<string>>;
  try
    for LCurrentRootPath in ADirectories do
    begin
      if not DirectoryExists(LCurrentRootPath) then
        Continue;

{$IFDEF USE_OMNI_THREAD_LIBRARY}
      Parallel
        .For(0, FExtensions.Count - 1)
        .NumTasks(TThread.ProcessorCount)
        .Execute(
{$ELSE}
      TParallel
        .For(0, FExtensions.Count - 1,
{$ENDIF}
        procedure(AIndex: Integer)
        var
          LExtension: string;
          LTempFileNames: IList<string>;
        begin
          LExtension := FExtensions[AIndex];

          LTempFileNames := TCollections.CreateList<string>;

          // Seems that this GetFiles ripped from the RTL, ported to use Spring container. MAYBE it is not faster after all.
          // Should make two version, one using standard RTL and one using this. Time them, which is faster.
          // This is now about 450ms so maybe no use to optimize much
          GetFiles(LCurrentRootPath, LExtension, TSearchOption.soAllDirectories, LTempFileNames);

          if LTempFileNames.Count > 0 then
          begin
            FLock.Acquire;
            try
              LListsOfFilelists.Add(LTempFileNames);
            finally
              FLock.Release;
            end;
          end;

          LTempFileNames := nil;
        end
      );
    end;

    FLock.Acquire;
    try
      MergeResultLists(LListsOfFilelists, AFileNamesList);

      if SortResultList then
        AFileNamesList.Sort;
    finally
      FLock.Release;
    end;
  finally
    LListsOfFilelists := nil;
  end;

  Result := AFileNamesList.Count > 0;

  LFileScanStopWatch.Stop;
  FDiskScanTimeForFiles := LFileScanStopWatch.Elapsed.TotalMilliseconds;
end;

function TParallelFileScannerSpring.GetFileList(const ADirectories: TStringList; const AExcludes: TFileScanExcludes;
  var AFileNamesList: IList<string>): Boolean;
begin
  Result := GetFileList(ADirectories.ToStringArray, AExcludes, AFileNamesList);
end;

procedure TParallelFileScannerSpring.GetFiles(const APath, ASearchPattern: string; const ASearchOption: TSearchOption;
  const AFiles: IList<string>);
var
  LPreCallback: TDirectoryWalkProc;
begin
  CheckGetFilesParameters(APath, ASearchPattern);

  AFiles.Capacity := 1024 * 16;

  LPreCallback :=
    function (const APath: string; const AFileInfo: TSearchRec): Boolean
    begin
      Result := True;

      if AFileInfo.Attr and System.SysUtils.faDirectory = 0 then
        AFiles.Add(TPath.Combine(APath, AFileInfo.Name, False));
    end;

  WalkThroughDirectory(APath, ASearchPattern, LPreCallback, ASearchOption = TSearchOption.soAllDirectories);
end;

procedure TParallelFileScannerSpring.MergeResultLists(const AResultList: IList<IList<string>>;
  const AResult: IList<string>);
var
  I: Integer;
  J: Integer;
  LFileName: string;
  LUniqueFiles: ISet<string>;
begin
  LUniqueFiles := TCollections.CreateSortedSet<string>;
  LUniqueFiles.Capacity := 2000;

  for I := 0 to AResultList.Count - 1 do
  begin
    for J := 0 to AResultList[I].Count - 1 do
    begin
      LFileName := AResultList[I][J];

      if not LUniqueFiles.Contains(LFileName) then
        LUniqueFiles.Add(LFileName);
    end;
  end;

  AResult.AddRange(LUniqueFiles);
end;

end.
