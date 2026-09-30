program DPFSScannerTests;

{ Regression test: the parallel scanner must return exactly the same set of files as a
  straightforward flat enumeration (same pattern matcher), for every result-container API,
  and prefix exclusion must keep excluded subtrees out of the result.

  Scans this repository's own Source tree, so it stays meaningful as the code evolves.
  Exit code is 0 when all checks pass, 1 otherwise (usable from CI / scripts). }

{$APPTYPE CONSOLE}

{$INCLUDE ..\..\Source\Units\DPFSUnit.Parallel.FileScanner.inc}

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  DPFSUnit.Parallel.FileScanner in '..\..\Source\Units\DPFSUnit.Parallel.FileScanner.pas'
  {$IFDEF USE_OMNI_THREAD_LIBRARY}
  , OtlContainers
  , OtlCommon
  {$ENDIF}
  {$IFDEF USE_SPRING4D}
  , DPFSUnit.Parallel.FileScanner.Spring in '..\..\Source\Units\DPFSUnit.Parallel.FileScanner.Spring.pas'
  , Spring.Collections
  {$ENDIF}
  ;

const
  EXTENSION_PATTERNS: array[0..4] of string = ('*.pas', '*.inc', '*.dfm', '*.dpr', '*.dproj');

type
  // Exercises the "of object" ScanFiles overload; FileFound runs on worker threads.
  TCallbackCollector = class(TObject)
  strict private
    FFiles: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure FileFound(const AFileName: string);
    property Files: TStringList read FFiles;
  end;

constructor TCallbackCollector.Create;
begin
  inherited Create;

  FFiles := TStringList.Create;
end;

destructor TCallbackCollector.Destroy;
begin
  FFiles.Free;

  inherited Destroy;
end;

procedure TCallbackCollector.FileFound(const AFileName: string);
begin
  TMonitor.Enter(FFiles);
  try
    FFiles.Add(AFileName);
  finally
    TMonitor.Exit(FFiles);
  end;
end;

var
  GFailures: Integer = 0;
  GScanRoots: TArray<string>;

function Extensions: TArray<string>;
begin
  Result := ['*.pas', '*.inc', '*.dfm', '*.dpr', '*.dproj'];
end;

function MatchesAnyExtension(const AName: string): Boolean;
begin
  Result := False;

  for var LExt in EXTENSION_PATTERNS do
    if TPath.MatchesPattern(AName, LExt, False) then
      Exit(True);
end;

// Sorted, de-duplicated, normalised (absolute + lower case) set, for order-independent compares.
function NormalizedSet(const AItems: TArray<string>): TStringList;
begin
  Result := TStringList.Create;
  Result.Sorted := True;
  Result.Duplicates := dupIgnore;

  for var LItem in AItems do
    Result.Add(TPath.GetFullPath(LItem).ToLower);
end;

// Independent baseline: flat recursive enumeration filtered with the same matcher the scanner uses.
function BaselineSet: TStringList;
var
  LRaw: TStringList;
begin
  LRaw := TStringList.Create;
  try
    for var LDir in GScanRoots do
      if TDirectory.Exists(LDir) then
        for var LFile in TDirectory.GetFiles(LDir, '*', TSearchOption.soAllDirectories) do
          if MatchesAnyExtension(TPath.GetFileName(LFile)) then
            LRaw.Add(LFile);
    Result := NormalizedSet(LRaw.ToStringArray);
  finally
    LRaw.Free;
  end;
end;

procedure CheckSameAsBaseline(const ATestName: string; const AActual, ABaseline: TStringList);
var
  LDiff, LIndex: Integer;
begin
  LDiff := 0;

  for LIndex := 0 to ABaseline.Count - 1 do
    if AActual.IndexOf(ABaseline[LIndex]) < 0 then
      Inc(LDiff);

  for LIndex := 0 to AActual.Count - 1 do
    if ABaseline.IndexOf(AActual[LIndex]) < 0 then
      Inc(LDiff);

  if (LDiff = 0) and (AActual.Count = ABaseline.Count) then
    Writeln(Format('[PASS] %-22s %d files', [ATestName, AActual.Count]))
  else
  begin
    Writeln(Format('[FAIL] %-22s actual=%d baseline=%d diff=%d', [ATestName, AActual.Count, ABaseline.Count, LDiff]));
    Inc(GFailures);
  end;
end;

function ScanRtl: TStringList;
var
  LScanner: TParallelFileScanner;
  LExclusions: TFileScanExclusions;
  LList: TStringList;
begin
  LScanner := TParallelFileScanner.Create(Extensions);
  LList := TStringList.Create;
  try
    LScanner.ConvertRelativePathsToAbsolute := True;
    LScanner.GetFileList(GScanRoots, LExclusions, LList);

    Result := NormalizedSet(LList.ToStringArray);
  finally
    LList.Free;
    LScanner.Free;
  end;
end;

{$IFDEF USE_OMNI_THREAD_LIBRARY}
function ScanOtlQueue: TStringList;
var
  LScanner: TParallelFileScanner;
  LExclusions: TFileScanExclusions;
  LQueue: TOmniQueue;
  LValue: TOmniValue;
  LRaw: TStringList;
  LFileCount: Integer;
begin
  LScanner := TParallelFileScanner.Create(Extensions);
  LRaw := TStringList.Create;
  try
    LScanner.ConvertRelativePathsToAbsolute := True;
    LQueue := TOmniQueue.Create;
    LFileCount := 0;
    LScanner.GetFileList(GScanRoots, LExclusions, LQueue, LFileCount);

    while LQueue.TryDequeue(LValue) do
      LRaw.Add(LValue.AsString);

    Result := NormalizedSet(LRaw.ToStringArray);
  finally
    LRaw.Free;
    LScanner.Free;
  end;
end;
{$ENDIF}

{$IFDEF USE_SPRING4D}
function ScanSpring: TStringList;
var
  LScanner: TParallelFileScannerSpring;
  LExclusions: TFileScanExclusions;
  LList: IList<string>;
begin
  LScanner := TParallelFileScannerSpring.Create(Extensions);
  try
    LScanner.ConvertRelativePathsToAbsolute := True;
    LList := TCollections.CreateList<string>;
    LScanner.GetFileList(GScanRoots, LExclusions, LList);

    Result := NormalizedSet(LList.ToArray);
  finally
    LScanner.Free;
  end;
end;
{$ENDIF}

// Streaming callback API: files are delivered from worker threads as they are found, so the
// callback locks the shared list - exactly what real calling code must do.
function ScanViaCallback: TStringList;
var
  LScanner: TParallelFileScanner;
  LExclusions: TFileScanExclusions;
  LRaw: TStringList;
  LFileCount: Integer;
begin
  LScanner := TParallelFileScanner.Create(Extensions);
  LRaw := TStringList.Create;
  try
    LScanner.ConvertRelativePathsToAbsolute := True;
    LFileCount := 0;

    LScanner.ScanFiles(GScanRoots, LExclusions,
      procedure(const AFileName: string)
      begin
        TMonitor.Enter(LRaw);
        try
          LRaw.Add(AFileName);
        finally
          TMonitor.Exit(LRaw);
        end;
      end,
      LFileCount);

    if LFileCount <> LRaw.Count then
    begin
      Writeln(Format('[FAIL] %-22s AFileCount=%d but callback delivered %d', ['callback count', LFileCount, LRaw.Count]));
      Inc(GFailures);
    end;

    Result := NormalizedSet(LRaw.ToStringArray);
  finally
    LRaw.Free;
    LScanner.Free;
  end;
end;

// Same scan through the classic "of object" method-pointer overload.
function ScanViaObjectCallback: TStringList;
var
  LScanner: TParallelFileScanner;
  LExclusions: TFileScanExclusions;
  LCollector: TCallbackCollector;
  LFileCount: Integer;
begin
  LScanner := TParallelFileScanner.Create(Extensions);
  LCollector := TCallbackCollector.Create;
  try
    LScanner.ConvertRelativePathsToAbsolute := True;
    LFileCount := 0;

    LScanner.ScanFiles(GScanRoots, LExclusions, LCollector.FileFound, LFileCount);

    Result := NormalizedSet(LCollector.Files.ToStringArray);
  finally
    LCollector.Free;
    LScanner.Free;
  end;
end;

procedure CheckExclusion;
var
  LScanner: TParallelFileScanner;
  LExclusions: TFileScanExclusions;
  LList: TStringList;
  LExcludedPrefix: string;
  LViolations, LIndex: Integer;
begin
  LExcludedPrefix := TPath.GetFullPath(TPath.Combine(GScanRoots[0], '3rdPartyLibraries')).ToLower;
  LScanner := TParallelFileScanner.Create(Extensions);
  LList := TStringList.Create;
  try
    LScanner.ConvertRelativePathsToAbsolute := True;
    LExclusions.PathPrefixes := [LExcludedPrefix];
    LScanner.GetFileList(GScanRoots, LExclusions, LList);

    LViolations := 0;
    for LIndex := 0 to LList.Count - 1 do
      if TPath.GetFullPath(LList[LIndex]).ToLower.StartsWith(LExcludedPrefix) then
        Inc(LViolations);

    if (LViolations = 0) and (LList.Count > 0) then
      Writeln(Format('[PASS] %-22s %d files kept, none under excluded prefix', ['prefix exclusion', LList.Count]))
    else
    begin
      Writeln(Format('[FAIL] %-22s %d files kept, %d under excluded prefix', ['prefix exclusion', LList.Count, LViolations]));
      Inc(GFailures);
    end;
  finally
    LList.Free;
    LScanner.Free;
  end;
end;

// The raw result must not hold a file twice (compared case-insensitively) and must match the expected set.
procedure CheckNoDuplicatesAndSameAsBaseline(const ATestName: string; const ARaw, ABaseline: TStringList);
var
  LSet: TStringList;
begin
  LSet := NormalizedSet(ARaw.ToStringArray);
  try
    if LSet.Count <> ARaw.Count then
    begin
      Writeln(Format('[FAIL] %-22s %d files returned, only %d distinct', [ATestName, ARaw.Count, LSet.Count]));
      Inc(GFailures);
    end
    else
      CheckSameAsBaseline(ATestName, LSet, ABaseline);
  finally
    LSet.Free;
  end;
end;

// Overlapping roots - the root again, spelled with a trailing delimiter and in upper case, plus a nested one -
// must be walked once: every file exactly once, and the same set as scanning the root alone.
procedure CheckOverlappingRoots(const ABaseline: TStringList);
var
  LExclusions: TFileScanExclusions;
  LFileCount: Integer;
  LList: TStringList;
  LRoots: TArray<string>;
  LScanner: TParallelFileScanner;
begin
  LRoots := [GScanRoots[0], TPath.Combine(GScanRoots[0], 'Units'), IncludeTrailingPathDelimiter(GScanRoots[0]),
    GScanRoots[0].ToUpper];
  LScanner := TParallelFileScanner.Create(Extensions);
  LList := TStringList.Create;
  try
    LScanner.ConvertRelativePathsToAbsolute := True;
    LScanner.GetFileList(LRoots, LExclusions, LList);
    CheckNoDuplicatesAndSameAsBaseline('overlapping roots', LList, ABaseline);

    LList.Clear;
    LFileCount := 0;
    LScanner.ScanFiles(LRoots, LExclusions,
      procedure(const AFileName: string)
      begin
        TMonitor.Enter(LList);
        try
          LList.Add(AFileName);
        finally
          TMonitor.Exit(LList);
        end;
      end,
      LFileCount);
    CheckNoDuplicatesAndSameAsBaseline('overlapping (stream)', LList, ABaseline);
  finally
    LList.Free;
    LScanner.Free;
  end;
end;

// A root nested under an excluded prefix of another root is not reached by that root's walk, so it must stay a
// root of its own: its own files are scanned (its subdirectories fall under the prefix too, as before).
procedure CheckNestedRootUnderExclusion(const ABaseline: TStringList);
var
  LExcludedPrefix: string;
  LExclusions: TFileScanExclusions;
  LExpected: TStringList;
  LList: TStringList;
  LNestedRoot: string;
  LScanner: TParallelFileScanner;
begin
  LExcludedPrefix := TPath.GetFullPath(TPath.Combine(GScanRoots[0], '3rdPartyLibraries'));
  LNestedRoot := TPath.Combine(LExcludedPrefix, 'FastMM5');

  // Expected: everything outside the excluded prefix, plus the nested root's own files.
  LExpected := TStringList.Create;
  try
    LExpected.Sorted := True;
    LExpected.Duplicates := dupIgnore;

    for var LFile in ABaseline do
      if not LFile.StartsWith(LExcludedPrefix.ToLower) then
        LExpected.Add(LFile);

    for var LFile in TDirectory.GetFiles(LNestedRoot, '*', TSearchOption.soTopDirectoryOnly) do
      if MatchesAnyExtension(TPath.GetFileName(LFile)) then
        LExpected.Add(TPath.GetFullPath(LFile).ToLower);

    LScanner := TParallelFileScanner.Create(Extensions);
    LList := TStringList.Create;
    try
      LScanner.ConvertRelativePathsToAbsolute := True;
      LExclusions.PathPrefixes := [LExcludedPrefix];
      LScanner.GetFileList([GScanRoots[0], LNestedRoot], LExclusions, LList);
      CheckNoDuplicatesAndSameAsBaseline('nested excluded root', LList, LExpected);
    finally
      LList.Free;
      LScanner.Free;
    end;
  finally
    LExpected.Free;
  end;
end;

// With SortResultList (the default) the result must come back in CompareText order.
procedure CheckSortOrder(const ATestName: string; const AFiles: TArray<string>);
var
  LDisorders: Integer;
begin
  LDisorders := 0;

  for var LIndex := 1 to High(AFiles) do
    if CompareText(AFiles[LIndex - 1], AFiles[LIndex]) > 0 then
      Inc(LDisorders);

  if (LDisorders = 0) and (Length(AFiles) > 1) then
    Writeln(Format('[PASS] %-22s %d files in CompareText order', [ATestName, Length(AFiles)]))
  else
  begin
    Writeln(Format('[FAIL] %-22s %d of %d neighbours out of order', [ATestName, LDisorders, Length(AFiles)]));
    Inc(GFailures);
  end;
end;

procedure CheckResultOrder;
var
  LExclusions: TFileScanExclusions;
  LList: TStringList;
  LScanner: TParallelFileScanner;
  {$IFDEF USE_SPRING4D}
  LSpringList: IList<string>;
  LSpringScanner: TParallelFileScannerSpring;
  {$ENDIF}
begin
  LScanner := TParallelFileScanner.Create(Extensions);
  LList := TStringList.Create;
  try
    LScanner.GetFileList(GScanRoots, LExclusions, LList);
    CheckSortOrder('RTL sort order', LList.ToStringArray);
  finally
    LList.Free;
    LScanner.Free;
  end;

  {$IFDEF USE_SPRING4D}
  LSpringScanner := TParallelFileScannerSpring.Create(Extensions);
  try
    LSpringList := TCollections.CreateList<string>;
    LSpringScanner.GetFileList(GScanRoots, LExclusions, LSpringList);
    CheckSortOrder('Spring4D sort order', LSpringList.ToArray);
  finally
    LSpringScanner.Free;
  end;
  {$ENDIF}
end;

function RepoRoot: string;
begin
  // The executable lives in <repo>\Tests\ScannerTests\Win32\Debug\.
  Result := TPath.GetFullPath(TPath.Combine(ExtractFilePath(ParamStr(0)), '..\..\..\..'));
end;

begin
  try
    GScanRoots := [TPath.Combine(RepoRoot, 'Source')];
    Writeln('Scanning: ' + GScanRoots[0]);
    Writeln('');

    var LBaseline := BaselineSet;
    try
      var LRtl := ScanRtl;
      try
        CheckSameAsBaseline('RTL TStringList', LRtl, LBaseline);
      finally
        LRtl.Free;
      end;

      {$IFDEF USE_OMNI_THREAD_LIBRARY}
      var LQueue := ScanOtlQueue;
      try
        CheckSameAsBaseline('OTL value queue', LQueue, LBaseline);
      finally
        LQueue.Free;
      end;
      {$ENDIF}

      {$IFDEF USE_SPRING4D}
      var LSpring := ScanSpring;
      try
        CheckSameAsBaseline('Spring4D IList', LSpring, LBaseline);
      finally
        LSpring.Free;
      end;
      {$ENDIF}

      var LCallback := ScanViaCallback;
      try
        CheckSameAsBaseline('streaming callback', LCallback, LBaseline);
      finally
        LCallback.Free;
      end;

      var LObjectCallback := ScanViaObjectCallback;
      try
        CheckSameAsBaseline('of-object callback', LObjectCallback, LBaseline);
      finally
        LObjectCallback.Free;
      end;

      CheckOverlappingRoots(LBaseline);
      CheckNestedRootUnderExclusion(LBaseline);
    finally
      LBaseline.Free;
    end;

    CheckExclusion;
    CheckResultOrder;

    Writeln('');
    if GFailures = 0 then
      Writeln('ALL TESTS PASSED')
    else
      Writeln(Format('%d TEST(S) FAILED', [GFailures]));
  except
    on E: Exception do
    begin
      Writeln('EXCEPTION: ' + E.ClassName + ': ' + E.Message);
      Inc(GFailures);
    end;
  end;

  ExitCode := Ord(GFailures <> 0);
end.
