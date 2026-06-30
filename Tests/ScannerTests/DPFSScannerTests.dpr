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
  LQueue: IOmniValueQueue;
  LValue: TOmniValue;
  LRaw: TStringList;
  LFileCount: Integer;
begin
  LScanner := TParallelFileScanner.Create(Extensions);
  LRaw := TStringList.Create;
  try
    LScanner.ConvertRelativePathsToAbsolute := True;
    LQueue := CreateOmniValueQueue(False);
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
    finally
      LBaseline.Free;
    end;

    CheckExclusion;

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
