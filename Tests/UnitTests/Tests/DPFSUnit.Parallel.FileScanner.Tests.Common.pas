unit DPFSUnit.Parallel.FileScanner.Tests.Common;

// Shared by the scanner test fixtures: the scanned tree, an independent baseline of it, and file-set assertions.

interface

uses
  System.Classes, System.SysUtils;

type
  // Raised on purpose by a test callback, to check that a worker's exception reaches the caller. Its own class, so
  // the debugger can be told to ignore it.
  EScannerTestCallbackFailure = class(Exception);

  // Collects the files of the "of object" ScanFiles overload; FileFound runs on worker threads.
  TCallbackCollector = class(TObject)
  strict private
    FFiles: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure FileFound(const AFileName: string);
    property Files: TStringList read FFiles;
  end;

  // Absolute, lower-case, sorted file set without duplicates, for order-independent compares. Also a thread-safe
  // collector for streaming callbacks (Add locks).
  TFileSet = class(TObject)
  strict private
    FFiles: TStringList;
    function GetCount: Integer;
  public
    constructor Create; overload;
    constructor Create(const AFiles: TArray<string>); overload;
    destructor Destroy; override;
    // Files in AExpected but not here, and here but not in AExpected - the first few of each, for a failure message.
    function DescribeDifference(const AExpected: TFileSet): string;
    function DifferenceCount(const AExpected: TFileSet): Integer;
    function StartsWithCount(const APrefix: string): Integer;
    procedure Add(const AFileName: string);
    property Count: Integer read GetCount;
    property Files: TStringList read FFiles;
  end;

  TScanTree = class(TObject)
  public
    // The patterns every test scans with.
    class function Extensions: TArray<string>; static;
    // Flat recursive enumeration of ARoots filtered with the same patterns - independent of the scanner.
    class function BaselineFiles(const ARoots: TArray<string>): TFileSet; static;
    class function MatchesAnyExtension(const AFileName: string): Boolean; static;
    // This repository's Source directory: a real tree (a few thousand entries, one excluded-worthy subtree) that
    // stays meaningful as the code evolves. Found by walking up from the executable.
    class function SourceRoot: string; static;
  end;

// Asserts that ARawFiles - a scanner's result as returned - holds every file of AExpected exactly once, and nothing
// else.
procedure AssertSameFiles(const AExpected: TFileSet; const ARawFiles: TArray<string>; const AWhat: string);

implementation

uses
  System.IOUtils, DUnitX.TestFramework;

const
  EXTENSION_PATTERNS: array[0..4] of string = ('*.pas', '*.inc', '*.dfm', '*.dpr', '*.dproj');
  MAX_LISTED_DIFFERENCES = 5;

{ TCallbackCollector }

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

{ TFileSet }

constructor TFileSet.Create;
begin
  inherited Create;

  FFiles := TStringList.Create;
  FFiles.Sorted := True;
  FFiles.Duplicates := dupIgnore;
end;

constructor TFileSet.Create(const AFiles: TArray<string>);
begin
  Create;

  for var LIndex := 0 to High(AFiles) do
    Add(AFiles[LIndex]);
end;

destructor TFileSet.Destroy;
begin
  FFiles.Free;

  inherited Destroy;
end;

function TFileSet.DescribeDifference(const AExpected: TFileSet): string;

  function Missing(const AFrom, AIn: TStringList): string;
  var
    LCount: Integer;
  begin
    Result := '';
    LCount := 0;

    for var LIndex := 0 to AFrom.Count - 1 do
      if AIn.IndexOf(AFrom[LIndex]) < 0 then
      begin
        Inc(LCount);

        if LCount <= MAX_LISTED_DIFFERENCES then
          Result := Result + sLineBreak + '    ' + AFrom[LIndex];
      end;

    if LCount > MAX_LISTED_DIFFERENCES then
      Result := Result + sLineBreak + Format('    ... %d more', [LCount - MAX_LISTED_DIFFERENCES]);
  end;

begin
  Result := 'missing:' + Missing(AExpected.Files, FFiles) + sLineBreak + 'unexpected:' + Missing(FFiles, AExpected.Files);
end;

function TFileSet.DifferenceCount(const AExpected: TFileSet): Integer;
begin
  Result := 0;

  for var LIndex := 0 to AExpected.Count - 1 do
    if FFiles.IndexOf(AExpected.Files[LIndex]) < 0 then
      Inc(Result);

  for var LIndex := 0 to FFiles.Count - 1 do
    if AExpected.Files.IndexOf(FFiles[LIndex]) < 0 then
      Inc(Result);
end;

function TFileSet.GetCount: Integer;
begin
  Result := FFiles.Count;
end;

function TFileSet.StartsWithCount(const APrefix: string): Integer;
var
  LPrefix: string;
begin
  LPrefix := TPath.GetFullPath(APrefix).ToLower;
  Result := 0;

  for var LIndex := 0 to FFiles.Count - 1 do
    if FFiles[LIndex].StartsWith(LPrefix) then
      Inc(Result);
end;

procedure TFileSet.Add(const AFileName: string);
begin
  TMonitor.Enter(FFiles);
  try
    FFiles.Add(TPath.GetFullPath(AFileName).ToLower);
  finally
    TMonitor.Exit(FFiles);
  end;
end;

{ TScanTree }

class function TScanTree.BaselineFiles(const ARoots: TArray<string>): TFileSet;
begin
  Result := TFileSet.Create;

  for var LRoot in ARoots do
    if TDirectory.Exists(LRoot) then
      for var LFile in TDirectory.GetFiles(LRoot, '*', TSearchOption.soAllDirectories) do
        if MatchesAnyExtension(TPath.GetFileName(LFile)) then
          Result.Add(LFile);
end;

class function TScanTree.Extensions: TArray<string>;
begin
  Result := ['*.pas', '*.inc', '*.dfm', '*.dpr', '*.dproj'];
end;

class function TScanTree.MatchesAnyExtension(const AFileName: string): Boolean;
begin
  for var LPattern in EXTENSION_PATTERNS do
    if TPath.MatchesPattern(AFileName, LPattern, False) then
      Exit(True);

  Result := False;
end;

class function TScanTree.SourceRoot: string;
var
  LDirectory: string;
begin
  LDirectory := ExcludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));

  while LDirectory <> '' do
  begin
    if TFile.Exists(TPath.Combine(LDirectory, 'Source\Units\DPFSUnit.Parallel.FileScanner.pas')) then
      Exit(TPath.Combine(LDirectory, 'Source'));

    if ExtractFileDir(LDirectory) = LDirectory then
      Break;

    LDirectory := ExtractFileDir(LDirectory);
  end;

  raise Exception.Create('Repository root (Source\Units\DPFSUnit.Parallel.FileScanner.pas) not found above '
    + ParamStr(0));
end;

procedure AssertSameFiles(const AExpected: TFileSet; const ARawFiles: TArray<string>; const AWhat: string);
var
  LActual: TFileSet;
begin
  LActual := TFileSet.Create(ARawFiles);
  try
    Assert.AreEqual(LActual.Count, Integer(Length(ARawFiles)), AWhat + ': files returned vs distinct files - a file was '
      + 'returned more than once');
    Assert.AreEqual(0, LActual.DifferenceCount(AExpected), AWhat + ': not the expected files' + sLineBreak
      + LActual.DescribeDifference(AExpected));
  finally
    LActual.Free;
  end;
end;

end.
