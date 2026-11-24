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

  // A throw-away tree under %TEMP%, for cases the Source tree does not hold (odd names, non-ASCII names, a shape that
  // forces hand-offs between workers). Deleted, with everything in it, by Free.
  TTempTree = class(TObject)
  strict private
    FRoot: string;
  public
    // AEntries are paths relative to the root: a file (created with a little content), or a folder when it ends in '\'.
    constructor Create(const AEntries: array of string);
    destructor Destroy; override;
    procedure Add(const AEntry: string);
    // The absolute path of ARelative.
    function Path(const ARelative: string): string;
    // The files ARelative names, as a TFileSet (absolute paths).
    function FileSet(const ARelative: array of string): TFileSet;
    property Root: string read FRoot;
  end;

// Asserts that ARawFiles - a scanner's result as returned - holds every file of AExpected exactly once, and nothing
// else.
procedure AssertSameFiles(const AExpected: TFileSet; const ARawFiles: TArray<string>; const AWhat: string);
// Creates the directory junction ALink pointing at the folder ATarget (mklink /J: needs no elevation). Remove it with
// RemoveDirectory, which removes the junction itself and never touches its target.
procedure CreateDirectoryJunction(const ALink, ATarget: string);

implementation

uses
  Winapi.Windows, System.IOUtils, DUnitX.TestFramework;

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

{ TTempTree }

// The tree is built and deleted with the Win32 calls on \\?\ paths, so it may hold paths longer than MAX_PATH, which
// TDirectory and TFile refuse unless the process opted in to long paths.

// AAbsolutePath (no trailing delimiter) with the \\?\ prefix.
function ExtendedPath(const AAbsolutePath: string): string;
begin
  Result := '\\?\' + ExcludeTrailingPathDelimiter(AAbsolutePath);
end;

procedure ForceDirectoriesAnyLength(const AAbsolutePath: string);
var
  LPath: string;
begin
  LPath := ExcludeTrailingPathDelimiter(AAbsolutePath);

  if GetFileAttributes(PChar(ExtendedPath(LPath))) <> INVALID_FILE_ATTRIBUTES then
    Exit;

  ForceDirectoriesAnyLength(ExtractFileDir(LPath));

  if not CreateDirectory(PChar(ExtendedPath(LPath)), nil) and (GetLastError <> ERROR_ALREADY_EXISTS) then
    RaiseLastOSError(GetLastError, ': ' + LPath);
end;

procedure WriteFileAnyLength(const AAbsolutePath, AContent: string);
var
  LBytes: TBytes;
  LHandle: THandle;
  LWritten: Cardinal;
begin
  LHandle := CreateFile(PChar(ExtendedPath(AAbsolutePath)), GENERIC_WRITE, 0, nil, CREATE_ALWAYS,
    FILE_ATTRIBUTE_NORMAL, 0);

  if LHandle = INVALID_HANDLE_VALUE then
    RaiseLastOSError(GetLastError, ': ' + AAbsolutePath);

  try
    LBytes := TEncoding.UTF8.GetBytes(AContent);

    if (Length(LBytes) > 0) and not WriteFile(LHandle, LBytes[0], Length(LBytes), LWritten, nil) then
      RaiseLastOSError(GetLastError, ': ' + AAbsolutePath);
  finally
    CloseHandle(LHandle);
  end;
end;

// Deletes the folder AAbsolutePath and everything in it. A directory link inside is removed itself, never entered.
procedure DeleteTreeAnyLength(const AAbsolutePath: string);
var
  LFindData: TWin32FindData;
  LFindHandle: THandle;
  LName: string;
  LPath: string;
begin
  LPath := ExcludeTrailingPathDelimiter(AAbsolutePath);
  LFindHandle := FindFirstFile(PChar(ExtendedPath(LPath) + '\*'), LFindData);

  if LFindHandle <> INVALID_HANDLE_VALUE then
  try
    repeat
      LName := LFindData.cFileName;

      if (LName = '.') or (LName = '..') then
        Continue;

      if LFindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY = 0 then
      begin
        SetFileAttributes(PChar(ExtendedPath(LPath + '\' + LName)), FILE_ATTRIBUTE_NORMAL);
        DeleteFile(PChar(ExtendedPath(LPath + '\' + LName)));
      end
      else if LFindData.dwFileAttributes and FILE_ATTRIBUTE_REPARSE_POINT <> 0 then
        RemoveDirectory(PChar(ExtendedPath(LPath + '\' + LName)))
      else
        DeleteTreeAnyLength(LPath + '\' + LName);
    until not FindNextFile(LFindHandle, LFindData);
  finally
    Winapi.Windows.FindClose(LFindHandle);
  end;

  RemoveDirectory(PChar(ExtendedPath(LPath)));
end;

constructor TTempTree.Create(const AEntries: array of string);
begin
  inherited Create;

  FRoot := TPath.Combine(TPath.GetTempPath, 'DPFSTest_' + TGUID.NewGuid.ToString.Trim(['{', '}']));
  TDirectory.CreateDirectory(FRoot);

  for var LEntry in AEntries do
    Add(LEntry);
end;

destructor TTempTree.Destroy;
begin
  if (FRoot <> '') and TDirectory.Exists(FRoot) then
    DeleteTreeAnyLength(FRoot);

  inherited Destroy;
end;

procedure TTempTree.Add(const AEntry: string);
begin
  if AEntry.EndsWith('\') then
    ForceDirectoriesAnyLength(Path(AEntry))
  else
  begin
    ForceDirectoriesAnyLength(ExtractFileDir(Path(AEntry)));
    WriteFileAnyLength(Path(AEntry), AEntry);
  end;
end;

function TTempTree.FileSet(const ARelative: array of string): TFileSet;
begin
  Result := TFileSet.Create;

  for var LRelative in ARelative do
    Result.Add(Path(LRelative));
end;

function TTempTree.Path(const ARelative: string): string;
begin
  Result := TPath.Combine(FRoot, ARelative);
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

procedure CreateDirectoryJunction(const ALink, ATarget: string);
var
  LCommandLine: string;
  LExitCode: Cardinal;
  LProcessInfo: TProcessInformation;
  LStartupInfo: TStartupInfo;
begin
  LCommandLine := Format('cmd.exe /c mklink /J "%s" "%s"', [ALink, ATarget]);
  UniqueString(LCommandLine); // CreateProcess may write to the command line
  LStartupInfo := Default(TStartupInfo);
  LStartupInfo.cb := SizeOf(LStartupInfo);

  if not CreateProcess(nil, PChar(LCommandLine), nil, nil, False, CREATE_NO_WINDOW, nil, nil, LStartupInfo,
    LProcessInfo) then
    RaiseLastOSError;

  try
    WaitForSingleObject(LProcessInfo.hProcess, INFINITE);

    if not GetExitCodeProcess(LProcessInfo.hProcess, LExitCode) then
      RaiseLastOSError;
  finally
    CloseHandle(LProcessInfo.hThread);
    CloseHandle(LProcessInfo.hProcess);
  end;

  if (LExitCode <> 0) or not TDirectory.Exists(ALink) then
    raise Exception.CreateFmt('mklink /J "%s" "%s" failed with exit code %d', [ALink, ATarget, LExitCode]);
end;

end.
