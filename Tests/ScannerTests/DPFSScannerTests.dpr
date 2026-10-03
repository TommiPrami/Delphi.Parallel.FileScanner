program DPFSScannerTests;

{ The scanner checks that need a process of their own - everything else is in the DUnitX suite, Tests\UnitTests.
  They measure process-wide state from a clean start, which a test runner would disturb: its other fixtures start
  thread pools too, and TestInsight runs any subset of the tests in any order.

  - OTL pool threads bounded: back-to-back scans must keep reusing the scanner pool's threads. Counted from before
    the first scan of the process, since the pool's threads outlive the scans that start them.

  Exit code is 0 when all checks pass, 1 otherwise (usable from CI / scripts). }

{$APPTYPE CONSOLE}

{$INCLUDE ..\..\Source\Units\DPFSUnit.Parallel.FileScanner.inc}

uses
  Winapi.Windows,
  Winapi.TlHelp32,
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.Math,
  DPFSUnit.Parallel.FileScanner in '..\..\Source\Units\DPFSUnit.Parallel.FileScanner.pas'
  {$IF DEFINED(USE_OMNI_THREAD_LIBRARY)}
  , DPFSUnit.Parallel.FileScanner.OTL in '..\..\Source\Units\DPFSUnit.Parallel.FileScanner.OTL.pas'
  {$IFEND}
  ;

var
  GFailures: Integer = 0;

procedure Report(const APassed: Boolean; const AName, ADetails: string);
begin
  if APassed then
    Writeln(Format('[PASS] %-30s %s', [AName, ADetails]))
  else
  begin
    Writeln(Format('[FAIL] %-30s %s', [AName, ADetails]));
    Inc(GFailures);
  end;
end;

function RepoRoot: string;
begin
  // The executable lives in <repo>\Tests\ScannerTests\Win32\<Config>\.
  Result := TPath.GetFullPath(TPath.Combine(ExtractFilePath(ParamStr(0)), '..\..\..\..'));
end;

{$IF DEFINED(USE_OMNI_THREAD_LIBRARY)}
function ThreadCount: Integer;
var
  LEntry: TThreadEntry32;
  LSnapshot: THandle;
begin
  Result := 0;
  LSnapshot := CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD, 0);

  if LSnapshot = INVALID_HANDLE_VALUE then
    Exit;

  try
    LEntry.dwSize := SizeOf(LEntry);

    if Thread32First(LSnapshot, LEntry) then
      repeat
        if LEntry.th32OwnerProcessID = GetCurrentProcessId then
          Inc(Result);
      until not Thread32Next(LSnapshot, LEntry);
  finally
    CloseHandle(LSnapshot);
  end;
end;

// Back-to-back scans must reuse the pool's threads. A pool without a thread limit kept adding threads - the next
// scan's tasks arrived before the previous scan's threads were idle again - and past ~60 of them OTL's wait for
// more than 64 handles crashed the process (EListError on a Windows thread-pool thread).
procedure CheckPoolThreadsBounded(const AScanRoots: TArray<string>; const AThreadsAtStart: Integer);
const
  SCAN_COUNT = 300;
  THREAD_MARGIN = 8; // the pool's manager thread, OTL housekeeping
var
  LExclusions: TFileScanExclusions;
  LLimit: Integer;
  LList: TStringList;
  LMaxThreads: Integer;
  LScanner: TParallelFileScannerOTL;
begin
  LLimit := AThreadsAtStart + Min(TThread.ProcessorCount, 56) + THREAD_MARGIN;
  LMaxThreads := ThreadCount;
  LScanner := TParallelFileScannerOTL.Create(['*.pas', '*.inc', '*.dfm', '*.dpr', '*.dproj'], nil, False);
  LList := TStringList.Create;
  try
    for var LIndex := 1 to SCAN_COUNT do
    begin
      LList.Clear;
      LScanner.GetFileList(AScanRoots, LExclusions, LList);
      LMaxThreads := Max(LMaxThreads, ThreadCount);
    end;

    Report((LMaxThreads <= LLimit) and (LList.Count > 0), 'OTL pool threads bounded',
      Format('%d scans of %d files: at most %d threads (limit %d)', [SCAN_COUNT, LList.Count, LMaxThreads, LLimit]));
  finally
    LList.Free;
    LScanner.Free;
  end;
end;
{$IFEND}

begin
  try
    {$IF DEFINED(USE_OMNI_THREAD_LIBRARY)}
    // Before anything in this process has started a thread pool.
    var LThreadsAtStart := ThreadCount;
    var LScanRoots: TArray<string> := [TPath.Combine(RepoRoot, 'Source')];

    Writeln('Scanning: ' + LScanRoots[0]);
    Writeln('');

    CheckPoolThreadsBounded(LScanRoots, LThreadsAtStart);
    {$ELSE}
    Writeln('Nothing to check: the checks here need USE_OMNI_THREAD_LIBRARY.');
    {$IFEND}

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

  {$IF DEFINED(DEBUG)}
  Writeln('Press [enter] to continue');

  ReadLn;
  {$IFEND}


end.
