unit Delphi.WildCardMatcher.Benchmark.Utils;

// Utility routines for the WildCardMatcher benchmark: logging and the
// best-of-rounds timing plumbing.  Deliberately independent of the
// matcher unit itself, so only Delphi.WildCardMatcher.Benchmarks binds to
// Delphi.WildCardMatcher (which lets the baseline program swap in the
// frozen matcher snapshot).

interface

uses
  Delphi.WildCardMatcher.Benchmark.Types;


  // Writes ALine to the console AND appends it to the log file (created
  // fresh on the first call, next to the exe).  If the file cannot be
  // created, logging silently continues console-only.
  procedure Log(const ALine: string); overload;
  procedure Log(const AFormat: string; const AArgs: array of const); overload;

  // Full path of the log file this run writes to.
  function LogFileName: string;

  // Runs ARun BENCH_ROUNDS times, validates the match count every round and
  // logs the BEST (minimum) time as ns/op.
  procedure TimeVariant(const AVariant: string; const ATotalOps, AExpectedTotal: Int64; const ARun: TBenchRun);

implementation

uses
  System.Diagnostics, System.SysUtils;

var
  GLogFile: TextFile;
  GLogOpen: Boolean = False;
  GLogFailed: Boolean = False;

function LogFileName: string;
const
  LOG_FILE_NAME = 'WildCardMatcherBenchmarkResults.txt';
begin
  Result := ExtractFilePath(ParamStr(0)) + LOG_FILE_NAME;
end;

procedure EnsureLogOpen;
begin
  if GLogOpen or GLogFailed then
    Exit;

  try
    AssignFile(GLogFile, LogFileName);
    Rewrite(GLogFile);
    GLogOpen := True;

    WriteLn(GLogFile, Format('WildCardMatcher benchmark run - %s', [DateTimeToStr(Now)]));
    WriteLn(GLogFile, '');
  except
    // Console-only from here on (e.g. exe dir not writable).
    GLogFailed := True;
  end;
end;

procedure Log(const ALine: string);
begin
  WriteLn(ALine);

  EnsureLogOpen;

  if GLogOpen then
  begin
    WriteLn(GLogFile, ALine);
    Flush(GLogFile);
  end;
end;

procedure Log(const AFormat: string; const AArgs: array of const);
begin
  Log(Format(AFormat, AArgs));
end;

procedure TimeVariant(const AVariant: string; const ATotalOps, AExpectedTotal: Int64; const ARun: TBenchRun);
var
  LRound: Integer;
  LWatch: TStopwatch;
  LMatches: Int64;
  LMs, LBestMs: Double;
  LNsPerOp: Double;
begin
  LBestMs := 0;

  for LRound := 1 to BENCH_ROUNDS do
  begin
    LWatch := TStopwatch.StartNew;
    LMatches := ARun();
    LWatch.Stop;

    if LMatches <> AExpectedTotal then
    begin
      Log('  %-24s MATCH-COUNT MISMATCH: expected %d, got %d - RESULT INVALID', [AVariant, AExpectedTotal, LMatches]);
      Halt(1);
    end;

    LMs := LWatch.Elapsed.TotalMilliseconds;

    if (LRound = 1) or (LMs < LBestMs) then
      LBestMs := LMs;
  end;

  LNsPerOp := (LBestMs * 1_000_000.0) / ATotalOps;
  Log('  %-24s %10.1f ns/op   (best %8.2f ms, %d ops)', [AVariant, LNsPerOp, LBestMs, ATotalOps]);
end;

initialization

finalization
  if GLogOpen then
    CloseFile(GLogFile);

end.
