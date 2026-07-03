unit Delphi.WildCardMatcher.Speed.DUnitX;

// Throughput "tests" - they double as correctness checks (each scenario
// asserts the expected match count) but their real purpose is to print
// timing so regressions are visible.
//
// LOOP_COUNT * INPUT_COUNT = total Match() calls per scenario.
// Bump LOOP_COUNT if the runs finish too fast to be informative on your
// machine.

interface

uses
  DUnitX.TestFramework,
  Delphi.WildCardMatcher;

type
  [TestFixture]
  [Category('Speed')]
  TWildCardSpeedDUnitX = class
  strict private
    FInputs: TArray<string>;
    procedure RunBenchmark(const AScenarioLabel, APattern: string; const AExpectedMatchesPerIter, ALoopCount: Integer;
      var ASpeedInfo: string; const ACaseSensitive: Boolean = False);
  public
    [Setup]
    procedure Setup;

    [Test]
    procedure SpeedZeroMatchesTest;
    [Test]
    procedure SpeedAllMatchTest;
    [Test]
    procedure SpeedFivePercentMatchTest;
    [Test]
    procedure SpeedWorstCaseBacktrackingTest;
    [Test]
    procedure SpeedWorstCaseBacktrackingCaseSensitiveTest;
  end;

implementation

uses
  System.SysUtils, System.Diagnostics;

const
  INPUT_COUNT = 1000;
  LOOP_COUNT = 1000; // for the bulk 1000-input scenarios
  WORST_CASE_LOOP_COUNT = 100_000; // for the single-long-path worst case

  // Worst-case fixture: input and pattern shared by the CI and CS variants
  // so the only thing that differs between the two runs is the engine.
  WORST_CASE_LONG_PATH = 'C:\Users\developer\source\repos\MyApp\src\main\delphi\modules\data_access\repositories\implementations\sqlserver\'
    + 'concrete\OrderRepositoryImplementation_2024_03_15_v3_revised_final.pas';
  // Every '*' segment finds something in the path; '#' matches the '3' in
  // '_v3_'.  Only the final '.xyz' anchor fails - forcing the matcher to
  // unwind through every '*' alternative before reporting no-match.
  WORST_CASE_COMPLEX_PATTERN = '*\Users\*\source\*\MyApp\*\modules\*\repositories\*\sqlserver\*\*Repository*_v#_*.xyz';

procedure TWildCardSpeedDUnitX.Setup;
var
  LIdx: Integer;
begin
  SetLength(FInputs, INPUT_COUNT);

  // First 5% of inputs match the 5%-scenario pattern 'MatchMe_*.pas'.
  for LIdx := 0 to (INPUT_COUNT div 20) - 1 do
    FInputs[LIdx] := Format('MatchMe_%.4d.pas', [LIdx]);

  // Remaining 95% start with 'Skip_' so they never match 'MatchMe_*'.
  for LIdx := INPUT_COUNT div 20 to INPUT_COUNT - 1 do
    FInputs[LIdx] := Format('Skip_%.4d.txt', [LIdx]);
end;

procedure TWildCardSpeedDUnitX.RunBenchmark(const AScenarioLabel, APattern: string; const AExpectedMatchesPerIter, ALoopCount: Integer;
  var  ASpeedInfo: string; const ACaseSensitive: Boolean);
var
  LMatcher: TWildCard;
  LWatch: TStopwatch;
  LIter, LIdx, LMatches: Integer;
  LElapsedMs, LNsPerOp, LOpsPerMs: Double;
  LTotalOps, LExpectedTotal: Int64;
begin
  LMatches := 0;
  ASpeedInfo := '';

  // Create the matcher once outside the timed region with the requested
  // case mode - the per-call cost in the loop is then identical to what
  // the old static API used to do.
  LMatcher := TWildCard.Create(ACaseSensitive);

  LWatch := TStopwatch.StartNew;

  for LIter := 1 to ALoopCount do
    for LIdx := 0 to High(FInputs) do
      if LMatcher.Match(FInputs[LIdx], APattern) then
        Inc(LMatches);

  LWatch.Stop;

  // Correctness gate first - if the matcher is wrong, the timing is
  // meaningless, so fail loudly before reporting the speed result.
  LExpectedTotal := Int64(ALoopCount) * AExpectedMatchesPerIter;
  Assert.AreEqual(LExpectedTotal, Int64(LMatches), Format('%s: total match count mismatch (expected %d, got %d)',
    [AScenarioLabel, LExpectedTotal, LMatches]));

  LTotalOps  := Int64(ALoopCount) * Length(FInputs);
  LElapsedMs := LWatch.Elapsed.TotalMilliseconds;
  LNsPerOp   := (LElapsedMs * 1_000_000.0) / LTotalOps;

  if LElapsedMs > 0 then
    LOpsPerMs := LTotalOps / LElapsedMs
  else
    LOpsPerMs := 0;

  // Pass with a message so the timing shows up in TestInsight's pass
  // output instead of relying on a console WriteLn.
  ASpeedInfo := Format('%s: ops=%d,  elapsed=%.3fms,  %.1fns/op,  %.0fops/ms  matches/iter=%d',
    [AScenarioLabel, LTotalOps, LElapsedMs, LNsPerOp, LOpsPerMs, AExpectedMatchesPerIter]);
end;

procedure TWildCardSpeedDUnitX.SpeedZeroMatchesTest;
var
  LSpeedInfo: string;
begin
  // Pattern that no input matches.  Cheap-failure case: 'Skip_'/'MatchMe_'
  // inputs both fail on character 1 ('x' vs 'S'/'M').
  RunBenchmark('0%_Matches', 'xyzzy_*.zzz', 0, LOOP_COUNT, LSpeedInfo);

  Assert.Pass(LSpeedInfo);
end;

procedure TWildCardSpeedDUnitX.SpeedAllMatchTest;
var
  LSpeedInfo: string;
begin
  // Pattern that matches every input on the first try - cheapest path.
  RunBenchmark('100%', '*', INPUT_COUNT, LOOP_COUNT, LSpeedInfo);

  Assert.Pass(LSpeedInfo);
end;

procedure TWildCardSpeedDUnitX.SpeedFivePercentMatchTest;
var
  LSpeedInfo: string;
begin
  // Pattern that matches exactly 5% of the inputs.  Mixes early-success
  // ('MatchMe_...') with early-failure ('Skip_...' fails on character 1).
  RunBenchmark('5%_Matches', 'MatchMe_*.pas', INPUT_COUNT div 20, LOOP_COUNT, LSpeedInfo);

  Assert.Pass(LSpeedInfo);
end;

procedure TWildCardSpeedDUnitX.SpeedWorstCaseBacktrackingTest;
var
  LSpeedInfo: string;
begin
  // Worst case (case-insensitive engine): pathological backtracking, 0%
  // matches, one long synthetic path with many '*' segments in the pattern.
  // Per-call cost dominates here, so we use a single input and a dedicated
  // loop count instead of the 1000-input bulk array used by the other
  // scenarios.
  SetLength(FInputs, 1);
  FInputs[0] := WORST_CASE_LONG_PATH;
  RunBenchmark('WorstCaseCI', WORST_CASE_COMPLEX_PATTERN, 0, WORST_CASE_LOOP_COUNT, LSpeedInfo, False);

  Assert.Pass(LSpeedInfo);
end;

procedure TWildCardSpeedDUnitX.SpeedWorstCaseBacktrackingCaseSensitiveTest;
var
  LSpeedInfo: string;
begin
  // Identical scenario to SpeedWorstCaseBacktrackingTest but routed through
  // the case-sensitive engine.  The literal anchors in the pattern match
  // the path's casing exactly, so the backtracking shape is the same - the
  // only difference is the per-char comparison cost (no ToUpper calls).
  // Compare the two numbers to see the speedup the split paths deliver.
  SetLength(FInputs, 1);
  FInputs[0] := WORST_CASE_LONG_PATH;
  RunBenchmark('WorstCaseCS', WORST_CASE_COMPLEX_PATTERN, 0, WORST_CASE_LOOP_COUNT, LSpeedInfo, True);

  Assert.Pass(LSpeedInfo);
end;

initialization
  TDUnitX.RegisterTestFixture(TWildCardSpeedDUnitX);

end.
