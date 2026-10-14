unit Delphi.WildCardMatcher.Benchmarks;

// The WildCardMatcher benchmark suite: parity checks + timed scenarios.
//
// This unit is shared by BOTH benchmark programs:
//   Delphi.WildCardMatcher.Benchmark.dpr          - current Source\ unit
//   Baseline\...BaselineBenchmark.dpr             - frozen pre-optimization
//                                                   unit (commit 0e83678)
// Each program binds Delphi.WildCardMatcher to its own copy via the 'in'
// path in its uses clause, so both always run identical code against
// their respective engines and the numbers stay comparable.
//
// RunParitySuite runs every edge case through the registered (compiled)
// and ad-hoc (interpreting) engines, CI and CS, and halts on any
// disagreement - run it after ANY change to the matcher.
//
// RunBenchmarkScenarios times both engines over realistic scenarios
// (best-of-rounds, see Delphi.WildCardMatcher.Benchmark.Types).  All
// output goes through Log - console plus the results file next to the
// exe.  Historical numbers live in RESULTS.md.
//
// Adding a scenario: one RunScenario call in RunBenchmarkScenarios.
// Adding a parity case: one CheckParity(pattern, input) line.
//
// Patterns are written in the current '<...>' syntax.  The frozen baseline
// engine predates it ('[...]' classes, bare '#' digit), so when the bound
// engine turns out to be the legacy one, ToEngineSyntax translates every
// pattern before use - the baseline unit itself stays untouched.

interface

  procedure RunParitySuite;
  procedure RunBenchmarkScenarios;

implementation

uses
  System.SysUtils, Delphi.WildCardMatcher, Delphi.WildCardMatcher.Benchmark.Types, Delphi.WildCardMatcher.Benchmark.Utils;

const
  BULK_INPUT_COUNT = 1000;
  BULK_LOOPS = 1000;
  WORST_CASE_LOOPS = 100_000;

  WORST_CASE_LONG_PATH = 'C:\Users\developer\source\repos\MyApp\src\main\delphi\modules\data_access\repositories\implementations\sqlserver\'
    + 'concrete\OrderRepositoryImplementation_2024_03_15_v3_revised_final.pas';
  WORST_CASE_COMPLEX_PATTERN = '*\Users\*\source\*\MyApp\*\modules\*\repositories\*\sqlserver\*\*Repository*_v<#>_*.xyz';

var
  GParityFailures: Integer = 0;

{ ----------------------------------------------------------------------- }
{ Pattern syntax of the bound engine                                       }
{ ----------------------------------------------------------------------- }

// True when the bound Delphi.WildCardMatcher speaks the '<...>' syntax;
// the legacy engine reads '<a>' as three literal characters.
function EngineHasAngleBracketSyntax: Boolean;
begin
  Result := TWildCard.Create.Match('a', '<a>');
end;

// Translates a '<...>' pattern to the legacy '[...]' syntax when the bound
// engine needs it: digit run '<##>' -> '##', '<' -> '[', '>' -> ']'.  A
// '>' inside a quoted alternative becomes ']', which the legacy engine
// also treats as a literal there, so the meaning is preserved.
function ToEngineSyntax(const APattern: string): string;
var
  LIndex, LRunEnd: Integer;
begin
  if EngineHasAngleBracketSyntax then
    Exit(APattern);

  Result := '';
  LIndex := 1;

  while LIndex <= Length(APattern) do
  begin
    if APattern[LIndex] = '<' then
    begin
      LRunEnd := LIndex + 1;

      while (LRunEnd <= Length(APattern)) and (APattern[LRunEnd] = '#') do
        Inc(LRunEnd);

      if (LRunEnd > LIndex + 1) and (LRunEnd <= Length(APattern)) and (APattern[LRunEnd] = '>') then
      begin
        Result := Result + StringOfChar('#', LRunEnd - LIndex - 1);
        LIndex := LRunEnd + 1;
        Continue;
      end;

      Result := Result + '[';
    end
    else if APattern[LIndex] = '>' then
      Result := Result + ']'
    else
      Result := Result + APattern[LIndex];

    Inc(LIndex);
  end;
end;

{ ----------------------------------------------------------------------- }
{ Parity suite: registered engine vs ad-hoc engine                         }
{ ----------------------------------------------------------------------- }

procedure CheckParity(const APattern, AInput: string);
var
  LPattern: string;
  LAdhocCI, LAdhocCS, LRegCI, LRegCS: Boolean;
begin
  LPattern := ToEngineSyntax(APattern);

  LAdhocCI := TWildCard.Create.Match(AInput, LPattern);
  LRegCI := TWildCard.Create(LPattern).Match(AInput);

  LAdhocCS := TWildCard.Create(True).Match(AInput, LPattern);
  LRegCS := TWildCard.Create(LPattern, True).Match(AInput);

  if LAdhocCI <> LRegCI then
  begin
    Inc(GParityFailures);
    Log('PARITY FAIL (CI): pattern=<%s> input=<%s> ad-hoc=%s registered=%s',
      [LPattern, AInput, BoolToStr(LAdhocCI, True), BoolToStr(LRegCI, True)]);
  end;

  if LAdhocCS <> LRegCS then
  begin
    Inc(GParityFailures);
    Log('PARITY FAIL (CS): pattern=<%s> input=<%s> ad-hoc=%s registered=%s',
      [LPattern, AInput, BoolToStr(LAdhocCS, True), BoolToStr(LRegCS, True)]);
  end;
end;

procedure RunParitySuite;
begin
  if EngineHasAngleBracketSyntax then
    Log('Pattern syntax: <...> classes (current engine).')
  else
    Log('Pattern syntax: [...] classes (legacy engine) - patterns are translated before use.');

  // '*' / '?' / digit-run basics
  CheckParity('wh*', 'what');
  CheckParity('wh*', 'awhile');
  CheckParity('*', '');
  CheckParity('', '');
  CheckParity('', 'x');
  CheckParity('*.txt', 'readme.txt');
  CheckParity('*.txt', 'readme.doc');
  CheckParity('*world*', 'hello world');
  CheckParity('*world*', 'hello');
  CheckParity('b?ll', 'ball');
  CheckParity('b?ll', 'bll');
  CheckParity('1<#>3', '103');
  CheckParity('1<#>3', '1a3');
  CheckParity('1<#>3', '13');
  CheckParity('*?', '');
  CheckParity('*?', 'x');
  CheckParity('?*', 'x');
  CheckParity('**.txt', 'a.b.c.txt');
  CheckParity('*a*b*c*', 'xaxbxcx');
  CheckParity('*a*b*c*', 'xxxxxx');
  CheckParity('Test_<###>.log', 'Test_001.log');
  CheckParity('Test_<###>.log', 'Test_00A.log');
  CheckParity('*<##>.log', 'trace_42.log');
  CheckParity('*<##>.log', 'trace_4.log');
  CheckParity('*.pas', 'pas');
  CheckParity('*.pas', '.pas');

  // Char classes
  CheckParity('b<ae>ll', 'bell');
  CheckParity('b<ae>ll', 'bill');
  CheckParity('b<!ae>ll', 'bull');
  CheckParity('b<!ae>ll', 'ball');
  CheckParity('b<a-c>d', 'bbd');
  CheckParity('b<a-c>d', 'bdd');
  CheckParity('<a-zA-Z>', 'X');
  CheckParity('<a-zA-Z>', '5');
  CheckParity('<!a>*', 'apple');
  CheckParity('<!a>*', 'zebra');
  CheckParity('<>abc>', '>');
  CheckParity('<>abc>', 'x');
  CheckParity('<-x>', '-');
  CheckParity('<a->', '-');
  CheckParity('<a->', 'b');
  CheckParity('<abc', 'a');
  CheckParity('<>', 'a');
  CheckParity('Foo<_-><#>.pas', 'Foo_5.pas');
  CheckParity('Foo<_-><#>.pas', 'Foo.5.pas');
  CheckParity('*<abc>end', 'xxxaend');
  CheckParity('*<abc>end', 'xxxdend');
  CheckParity('<#a>', '#');
  CheckParity('<!#>', '5');

  // Quoted alternation
  CheckParity('<"foo">', 'foo');
  CheckParity('<"foo">', 'fo');
  CheckParity('<"foo"|"bar">', 'bar');
  CheckParity('<"foo"|"bar">', 'qux');
  CheckParity('*<"foo"|"bar">*', 'xxbaryy');
  CheckParity('*<"foo"|"bar">*', 'xxxxxx');
  CheckParity('<"a"|"ab">b', 'abb');
  CheckParity('<"a"|"ab">b', 'ab');
  CheckParity('<"">foo', 'foo');
  CheckParity('foo<"">', 'foo');
  CheckParity('<""|"foo">bar', 'foobar');
  CheckParity('<!"foo"|"bar">*', 'quxsuffix');
  CheckParity('<!"foo"|"bar">*', 'foosuffix');
  CheckParity('<!"foo"|"barbaz">-tail', 'abcdef-tail');
  CheckParity('<!"foo"|"barbaz">-tail', 'barbaz-tail');
  CheckParity('<!"foo"|"bar">', 'ab');
  CheckParity('<"foo', 'foo');
  CheckParity('<"foo"|"bar', 'foo');
  CheckParity('<"foo""bar">', 'foo');
  CheckParity('<"foo"x"bar">', 'foo');
  CheckParity('<"a>b">', 'a>b');
  CheckParity('<"<x>">', '<x>');
  CheckParity('*<"3rdparty"|"ThirdParty">*.md', 'docs\3rdparty\notes.md');
  CheckParity('*<"3rdparty"|"ThirdParty">*.md', 'docs\internal\notes.md');

  // Literals: '|' outside classes; '[', ']' and '#' (legal in file names)
  CheckParity('a|b', 'a|b');
  CheckParity('a|b', 'ab');
  CheckParity('C:\Temp\Foo[ab].txt', 'C:\Temp\Foo[ab].txt');
  CheckParity('Track #<##>.mp3', 'Track #01.mp3');
  CheckParity('*[old]*', 'notes[old].txt');

  // Worst-case pattern parity
  CheckParity(WORST_CASE_COMPLEX_PATTERN, WORST_CASE_LONG_PATH);

  if GParityFailures > 0 then
  begin
    Log('');
    Log('PARITY SUITE FAILED: %d mismatches - benchmark aborted.', [GParityFailures]);
    Halt(1);
  end;

  Log('Parity suite passed (registered vs ad-hoc engine, CI + CS).');
end;

{ ----------------------------------------------------------------------- }
{ Timed scenarios                                                          }
{ ----------------------------------------------------------------------- }

// Times four variants over the same inputs / patterns:
//   registered CI/CS - patterns handed to Create, Match(input)
//   ad-hoc CI/CS     - empty Create, Match(input, patterns) per call
// Note: inputs are constructed so the expected match count is the same in
// CS mode as in CI mode (exact-case inputs).
procedure RunScenario(const AName: string; const AInputs: TArray<string>; const APatterns: TArray<string>; const ALoops: Integer;
  const AExpectedPerIter: Integer);
var
  LRegisteredCI, LRegisteredCS, LAdhocCI, LAdhocCS: TWildCard;
  LTotalOps, LExpectedTotal: Int64;
  LPatterns: TArray<string>;
  LIndex: Integer;
begin
  Log('');
  Log('=== %s  (%d patterns x %d inputs x %d loops, best of %d rounds) ===',
    [AName, Length(APatterns), Length(AInputs), ALoops, BENCH_ROUNDS]);

  LTotalOps := Int64(ALoops) * Length(AInputs);
  LExpectedTotal := Int64(ALoops) * AExpectedPerIter;

  SetLength(LPatterns, Length(APatterns));

  for LIndex := 0 to High(APatterns) do
    LPatterns[LIndex] := ToEngineSyntax(APatterns[LIndex]);

  LRegisteredCI := TWildCard.Create(LPatterns, False);
  LRegisteredCS := TWildCard.Create(LPatterns, True);
  LAdhocCI := TWildCard.Create(False);
  LAdhocCS := TWildCard.Create(True);

  TimeVariant('registered CI', LTotalOps, LExpectedTotal,
    function: Int64
    var
      LIter, LIdx: Integer;
    begin
      Result := 0;
      for LIter := 1 to ALoops do
        for LIdx := 0 to High(AInputs) do
          if LRegisteredCI.Match(AInputs[LIdx]) then
            Inc(Result);
    end);

  TimeVariant('ad-hoc CI', LTotalOps, LExpectedTotal,
    function: Int64
    var
      LIter, LIdx: Integer;
    begin
      Result := 0;
      for LIter := 1 to ALoops do
        for LIdx := 0 to High(AInputs) do
          if LAdhocCI.Match(AInputs[LIdx], LPatterns) then
            Inc(Result);
    end);

  TimeVariant('registered CS', LTotalOps, LExpectedTotal,
    function: Int64
    var
      LIter, LIdx: Integer;
    begin
      Result := 0;
      for LIter := 1 to ALoops do
        for LIdx := 0 to High(AInputs) do
          if LRegisteredCS.Match(AInputs[LIdx]) then
            Inc(Result);
    end);

  TimeVariant('ad-hoc CS', LTotalOps, LExpectedTotal,
    function: Int64
    var
      LIter, LIdx: Integer;
    begin
      Result := 0;
      for LIter := 1 to ALoops do
        for LIdx := 0 to High(AInputs) do
          if LAdhocCS.Match(AInputs[LIdx], LPatterns) then
            Inc(Result);
    end);
end;

procedure RunBenchmarkScenarios;
var
  LBulkInputs: TArray<string>;
  LAltInputs: TArray<string>;
  LWorstInput: TArray<string>;
  LIdx: Integer;
begin
  // Bulk inputs: 5% 'MatchMe_####.pas', 95% 'Skip_####.txt' (same shape
  // as the DUnitX speed fixtures).  Case matches the patterns exactly so
  // CS and CI expect the same counts.
  SetLength(LBulkInputs, BULK_INPUT_COUNT);

  for LIdx := 0 to (BULK_INPUT_COUNT div 20) - 1 do
    LBulkInputs[LIdx] := Format('MatchMe_%.4d.pas', [LIdx]);

  for LIdx := BULK_INPUT_COUNT div 20 to BULK_INPUT_COUNT - 1 do
    LBulkInputs[LIdx] := Format('Skip_%.4d.txt', [LIdx]);

  // Quoted-alt inputs: 5% under a 3rdparty folder, 95% elsewhere.
  SetLength(LAltInputs, BULK_INPUT_COUNT);

  for LIdx := 0 to (BULK_INPUT_COUNT div 20) - 1 do
    LAltInputs[LIdx] := Format('docs\3rdparty\notes_%.4d.md', [LIdx]);

  for LIdx := BULK_INPUT_COUNT div 20 to BULK_INPUT_COUNT - 1 do
    LAltInputs[LIdx] := Format('docs\internal\notes_%.4d.txt', [LIdx]);

  LWorstInput := TArray<string>.Create(WORST_CASE_LONG_PATH);

  RunScenario('S1 single mask *.pas', LBulkInputs,
    TArray<string>.Create('*.pas'),
    BULK_LOOPS, BULK_INPUT_COUNT div 20);

  RunScenario('S2 8-mask ignore set', LBulkInputs,
    TArray<string>.Create('*.pas', '*.dpr', '*.dpk', '*.inc', '*.dfm', '*.res', '*.dproj', '*.groupproj'),
    BULK_LOOPS, BULK_INPUT_COUNT div 20);

  RunScenario('S3 worst-case backtracking', LWorstInput, TArray<string>.Create(WORST_CASE_COMPLEX_PATTERN), WORST_CASE_LOOPS, 0);

  RunScenario('S4 quoted-alt mask', LAltInputs, TArray<string>.Create('*<"3rdparty"|"ThirdParty">*.md'), BULK_LOOPS, BULK_INPUT_COUNT div 20);
end;

end.
