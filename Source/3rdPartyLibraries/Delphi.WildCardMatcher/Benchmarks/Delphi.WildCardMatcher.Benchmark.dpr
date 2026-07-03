program Delphi.WildCardMatcher.Benchmark;

// Permanent performance harness for Delphi.WildCardMatcher.
//
// Runs a parity suite (registered/compiled engine vs ad-hoc/interpreting
// engine, CI and CS - must agree on every edge case) and then times both
// engines over realistic scenarios, best-of-rounds.  Output goes to the
// console and to WildCardMatcherBenchmarkResults.txt next to the exe.
//
// The suite itself lives in Delphi.WildCardMatcher.Benchmarks.pas, shared
// with Baseline\Delphi.WildCardMatcher.BaselineBenchmark.dpr which binds
// the SAME suite against the frozen pre-optimization unit (commit
// 0e83678) - so any run can be compared against the original engine on
// the same machine.  Historical results live in RESULTS.md.
//
// Build & run (optimization ON - do not benchmark Debug builds):
//   "%ProgramFiles(x86)%\Embarcadero\Studio\37.0\bin\rsvars.bat"
//   dcc32 -B -$O+ -$R- -$Q- -E"Win32" -N"Win32" Delphi.WildCardMatcher.Benchmark.dpr
//   Win32\Delphi.WildCardMatcher.Benchmark.exe

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  Delphi.WildCardMatcher in '..\Source\Delphi.WildCardMatcher.pas',
  Delphi.WildCardMatcher.Benchmark.Types in 'Delphi.WildCardMatcher.Benchmark.Types.pas',
  Delphi.WildCardMatcher.Benchmark.Utils in 'Delphi.WildCardMatcher.Benchmark.Utils.pas',
  Delphi.WildCardMatcher.Benchmarks in 'Delphi.WildCardMatcher.Benchmarks.pas';

begin
  try
    RunParitySuite;
    RunBenchmarkScenarios;

    Log('');
    Log('Benchmark complete.');
    Log('Results saved to: %s', [LogFileName]);
  except
    on E: Exception do
    begin
      WriteLn(E.ClassName, ': ', E.Message);
      Halt(1);
    end;
  end;
end.
