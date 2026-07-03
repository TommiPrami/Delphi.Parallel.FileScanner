program Delphi.WildCardMatcher.BaselineBenchmark;

// BASELINE benchmark: runs the exact same suite as the main benchmark
// (shared Delphi.WildCardMatcher.Benchmarks.pas) against the FROZEN
// pre-optimization matcher from commit 0e83678 ('New Syntax, EOL stuff
// met its End Of Life') that lives next to this file.  That commit has
// the full current syntax (quoted alternation included) but none of the
// engine optimizations, so it is the reference point for all speedup
// claims in RESULTS.md.
//
// The 'in' path below binds Delphi.WildCardMatcher to the frozen copy;
// the shared benchmark units are recompiled against it.
//
// Do NOT edit Baseline\Delphi.WildCardMatcher.pas - it is a historical
// snapshot.
//
// Build & run:
//   "%ProgramFiles(x86)%\Embarcadero\Studio\37.0\bin\rsvars.bat"
//   dcc32 -B -$O+ -$R- -$Q- -E"Win32" -N"Win32" Delphi.WildCardMatcher.BaselineBenchmark.dpr
//   Win32\Delphi.WildCardMatcher.BaselineBenchmark.exe

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  Delphi.WildCardMatcher in 'Delphi.WildCardMatcher.pas',
  Delphi.WildCardMatcher.Benchmark.Types in '..\Delphi.WildCardMatcher.Benchmark.Types.pas',
  Delphi.WildCardMatcher.Benchmark.Utils in '..\Delphi.WildCardMatcher.Benchmark.Utils.pas',
  Delphi.WildCardMatcher.Benchmarks in '..\Delphi.WildCardMatcher.Benchmarks.pas';

begin
  try
    RunParitySuite;
    RunBenchmarkScenarios;
  except
    on E: Exception do
    begin
      WriteLn(E.ClassName, ': ', E.Message);
      Halt(1);
    end;
  end;
end.
