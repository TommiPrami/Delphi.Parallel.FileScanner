unit Delphi.WildCardMatcher.Benchmark.Types;

// Shared types and configuration for the WildCardMatcher benchmark.

interface

const
  // Every timed variant runs this many rounds and reports the BEST round -
  // the machine is never idle, so single-shot timings fluctuate; the
  // minimum is the closest estimate of the undisturbed cost.
  BENCH_ROUNDS = 5;

type
  // One timed benchmark body: performs all its Match calls and returns the
  // total match count (validated against the expected count every round).
  TBenchRun = reference to function: Int64;

implementation

end.
