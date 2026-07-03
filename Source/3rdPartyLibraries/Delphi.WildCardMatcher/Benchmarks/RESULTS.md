# Benchmark results history

Harness: `Delphi.WildCardMatcher.Benchmark.dpr` (current unit) and
`Baseline\Delphi.WildCardMatcher.BaselineBenchmark.dpr` (frozen
pre-optimization unit from commit `0e83678`).  Both share the same suite
unit (`Delphi.WildCardMatcher.Benchmarks.pas`, with helpers in
`...Benchmark.Utils.pas` / `...Benchmark.Types.pas`), so they always run
identical scenarios.  Each run also writes its output to
`WildCardMatcherBenchmarkResults.txt` next to the exe.  See the `.dpr`
headers for build/run instructions.

All runs Win32, dcc32 37.0 with `-$O+ -$R- -$Q-`.  A parity suite
(registered engine vs ad-hoc engine, CI and CS) must pass before anything
is timed.  Since 2026-07-02 (c), every variant runs **5 rounds and
reports the best round** - the machine is never idle, so single-shot
timings fluctuate; the minimum is the closest estimate of the undisturbed
cost.

**Units: ALL timings in every table below are nanoseconds (ns) per Match
call over the whole pattern set.**  1 ns = one billionth of a second;
100 ns/op = 10 million Match calls per second.

## 2026-07-03: SSE2 scan kernels (USE_SSE2 define, on by default on Win32)

The '*' first-char skip loops - the hottest loops in backtracking-heavy
patterns - got SSE kernels (`ScanCharIndex` / `ScanCharIndexCI`, 8 UTF-16
chars per step, PCMPEQW + PMOVMSKB).  On Win32 the kernels are the
DEFAULT; define `PUREPASCAL` to opt out for very old CPUs (other targets
always compile pure pascal).  Same day, same machine, best of 5; parity
suite and all 94 unit tests pass in BOTH variants.

| Scenario / variant | pure pascal | USE_SSE2 | speedup |
| --- | ---: | ---: | ---: |
| S3 worst-case, registered CI | 1274 | **602** | 2.1x |
| S3 worst-case, ad-hoc CI | 2047 | **863** | 2.4x |
| S3 worst-case, registered CS | 585 | **467** | 1.3x |
| S3 worst-case, ad-hoc CS | 785 | **499** | 1.6x |
| S1 / S2 / S4 (all variants) | - | - | flat (within noise) |

S1/S2/S4 do not exercise the skip loops (tail anchors and alt-group
scans dominate there), so the kernels neither help nor hurt them.  The
CI kernel returns CANDIDATE positions (either case variant, or any char
>= U+0080) and the caller re-verifies with the scalar Unicode check, so
semantics are bit-identical to pure pascal - including exotic mappings
like U+017F -> 'S'.

Deliberately NOT vectorized: FastToUpper / IsAsciiDigit (single-char;
Delphi cannot inline asm routines, so a call costs more than the two
compares), literal-run compares (typical runs 3-8 chars, below SIMD
break-even), FindClassEnd (ad-hoc-only cost; algorithmic caching would
beat SIMD there).  PCMPISTRI (the actual SSE4.2 string instruction) was
avoided on purpose - higher latency than compare+movemask on modern
cores.  A future AVX2 variant would widen the same kernels to 16 chars
per step; expected gain is modest since the scans are short-ish and
branch-bound.

## 2026-07-02 (c): baseline vs current, best-of-5 methodology

Baseline = commit `0e83678` ("New Syntax, EOL stuff met its End Of Life"):
full current syntax (quoted alternation included) but none of the engine
optimizations.  Current = HEAD after both optimization rounds
(interpreting-engine fast paths + compiled registered patterns).

### Registered patterns (compiled engine at HEAD) - ns/op

| Scenario | baseline CI | current CI | speedup | baseline CS | current CS | speedup |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| S1 single mask `*.pas` | 102.9 | **7.3** | 14.1x | 83.2 | **6.4** | 13.0x |
| S2 8-mask ignore set | 747.5 | **46.9** | 15.9x | 470.4 | **44.7** | 10.5x |
| S3 worst-case backtracking | 7064 | **1238** | 5.7x | 3119 | **552** | 5.6x |
| S4 quoted-alt mask | 1144 | **156** | 7.3x | 887.8 | **106** | 8.4x |

### Ad-hoc patterns (interpreting engine at HEAD) - ns/op

| Scenario | baseline CI | current CI | speedup | baseline CS | current CS | speedup |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| S1 single mask `*.pas` | 158.7 | **36.4** | 4.4x | 73.1 | **23.8** | 3.1x |
| S2 8-mask ignore set | 1085 | **203** | 5.3x | 446.1 | **94.1** | 4.7x |
| S3 worst-case backtracking | 7187 | **1974** | 3.6x | 3105 | **810** | 3.8x |
| S4 quoted-alt mask | 1223 | **904** | 1.4x | 886.5 | **790** | 1.1x |

(Ad-hoc CI pays a FastUpperString per pattern per call on top of
interpretation - by design; ad-hoc is the convenience path.  For repeated
matching against a fixed set, register the patterns at Create.)

What the optimizations were:

1. Interpreting-engine round (in both engines' numbers above): literal-tail
   fast path after `*` (end-anchored compare), first-char/digit skip in the
   `*` scan loop, `Char.ToUpper` instead of allocating `AnsiUpperCase`,
   ordinal-equality pre-check before `FastToUpper`, single-pass
   `FastUpperString`, negated-alternation early exit, cached lengths,
   merged class-end/kind scan.
2. Compiled-engine round (registered path only): patterns compiled to
   token programs at Create - `CompareMem` literal runs (CS), precompiled
   class/alternation descriptors, `MinRemain` pruning of `*` scans,
   whole-pattern minimum-length quick reject.

Evaluated and REJECTED: upper-casing the whole input once per Match call
("#3") - the per-call allocation made it slower than per-char ToUpper in
every realistic scenario once the pre-check and tail fast paths were in.

## Older single-shot runs (methodology differs - do not compare directly)

### 2026-07-02 (b): #11 integrated - compiled engine ships in TWildCard (ns/op)

| Scenario | registered CI | ad-hoc CI | registered CS | ad-hoc CS |
| --- | ---: | ---: | ---: | ---: |
| S1 single mask `*.pas` | 8.7 | 38.7 | 6.8 | 24.9 |
| S2 8-mask ignore set | 49.8 | 216.9 | 46.8 | 98.5 |
| S3 worst-case backtracking | 1322 | 2090 | 597 | 850 |
| S4 quoted-alt mask | 165 | 942 | 111 | 797 |

### 2026-07-02 (a): prototype evaluation of candidates #3 and #11 (ns/op)

| Scenario | current | #3 | #11 +prep | #11 perchar |
| --- | ---: | ---: | ---: | ---: |
| S1 single mask `*.pas` | 23.5 | 50.1 | 38.7 | 7.3 |
| S2 8-mask ignore set | 98.5 | 113.3 | 75.2 | 53.1 |
| S3 worst-case backtracking (CI) | 2000 | 1147 | 872 | 1284 |
| S3 worst-case backtracking (CS) | 820 | - | 570 | - |
| S4 quoted-alt mask | 830 | 918 | 194 | 225 |

"current" here = after optimization round 1, before the compiled engine.
This run led to rejecting #3 and integrating #11 with a per-char CI
engine.
