# Benchmarks

## What is being measured

Both programs mmap the same file, walk the same 2-byte length framing, decode the same
nine message types, and fold every decoded field into the same accumulators using the same
arithmetic. Then both print the aggregate.

**If the two aggregate lines are not byte-identical, the comparison is void.** That check
is the point of the design. Without it there is no way to know whether the two programs
did the same work — or whether a compiler noticed the decoded fields were never observed
and deleted the decoding, which is the classic way a parser benchmark reports a number for
an empty loop.

## Running it

```bash
g++ -O3 -march=native -std=c++20 -o bench/cpp/baseline bench/cpp/baseline.cpp

# A corpus large enough to measure: 6.9 MB fits in L3 and is 92% undecoded
# message types, both of which flatter the result. See "Why the corpus changed".
head -c 1000000000 data/01302020.NASDAQ_ITCH50 > data/prefix1g.itch50

dune build --profile release bin/itch.exe
./_build/default/bin/itch.exe checksum data/prefix1g.itch50 -repeats 7
./bench/cpp/baseline data/prefix1g.itch50 7
```

Compare the first line of each. They must match.

## Method

- **Fastest of N**, not a mean: the distribution is one-sided, since noise only ever makes
  a run slower.
- **Warm cache.** Both programs do one untimed pass first. What is being measured is parse
  throughput, not disk speed. End-to-end wall clock on a cold full-day file is a different
  number and is reported separately — it is I/O bound, and on this machine the file is
  larger than RAM.
- **Same machine, same file, same session.** Cross-machine numbers are not comparable.
- Run-to-run spread is reported rather than hidden. OCaml's is wider than C++'s here.

## Caveats, stated rather than buried

- Measured under **WSL 2**, which is noisier than bare metal.
- The OCaml switch used for the headline number is `ocaml-base-compiler`, which does
  **not** have flambda. A flambda switch is the fairer baseline and is measured separately;
  comparing a flambda2-based OxCaml build against a non-flambda vanilla build would
  overstate what OxCaml contributed.
- `bench/cpp/baseline.cpp` is written to do exactly this work and nothing else. It is a
  controlled comparison, not a general-purpose ITCH library, and it does not validate its
  input beyond the length check.

## Results

2026-08-20. Intel Core Ultra 7 258V, 8 cores, WSL 2, 23 GB RAM. Corpus is the first
1,000,000,000 bytes of `01302020.NASDAQ_ITCH50` — 31,762,048 messages, warm in page cache.
All OCaml configurations built with `dune --profile release`. Three independent rounds per
configuration, each reporting fastest-of-7.

| configuration | round bests (M msg/s) | range |
|---|---|---|
| `ocaml-base-compiler.5.2.1`, no flambda | 64.3, 61.8, 66.7 | 61.8 – 66.7 |
| `5.2.1+flambda` | 69.5, 66.0, 65.9 | 65.9 – 69.5 |
| `5.2.0+ox` (OxCaml) | 71.2, 72.0, 76.5 | 71.2 – 76.5 |
| C++ `-O3 -march=native` | 114.7, 112.4, 112.0 | 112.0 – 114.7 |

All four printed a byte-identical aggregate, so the comparison stands.

**What these numbers do and do not support.** Within a single round the spread between
fastest and slowest pass ran from +8% to +34%. That is the same size as the gap between
the non-flambda and flambda configurations, so **those two are not distinguishable here**
and it would be wrong to claim flambda is faster on this evidence. OxCaml's range does not
overlap the non-flambda baseline's, so that difference is real, and C++ is ahead of every
OCaml configuration by a margin far larger than the noise — roughly 1.5×.

**The OxCaml column moves two variables at once.** That switch also runs
`core v0.18~preview.130.106+341` where the others run `v0.17.x`, because the v0.18 preview
line is what the OxCaml opam repository ships. So its advantage is "OxCaml plus a newer
Core", not the compiler alone. Separating them would mean getting v0.18~preview onto a
vanilla flambda switch, which has not been done.

### Why the corpus changed

Earlier rounds used `prefix.itch50`, 6.9 MB. That corpus is misleading in two ways, and
both flatter the parser:

- **It is not representative.** 234,531 of its 254,895 messages are types the parser does
  not decode, so 92% of the measurement was the `on_other` path — a length check and a
  counter bump. The 1 GB corpus is 13.8M adds, 11.8M deletes and 2.8M replaces against
  1.4M others, which is what decoding a real session actually costs.
- **It fits in cache.** 6.9 MB sits in L3; 1 GB does not. Reported throughput on the small
  corpus was around 160–180 M msg/s, roughly 2.5× the honest figure, because it was never
  touching memory.

### A timing bug worth recording

The harness used `Core_unix.gettimeofday`, which is wall-clock and not monotonic. Under
WSL 2 it can step backwards, and a backwards step in a fastest-of-N loop is not a small
error — it yields a *negative* elapsed time that then beats every real sample. One round
printed `fastest of 7 -1.8768 s (-16.92 M msg/s)` before this was found. Timing now uses
`Clock.gettime Monotonic`, non-positive samples are discarded and counted out loud, and
the run-to-run spread is printed rather than hidden behind the best number.
