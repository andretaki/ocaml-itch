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

dune exec bin/itch.exe -- checksum data/prefix.itch50 -repeats 7
./bench/cpp/baseline data/prefix.itch50 7
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
