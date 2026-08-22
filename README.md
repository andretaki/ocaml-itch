# ocaml-itch

A parser for the **Nasdaq TotalView-ITCH 5.0** binary market data protocol, in OCaml.

ITCH is the feed Nasdaq uses to disseminate every order added to, removed from, and
executed on the exchange. Parsers exist in C++, Python, R, Rust and on FPGA. As far as I
can tell there was no OCaml one, so this is that.

## Status

Early. The framing, the domain types and the nine message types that drive an order
book are done and verified against real exchange data; the remaining message types are surfaced as `Unparsed` rather
than skipped silently, so a fold over a full trading day completes and reports honestly
what it did not decode.

| Message | Type | Status |
|---|---|---|
| System Event | `S` | decoded |
| Stock Directory | `R` | decoded |
| Add Order | `A` | decoded |
| Add Order with MPID | `F` | decoded |
| Order Executed | `E` | decoded |
| Order Executed with Price | `C` | decoded |
| Order Cancel | `X` | decoded |
| Order Delete | `D` | decoded |
| Order Replace | `U` | decoded |
| Trade / Cross Trade / Broken Trade | `P` `Q` `B` | surfaced as `Unparsed` |
| Trading Action / Reg SHO / Market Participant | `H` `Y` `L` | surfaced as `Unparsed` |
| everything else | | surfaced as `Unparsed` |

## Design notes

**Every scalar is an abstract type.** On the wire, prices, share counts, order reference
numbers and locate codes are all just big-endian integers, and nothing but a type system
stops you from adding a price to a share count. `Price.t`, `Shares.t`, `Order_ref.t`,
`Locate.t` and `Timestamp.t` are separate abstract types in `types.mli`, each with an
explicit constructor.

**Prices are fixed point, never float.** The spec's `Price (4)` is stored as the raw
integer with four implied decimals. $123.45 has no exact binary float representation, and
a market data parser is the last place you want to introduce that error.

**64-bit ids fail loudly.** Order reference and match numbers are `uint64` on the wire,
which does not fit OCaml's 63-bit native `int`. Rather than truncate silently — which
would corrupt an order book in a way that is near impossible to trace back — the parser
raises if the top bit is ever set.

**Enums are modelled, not left as chars.** Every code table in the spec (market category,
financial status, authenticity, LULD tier, system event codes) is a variant. The
`Y`/`N`/`<space>` fields become `bool option`, because "not available" is genuinely
different from "no" — IPO Flag is a space for every non-Nasdaq-listed issue.

## Verification

The parser is checked against ground truth, not against itself:

- **Real exchange bytes.** The expect tests parse the literal first messages of
  `01302020.NASDAQ_ITCH50` as served by `emi.nasdaq.com`.
- **Framing verified empirically.** The 2-byte big-endian length prefix is a property of
  the downloadable file distribution, not of the ITCH payload spec (which is carried over
  MoldUDP64 / SoupBinTCP). It is confirmed against real data rather than quoted.
- **Three implementations, one aggregate, checked in CI.** `test/cross_check.py` and
  `bench/cpp/baseline.cpp` each transcribe the ITCH offsets by hand from the spec, so
  neither shares an offset table with `lib/itch/wire.ml`. All three fold every decoded
  field into the same sixteen accumulators and must print the same numbers: on 254,895
  real messages, and on a generated corpus in CI on every push.

  This is the only check here that can catch a *shared misreading* of the spec. The
  round-trip property test cannot — the encoder and decoder would misread it identically
  and the round trip would still close. Measured: swapping the two order-reference
  offsets in `wire.ml` and `encoder.ml` together leaves the round trip, the differential
  test, the order book and the allocation test all green, and both independent
  implementations catch it. `cross_check.py` reports *which* accumulators moved rather
  than that a line differs, which names the message type and field to go and look at.

  CI also perturbs the Python and requires the comparison to notice, because an agreement
  check that has never been seen to disagree is not evidence that it can.
- **Malformed input is refused, by all three, and CI proves it.** The framing is free to
  declare a length that is too short for the message type it announces. Reading the
  header anyway runs off the end of the message — and off the end of the mapping, when it
  is the last message in a page-aligned file. Every implementation here has had that bug:
  the OCaml read the header before the type was known, then read it before the length
  check ran, and the C++ baseline did both until it was given a length check at all. CI
  generates the case and requires all three to reject the file.
- **48-bit timestamp assembly** is verified separately — it is the arithmetic most likely
  to be subtly wrong and silently corrupt everything downstream.
- **Spec-derived vectors.** Synthetic messages built by hand from the offset tables in
  the specification's sections 1.3 and 1.4.
- **Round-trip property test.** 20,000 generated messages per run go `encode → frame →
  decode` and must come back identical, which reaches field values no hand-written vector
  would (64-bit order references near the native `int` ceiling, full-width `uint32`
  prices). The generators emit wire-legal values only — every bound is a field width from
  the spec — because a generator that emits an illegal value fails the round trip without
  either side being wrong. The test is checked for teeth: injecting a one-byte offset
  error into the encoder makes it fail with a counterexample.
- **The whole book, cross-checked, in CI.** `test/cross_check_book.py` replays the same
  bytes with its own order book, written from the spec, and asserts on best bid and ask
  for every symbol. All 700 books on real data agree exactly, on both sides, in price and
  size. This is the only stateful check here, and it is the one the per-message checks
  cannot stand in for: a modify carries an order reference and nothing else, so both
  implementations have to remember every live order, carry side and symbol forward across
  a replace, and tell "subtract these shares" from "this is the new total". Each of those
  produces a book that balances and is wrong.

  CI runs it on a generated order flow, because the corpus used for the aggregate is not
  one. That corpus comes from the round-trip generators, whose order references are
  random: 12,124 of its 14,921 book messages are modifies for orders that never existed.
  `gen_corpus -book` keeps the model the book keeps instead — every modify names a live
  order, and no execution takes more shares than the order has left — so the replay is a
  test rather than a coincidence. CI then drops the carry-forward of side from replace and
  requires the comparison to notice.
- **Invariants over real data, and the exit status now says so.** Shares resting at price
  levels and shares recorded against live orders are maintained by different code paths;
  the replay checks that the two agree, per symbol, at the end of the file. It used to
  print `invariants FAILED` and exit 0, which meant nothing could gate on it. It exits
  non-zero now, and CI gates on it.
- **Decoded prices checked against the real world.** The sharpest check turns out to be the
  cheapest: replay the session and compare a few reconstructed books against what those
  stocks actually traded at. On 2020-01-30 this parser reconstructs AAPL at 321.01/321.79,
  MSFT at 170.78/174.59 and F at 8.75/9.29 — matching the real market across two orders of
  magnitude, with the bid below the ask in every book. That single check exercises the
  price offset, the four-decimal fixed point, side decoding, share counts and the
  locate-to-symbol mapping at once, and unlike every other check here its ground truth is
  external to the project rather than another program written by the same author.

  ```bash
  dune exec bin/itch.exe -- book data/prefix.itch50 -symbol AAPL -levels 3
  ```

- **A full trading session, replayed.** The whole of `01302020.NASDAQ_ITCH50` — 12,952,050,754
  bytes, 423,285,709 messages, 417,219,234 of them book-affecting — replays with
  `invariants ok`, consuming the file to the byte with no unconsumed tail. Two numbers
  there are worth more than the pass itself: **orphans 0**, meaning no modify ever
  referenced an order the book had not seen, and **live orders 0**, meaning the book
  unwinds exactly to empty by the end of the day. A pre-market prefix cannot exercise
  either, since it neither opens nor closes.
- **The session analysis has its own third implementation.**
  `test/cross_check_lifetime.py` decodes the same bytes from the spec tables with its own
  hand-transcribed offsets and computes the same fifteen exact integers: message and
  decode counts, the four terminal causes, the exact sum, minimum and maximum of every
  order lifetime, and the busiest millisecond. It agrees with the OCaml across the entire
  12.95 GB session — all fifteen fields, 423,285,709 messages.

  What it compares is deliberately *not* the percentile table. Percentiles come out of a
  bucketed histogram, so two correct implementations may legitimately disagree on one, and
  a comparison that tolerates disagreement is not a comparison. Every field checked has
  exactly one right answer. The subtle ones are the terminal causes: a partial execution
  or partial cancel is *not* a death, so reading a share count as a new total rather than
  a delta silently moves orders between causes without raising anywhere. CI injects that
  exact mistake and requires the comparison to report it.

- **An overflow the tests did not catch, and now do.** The first full-day run reported a
  **mean order lifetime of -13.6 seconds**. A day's 223 million lifetimes sum to
  4.31e19 nanoseconds and an OCaml int holds 4.6e18, so the accumulator had wrapped —
  silently, and not always to a negative: the same wrap on the `deleted` bucket alone
  turned 3.7 minutes into a plausible-looking 15.3 seconds. Every unit test passed
  throughout, because none of them summed anything that large. The total is now carried in
  two limbs of base 2^30, exactly rather than in a float, so that an independent
  implementation can still check it; and `test_histogram.ml` now records values past the
  int range and pins the exact answer.

- **The two decode paths must agree.** `Reader.Make`'s handler path and the allocating
  `Message.t` path are separate dispatch code, and both are folded into the same aggregate
  over the same bytes; a disagreement fails the build. This check was audited and found
  **blind** in one specific way, then fixed: order references were folded as
  `order_ref lxor match_number`, and because xor is commutative, swapping those two
  arguments left the aggregate bit-identical. Injecting exactly that swap at the dispatch
  site passed all 21 tests *and* produced a byte-identical C++ comparison. Order references
  now go into three separate accumulators, and the same injected swap is caught.
- **Allocation proved, not just measured.** Under OxCaml the callbacks in `Handler.S`
  carry `[@@zero_alloc]` and `Reader.Make`'s dispatch loop is checked against it, so the
  compiler rejects the build if the decode path can allocate at all. This is strictly
  stronger than the runtime test, which asserts only that allocation does not grow with
  message count — and it earned its keep immediately by catching a 64-byte closure in
  `Reader.consume` that the runtime test was blind to.

  It has now caught a second one, in the Level 4 analysis handler: `Histogram.t` is
  abstract outside its own module, so indexing a `Histogram.t array` cannot be proved not
  to be indexing a flat float array, and the compiler emitted the boxing read —
  *"allocation of 16 bytes for float"*, once per terminated order, 223 million times over
  a session. Vanilla OCaml compiled it without a word, and the existing runtime allocation
  test could not have seen it either, because that test drives the `Checksum` handler and
  not this one. Named record fields have a statically known type and read flat. There is
  now a runtime allocation test for this handler too, so both switches have something to
  say about it.

  CI enforces it, on every push to `main`, by building the OxCaml compiler from source and
  compiling with `--profile release` — the annotations are inert without it, because dune's
  dev profile passes `-opaque` and withholds the cross-module information the checker
  needs. The job then annotates a function that genuinely allocates and requires the build
  to fail, because an annotation that only ever passes proves nothing. It costs about
  twenty-three minutes, which is why pull requests skip it and merges do not.

## Performance

Two decode paths. `Parser.parse_exn` returns a `Message.t` you can pattern match on, and
allocates to do it. `Reader.Make (H)` dispatches into a handler whose callbacks take only
immediates, and allocates nothing per message.

On 31,762,048 messages of real exchange data — the first 1 GB of a live session, warm in
page cache (Intel Core Ultra 7 258V, WSL 2, `--profile release`, fastest of 7 across three
rounds):

| | throughput | allocation |
|---|---|---|
| `Reader.Make` (handler), OxCaml | 71 – 76 M msg/s | **0 words per message, compiler-proved** |
| `Reader.Make` (handler), flambda | 66 – 70 M msg/s | 0 words per message |
| `Reader.Make` (handler), no flambda | 62 – 67 M msg/s | 0 words per message |
| C++ baseline, `-O3 -march=native` | 112 – 115 M msg/s | n/a |

The C++ program computes the same aggregate over the same file and prints it; all four
lines must be byte-identical or the comparison is void. Ranges are given rather than single
figures because the run-to-run spread here is +8% to +34% — which is wide enough that the
flambda and non-flambda rows are **not** distinguishable, and saying otherwise would be
reading noise. See [bench/README.md](bench/README.md) for the method, the caveats, and why
an earlier 6.9 MB corpus produced numbers roughly 2.5× too high.

**Getting to zero allocation was not where I expected.** It was not the records or the
symbol strings. It was `Bigstring.get_uint32_be`, which converts through a boxed `Int32.t`:

```ocaml
let[@inline] unsafe_get_uint32_be t ~pos = uint32_of_int32_t (unsafe_get_int32_t_be t ~pos)
let          get_uint32_be        t ~pos = uint32_of_int32_t (get_int32_t_be        t ~pos)
```

The difference between those two lines is the `[@inline]`. Without it the boxed
intermediate survives the call, and ITCH reads a 32-bit field — a share count, a price, the
low half of a timestamp — in nearly every message:

| build | `get_uint32_be` | `unsafe_get_uint32_be` |
|---|---|---|
| `ocaml-base-compiler.5.2.1`, no flambda | **3 words/call** | 0 |
| `5.2.1+flambda`, dune dev profile | **3 words/call** | 0 |
| `5.2.1+flambda`, `dune --profile release` | **3 words/call** | 0 |
| `5.2.1+flambda`, explicit `-O3` | 0 | 0 |

Three words is exactly one boxed `Int32`. Note the third row: dune's release profile does
*not* pass `-O3`, so an ordinary flambda release build pays this too. It is not the
non-flambda-only curiosity it first looked like. `get_int32_be` reads straight to an
unboxed `int` and masking recovers the unsigned value exactly, including above 2^31. That
one substitution took the handler path from 6 words per message to 0.

The same sweep also narrows the claim in the other direction: `get_int64_be_exn`,
`get_uint64_be_exn` and their `_le` twins measure 0 words/call in every configuration
despite having the same shape and also lacking `[@inline]`, so this is about the `Int32`
path specifically and not a general pattern.

The other one was a local `check` function inside the dispatch loop that captured the
message length. A closure is a heap allocation, so that was one per message.

A third only surfaced once OxCaml was checking rather than a test measuring: `consume`'s
inner `let rec loop` closed over `state`, `buf`, `pos` and `limit`, costing a 64-byte
closure per call. Being per *call* rather than per *message*, it was invisible to a test
that asserts allocation does not grow with message count — which is the argument for
having the compiler check the property rather than only sampling it.

## Order book

`Order_book` reconstructs the book by replaying the message stream. Two things make that
less mechanical than it looks:

- The modify messages carry only an order reference — not the side, symbol or price of the
  order they modify. The book has to remember every live order, and a modify for an
  unknown reference cannot be applied at all. Starting mid-stream that is expected rather
  than exceptional, so those are counted as orphans, and the count is a diagnostic.
- The arithmetic differs between messages that look alike. A cancel carries shares to
  *subtract*; a replace carries the new *total*. Inverting either produces a book that
  looks plausible and is wrong.

```
$ dune exec bin/itch.exe -- book data/prefix.itch50 -symbol AAPL
replayed        7230995 bytes in 0.010 s
book messages   11447 applied
orphans         0 (modify for an order not seen)
live orders     3197
invariants      ok

AAPL (locate 13)
  asks (low to high)
        321.7900        30
        321.8000       200
  bids (high to low)
        321.0100        30
        321.0000        25
```

That is Apple's real pre-market book at 04:00 on 2020-01-30.

## Order lifetime and message rate

`itch analyze` replays a session and answers two questions that fall straight out of the
parser and the book, in one pass over the file. Both are measured here on the full
`01302020.NASDAQ_ITCH50` session — 12.95 GB, 423,285,709 messages, 03:02 to 20:05.

**How long does a resting order live?** The book cannot answer this: it tracks shares at a
price level and deliberately forgets when an order arrived. `Analysis` keeps a live-order
table keyed by reference and records `death - birth` when the reference leaves.

| | all terminated | executed | deleted | replaced |
|---|---|---|---|---|
| orders | 223,388,077 | 6,325,604 | 180,285,101 | 36,777,372 |
| share | | 2.83% | 80.70% | 16.46% |
| p1 | 28 µs | 32 µs | 32 µs | 18 µs |
| p10 | 557 µs | 885 µs | 606 µs | 377 µs |
| p50 | 889 ms | 6.0 s | 956 ms | 453 ms |
| p90 | 32.8 s | 2.0 min | 30.1 s | 32.8 s |
| p99 | 40.1 min | 41.2 min | 58.4 min | 16.3 min |
| mean | 3.2 min | 2.1 min | 3.7 min | 1.2 min |
| min | 157 ns | 157 ns | 246 ns | 9.7 µs |
| max | 16.0 h | 13.0 h | 16.0 h | 7.2 h |

Percentiles are histogram buckets, not exact values: the bucket holding a value is never
wider than `value / 32`, so each figure above is exact to about 3%. The mean, the min and
the max are exact.

Three things stand out.

- **Only 2.83% of orders ever execute.** Four in five are deleted outright and one in six
  is replaced. Whatever an order book is mostly doing, it is not trading.
- **The distribution has no typical value.** The median order lives 889 ms and the mean
  lives 3.2 minutes, because the tail runs to sixteen hours. Quoting either number alone
  is misleading, which is why the whole distribution is here.
- **Sub-millisecond orders are a minority, not the story.** 12.31% of orders live less
  than a millisecond and 6.30% less than 100 µs. That is a real tail — the shortest-lived
  order of the day lasted 157 nanoseconds — but it is not most of them, and the folklore
  that most orders are cancelled within a millisecond does not survive the measurement.
  Half of all orders live longer than 889 ms.
- **`X` never once ended an order.** Not in 4,990,972 Order Cancel messages across the
  whole day. NASDAQ shrinks an order with `X` and withdraws it with `D`, so the
  "cancelled" row of the table is empty on real data — which is indistinguishable from a
  broken code path until you exercise it synthetically, and `test_analysis.ml` does.

**How bad is the worst millisecond?** Every message is counted, including the types this
parser does not decode, because a feed handler has to keep up with the whole stream and
not just the part it understands.

| | |
|---|---|
| milliseconds in session | 61,346,597 |
| empty milliseconds | 33,957,504 (55.35%) |
| mean | 6.9 msg/ms |
| p90 | 17 msg/ms |
| p99 | 84 msg/ms |
| p99.9 | 344 msg/ms |
| p99.99 | 1,088 msg/ms |
| **peak** | **1,746 msg/ms**, at 16:00:00.017 |
| peak / mean | 253× |

The median millisecond of the trading day carries *no messages at all*, and the busiest
one carries 1,746 — at seventeen milliseconds past four o'clock, which is the closing
auction. A handler sized for the average is sized 253× too small. Empty milliseconds are
included in that average deliberately: a feed handler has to survive the peak whether or
not the quiet periods flatter its average.

```
$ dune exec bin/itch.exe -- analyze data/01302020.NASDAQ_ITCH50
messages        423285709
session         03:02:33.404452051 to 20:05:00.000039817
live table      1925638 of 33554432 slots used at peak (5.7%)
orphan modifies 0 (modify for an order never seen)
still resting   0 at end of file
```

`orphan modifies 0` means every one of the 230.6 million modify messages named an order the
table already held, and `still resting 0` means the table unwinds to exactly empty at the
close. Neither is testable on a prefix. The run also asserts, 223.4 million times, that no
order reference is added while another with the same reference is still live.

Three counts have to reconcile, and do, exactly:

- **References born equal references terminated.** 186,610,705 `A`/`F` adds plus
  36,777,372 `U` replacements is 223,388,077, which is the number of terminations to the
  order. Nothing was left over and nothing was counted twice.
- **Decoded messages reconcile with the type histogram.** The 417,228,156 decoded here is
  `S`+`R`+`A`+`F`+`E`+`C`+`X`+`D`+`U` from `itch stats`, to the message.
- **Both independent implementations agree.** `test/cross_check_lifetime.py` decodes the
  same file from the spec tables, with its own byte offsets, and matches all fifteen
  aggregate fields over the full session.

## Building

```bash
dune build && dune runtest
```

On the OxCaml switch, build with the release profile:

```bash
dune build --profile release && dune runtest --profile release
```

The `[@zero_alloc]` proofs need cross-module information about callees. Dune's dev
profile passes `-opaque` to speed up incremental builds, which deliberately withholds
exactly that, so the checker cannot see into a call and conservatively rejects it. The
diagnostic in that case reads `called function may allocate (direct call caml_apply2)`,
which is easy to misread as "your code allocates" when it means "I cannot see whether it
does".

## Quick start

```bash
opam install . --deps-only
dune build
dune runtest

# Nasdaq publishes full trading days, free, at https://emi.nasdaq.com/ITCH/Nasdaq ITCH/
dune exec bin/itch.exe -- stats data/prefix.itch50
dune exec bin/itch.exe -- dump data/prefix.itch50 -n 5
dune exec bin/itch.exe -- book data/prefix.itch50 -symbol AAPL
dune exec bin/itch.exe -- analyze data/prefix.itch50
```

## Roadmap

1. The remaining message types (`P` `Q` `B` `H` `Y` `L` `N`). The full-day analysis turned
   out not to need any of them: order lifetime uses only the nine already decoded, and the
   message rate counts every type through the `Unparsed` path.
2. A writeup of the vanilla-versus-OxCaml numbers with the session analysis as the payoff.

Done: zero allocation on the hot path with benchmarks; the
[OxCaml](https://oxcaml.org/) work, where `[@zero_alloc]` lets the compiler *prove* the
parse path never allocates, now enforced in CI on every push; and the full-session order
lifetime and message-rate analysis above.

## License

MIT
