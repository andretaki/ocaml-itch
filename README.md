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
- **Independent cross-check, at field level.** `test/cross_check.py` is a naive
  reimplementation written from the spec, not from the OCaml. On 254,895 real messages it
  agrees exactly on message counts, byte offsets, timestamps, share totals and notional.
  That last one is a sum of products of two independently decoded fields across every Add
  Order in the file, so any single-byte offset error changes it.
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
- **The whole book, cross-checked.** An independent order book implementation replays the
  same file and dumps best bid and ask for every symbol. All 700 books agree exactly, on
  both sides, in price and size.
- **Invariants over real data.** Shares resting at price levels and shares recorded
  against live orders are maintained by different code paths; the replay checks that the
  two agree, per symbol, at the end of the file.

## Performance

Two decode paths. `Parser.parse_exn` returns a `Message.t` you can pattern match on, and
allocates to do it. `Reader.Make (H)` dispatches into a handler whose callbacks take only
immediates, and allocates nothing per message.

On 254,895 messages of real exchange data (Intel Core Ultra 7 258V, WSL 2, fastest of 7):

| | throughput | allocation |
|---|---|---|
| `Reader.Make` (handler) | ~172 M msg/s | **0 words per message** |
| `Reader.iter` (`Message.t`) | ~49 M msg/s | 28 words per message |
| C++ baseline, `-O3 -march=native` | ~317 M msg/s | n/a |

The C++ program computes the same aggregate over the same file and prints it; the two
lines must be byte-identical or the comparison is void. See [bench/README.md](bench/README.md)
for the method and the caveats — including that the OCaml switch here has no flambda.

**Getting to zero allocation was not where I expected.** It was not the records or the
symbol strings. Core's `Bigstring.get_uint32_be` converts through a boxed `Int32.t`:

```ocaml
let get_uint32_be t ~pos = uint32_of_int32_t (get_int32_t_be t ~pos)
```

That is three words on every call, and ITCH reads a 32-bit field — a share count, a price,
the low half of a timestamp — in nearly every message. `get_int32_be` reads straight to an
unboxed `int`, and masking recovers the unsigned value exactly, including above 2^31. That
one substitution took the handler path from 6 words per message to 0.

The other one was a local `check` function inside the dispatch loop that captured the
message length. A closure is a heap allocation, so that was one per message.

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

## Quick start

```bash
opam install . --deps-only
dune build
dune runtest

# Nasdaq publishes full trading days, free, at https://emi.nasdaq.com/ITCH/Nasdaq ITCH/
dune exec bin/itch.exe -- stats data/prefix.itch50
dune exec bin/itch.exe -- dump data/prefix.itch50 -n 5
```

## Roadmap

1. The remaining message types (`P` `Q` `B` `H` `Y` `L` `N`).
3. Zero allocation on the hot path, with benchmarks.
4. An [OxCaml](https://oxcaml.org/) branch using unboxed layouts and stack allocation,
   where `[@zero_alloc]` lets the compiler *prove* the parse path never allocates.

## License

MIT
