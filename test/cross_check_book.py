#!/usr/bin/env python3
"""An independent order book replay, used to check `itch book -tops`.

Written from the ITCH 5.0 specification rather than from `lib/itch/order_book.ml`,
so agreement is evidence rather than a tautology. Reconstructing a book is where
a parser stops being a decoder and starts being a model of the market, and it is
the step where a wrong reading of the spec produces something that still looks
like a book:

  * a modify message carries only an order reference -- not the side, symbol or
    price -- so both implementations must remember every live order, and a
    modify for a reference never seen cannot be applied at all;
  * a cancel carries shares to *subtract*, a replace carries the new *total*,
    and the two messages otherwise look alike;
  * a replace carries side and symbol forward from the order it replaces,
    because the spec does not repeat them.

Get any of those backwards and you get a plausible, wrong book. Neither the
round trip nor the aggregate cross-check can see it: they are per-message, and
this is the only stateful check in the project.

    ./_build/default/bin/itch.exe book FILE -tops > /tmp/ml.txt
    python3 test/cross_check_book.py FILE --expect /tmp/ml.txt

With --expect it exits non-zero on disagreement and names the locates that
differ, rather than printing a CSV for a human to diff. Without it, it prints
that CSV: locate, bid_price_raw, bid_shares, ask_price_raw, ask_shares.
"""
import sys
from collections import defaultdict

# `itch book -tops` prints a variable-length preamble -- the invariant failure
# is a sexp and wraps -- so the CSV is found by this sentinel and not by a line
# count. Counting lines swept three lines of a wrapped error into the data the
# first time these two were compared.
TOPS_SENTINEL = "-- tops --"


def u(b: bytes) -> int:
    return int.from_bytes(b, "big")


def replay(data: bytes) -> dict:
    """Return {locate: "bid_price,bid_shares,ask_price,ask_shares"}."""
    orders = {}  # order_ref -> (locate, side, price, shares)
    books = defaultdict(lambda: (defaultdict(int), defaultdict(int)))

    def adjust(locate, side, price, delta):
        bids, asks = books[locate]
        levels = bids if side == "B" else asks
        levels[price] += delta
        if levels[price] <= 0:
            del levels[price]

    def reduce_(ref, shares):
        order = orders.get(ref)
        if order is None:
            return
        locate, side, price, have = order
        adjust(locate, side, price, -shares)
        if have - shares <= 0:
            del orders[ref]
        else:
            orders[ref] = (locate, side, price, have - shares)

    def remove(ref):
        order = orders.pop(ref, None)
        if order is None:
            return None
        locate, side, price, shares = order
        adjust(locate, side, price, -shares)
        return order

    pos = 0
    while pos + 2 <= len(data):
        length = u(data[pos:pos + 2])
        if length == 0 or pos + 2 + length > len(data):
            break
        m = data[pos + 2:pos + 2 + length]
        t = chr(m[0])
        if t in ("A", "F"):  # 4.3.1 / 4.3.2
            locate = u(m[1:3])
            ref, side, shares, price = u(m[11:19]), chr(m[19]), u(m[20:24]), u(m[32:36])
            orders[ref] = (locate, side, price, shares)
            adjust(locate, side, price, shares)
        elif t in ("E", "C", "X"):  # 4.4.1 / 4.4.2 / 4.4.3 -- all subtract a delta
            reduce_(u(m[11:19]), u(m[19:23]))
        elif t == "D":  # 4.4.4
            remove(u(m[11:19]))
        elif t == "U":  # 4.4.5 -- shares is a new total, side and symbol carry forward
            order = remove(u(m[11:19]))
            if order is not None:
                locate_, side_, _, _ = order
                new_ref, shares, price = u(m[19:27]), u(m[27:31]), u(m[31:35])
                orders[new_ref] = (locate_, side_, price, shares)
                adjust(locate_, side_, price, shares)
        pos += 2 + length

    tops = {}
    for locate in sorted(books):
        bids, asks = books[locate]
        bid = f"{max(bids)},{bids[max(bids)]}" if bids else "-,-"
        ask = f"{min(asks)},{asks[min(asks)]}" if asks else "-,-"
        tops[locate] = f"{bid},{ask}"
    return tops


def parse_expected(text: str) -> dict:
    lines = text.splitlines()
    if TOPS_SENTINEL in lines:
        lines = lines[lines.index(TOPS_SENTINEL) + 1:]
    tops = {}
    for line in lines:
        parts = line.split(",")
        if len(parts) != 5 or not parts[0].isdigit():
            continue  # the -symbol depth dump, if it was asked for
        tops[int(parts[0])] = ",".join(parts[1:])
    return tops


def compare(mine: dict, theirs: dict) -> int:
    locates = sorted(set(mine) | set(theirs))
    bad = [
        (locate, mine.get(locate, "(no book)"), theirs.get(locate, "(no book)"))
        for locate in locates
        if mine.get(locate) != theirs.get(locate)
    ]
    if not bad:
        print(f"cross_check_book: agrees on all {len(locates)} books, both sides")
        return 0
    print(
        f"cross_check_book: DISAGREES on {len(bad)} of {len(locates)} books",
        file=sys.stderr,
    )
    print("  locate  python (bid_px,bid_sz,ask_px,ask_sz)  |  expected", file=sys.stderr)
    for locate, a, b in bad[:20]:
        print(f"  {locate:<6}  {a}  |  {b}", file=sys.stderr)
    if len(bad) > 20:
        print(f"  ... and {len(bad) - 20} more", file=sys.stderr)
    return 1


def main(argv: list) -> int:
    args = argv[1:]
    expect_path = None
    if "--expect" in args:
        i = args.index("--expect")
        expect_path = args[i + 1]
        args = args[:i] + args[i + 2:]
    if len(args) != 1:
        print("usage: cross_check_book.py FILE [--expect FILE]", file=sys.stderr)
        return 2

    with open(args[0], "rb") as f:
        tops = replay(f.read())

    if expect_path is None:
        for locate in sorted(tops):
            print(f"{locate},{tops[locate]}")
        return 0

    with open(expect_path) as f:
        expected = parse_expected(f.read())
    if not expected:
        print(
            f"cross_check_book: no book rows found in {expect_path}; "
            f"was `itch book` run with -tops?",
            file=sys.stderr,
        )
        return 2
    return compare(tops, expected)


if __name__ == "__main__":
    sys.exit(main(sys.argv))
