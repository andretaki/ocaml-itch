#!/usr/bin/env python3
"""Independent order book replay, for cross-checking `itch book -tops`.

Written from the ITCH 5.0 specification rather than from the OCaml, and emits
the same CSV so the two can be diffed directly:

    dune exec bin/itch.exe -- book data/prefix.itch50 -tops | tail -n +6 > a.csv
    python3 test/cross_check_book.py data/prefix.itch50 > b.csv
    diff a.csv b.csv

Columns: locate, bid_price_raw, bid_shares, ask_price_raw, ask_shares.
"""
import sys
from collections import defaultdict


def u(b: bytes) -> int:
    return int.from_bytes(b, "big")


def main(path: str) -> None:
    data = open(path, "rb").read()
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
        length = u(data[pos : pos + 2])
        if length == 0 or pos + 2 + length > len(data):
            break
        m = data[pos + 2 : pos + 2 + length]
        t, locate = chr(m[0]), u(m[1:3])
        if t in "AF":  # 1.3
            ref, side, shares, price = u(m[11:19]), chr(m[19]), u(m[20:24]), u(m[32:36])
            orders[ref] = (locate, side, price, shares)
            adjust(locate, side, price, shares)
        elif t in "ECX":  # 1.4.1 / 1.4.2 / 1.4.3 -- all subtract a delta
            reduce_(u(m[11:19]), u(m[19:23]))
        elif t == "D":  # 1.4.4
            remove(u(m[11:19]))
        elif t == "U":  # 1.4.5 -- shares is a new total, side carries forward
            order = remove(u(m[11:19]))
            if order is not None:
                locate_, side_, _, _ = order
                new_ref, shares, price = u(m[19:27]), u(m[27:31]), u(m[31:35])
                orders[new_ref] = (locate_, side_, price, shares)
                adjust(locate_, side_, price, shares)
        pos += 2 + length

    for locate in sorted(books):
        bids, asks = books[locate]
        bid = f"{max(bids)},{bids[max(bids)]}" if bids else "-,-"
        ask = f"{min(asks)},{asks[min(asks)]}" if asks else "-,-"
        print(f"{locate},{bid},{ask}")


if __name__ == "__main__":
    main(sys.argv[1])
