#!/usr/bin/env python3
"""Independent cross-check of the OCaml parser against the same file.

Deliberately a naive reimplementation written from the ITCH 5.0 specification
rather than from the OCaml source, so that agreement between the two is evidence
rather than a tautology.

Compares message counts *and* decoded field values. The notional total is the
sharpest of these: it is a sum of products of two independently decoded fields
across every Add Order in the file, so a single-byte offset error anywhere in
the header or the price field changes it.

    dune exec bin/itch.exe -- stats data/prefix.itch50
    python3 test/cross_check.py data/prefix.itch50
"""
import sys
from collections import Counter


def u(b: bytes) -> int:
    return int.from_bytes(b, "big")


def main(path: str) -> None:
    data = open(path, "rb").read()
    pos, counts, n = 0, Counter(), 0
    first_ts = last_ts = None
    added = executed = cancelled = notional = max_ref = 0

    while pos + 2 <= len(data):
        length = u(data[pos : pos + 2])
        if length == 0 or pos + 2 + length > len(data):
            break
        m = data[pos + 2 : pos + 2 + length]
        t = chr(m[0])
        counts[t] += 1
        n += 1

        if t in "SRAFECXDU":
            ts = u(m[5:11])
            if first_ts is None:
                first_ts = ts
            last_ts = ts

        if t in "AF":  # 1.3.1 / 1.3.2
            shares, price = u(m[20:24]), u(m[32:36])
            added += shares
            notional += shares * price
            max_ref = max(max_ref, u(m[11:19]))
        elif t in "EC":  # 1.4.1 / 1.4.2
            executed += u(m[19:23])
            max_ref = max(max_ref, u(m[11:19]))
        elif t == "X":  # 1.4.3
            cancelled += u(m[19:23])
            max_ref = max(max_ref, u(m[11:19]))
        elif t == "D":  # 1.4.4
            max_ref = max(max_ref, u(m[11:19]))
        elif t == "U":  # 1.4.5
            max_ref = max(max_ref, u(m[11:19]), u(m[19:27]))

        pos += 2 + length

    print(f"messages        {n}")
    print(f"consumed        {pos} bytes ({len(data) - pos} unconsumed tail)")
    print(f"first timestamp {first_ts}")
    print(f"last timestamp  {last_ts}")
    print(f"added shares    {added}")
    print(f"executed shares {executed}")
    print(f"cancelled share {cancelled}")
    print(f"add notional    {notional}")
    print(f"max order ref   {max_ref}")
    print()
    for message_type in sorted(counts):
        print(f"  {message_type} {counts[message_type]}")


if __name__ == "__main__":
    main(sys.argv[1])
