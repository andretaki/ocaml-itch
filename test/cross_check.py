#!/usr/bin/env python3
"""Independent cross-check of the OCaml parser's framing and message counts.

Deliberately a naive reimplementation written from the specification rather than
from the OCaml source, so that agreement between the two is evidence rather than
a tautology. Run against the same file as `itch stats` and compare.

    python3 test/cross_check.py data/prefix.itch50
"""
import sys
from collections import Counter


def main(path: str) -> None:
    data = open(path, "rb").read()
    pos, counts, n = 0, Counter(), 0
    first_ts = last_ts = None
    while pos + 2 <= len(data):
        length = int.from_bytes(data[pos : pos + 2], "big")
        if length == 0 or pos + 2 + length > len(data):
            break
        body = data[pos + 2 : pos + 2 + length]
        counts[chr(body[0])] += 1
        n += 1
        if body[0:1] in (b"S", b"R", b"A", b"F"):
            ts = int.from_bytes(body[5:11], "big")
            if first_ts is None:
                first_ts = ts
            last_ts = ts
        pos += 2 + length
    print(f"messages: {n}  consumed: {pos}  tail: {len(data) - pos}")
    print(f"first_ts: {first_ts}  last_ts: {last_ts}")
    for message_type in sorted(counts):
        print(f"  {message_type} {counts[message_type]}")


if __name__ == "__main__":
    main(sys.argv[1])
