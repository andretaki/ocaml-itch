#!/usr/bin/env python3
"""An independent third implementation, used to check the other two.

Deliberately a naive reimplementation written from the ITCH 5.0 specification
tables rather than from ``lib/itch/wire.ml``, so that agreement between it and
the OCaml is evidence rather than a tautology. It is the only decoder in the
project that does not share an offset table with the thing it is checking:

    OCaml Message.t path  \\
                           |-- share lib/itch/wire.ml
    OCaml zero-alloc path /
    bench/cpp/baseline.cpp -- offsets transcribed by hand
    test/cross_check.py    -- offsets transcribed by hand, from the spec

The round-trip property test cannot catch a *shared misreading* of the spec,
because the encoder and the decoder would misread it identically and the round
trip would still close. This file can: a wrong offset here disagrees, and a
wrong offset there disagrees. Measured -- swapping the two order-reference
offsets in wire.ml and encoder.ml together leaves the round trip, the
differential test, the order book and the allocation test all green, and this
file reports which two accumulators moved.

Two modes.

``checksum`` emits the byte-identical aggregate that ``itch checksum`` and
``bench/cpp/baseline`` emit, and with ``--expect`` it *asserts* rather than
prints -- exit status 1 and a field-by-field report of which accumulator
diverged. That last part is the point: a bare ``diff`` of two 16-field lines
tells you they disagree, and this tells you it was ``sum_prices``, which names
the message types and the field to go and look at.

``stats`` emits the human summary that ``itch stats`` emits.

    ./_build/default/bin/itch.exe checksum FILE | head -2 > /tmp/expected
    python3 test/cross_check.py checksum FILE --expect /tmp/expected

The arithmetic below (which fields fold into which accumulator) is shared with
the OCaml and the C++ by design -- it is the comparison protocol, the thing all
three agree to compute. What is *not* shared, and is the whole point, is every
byte offset and every message width.
"""
import sys
from collections import Counter


class SpecError(Exception):
    """A message whose framed length is not the width the spec gives for it."""


def u(b: bytes) -> int:
    return int.from_bytes(b, "big")


# Message widths, from the ITCH 5.0 specification tables. The framing length
# must equal these exactly; the OCaml raises when it does not, and so do we,
# because a decoder that reads a field out of a message too short to contain it
# is the failure mode this whole comparison exists to catch.
SPEC_LENGTH = {
    "S": 12,   # 4.1   System Event
    "R": 39,   # 4.2.1 Stock Directory
    "A": 36,   # 4.3.1 Add Order, no MPID attribution
    "F": 40,   # 4.3.2 Add Order with MPID attribution
    "E": 31,   # 4.4.1 Order Executed
    "C": 36,   # 4.4.2 Order Executed with Price
    "X": 23,   # 4.4.3 Order Cancel
    "D": 19,   # 4.4.4 Order Delete
    "U": 35,   # 4.4.5 Order Replace
}

AGGREGATE_FIELDS = (
    "messages", "adds", "executes", "cancels", "deletes", "replaces",
    "directories", "system_events", "others", "sum_shares", "sum_prices",
    "xor_order_refs", "xor_match_numbers", "xor_new_refs", "xor_timestamps",
    "max_locate",
)


class Aggregate:
    """Mirrors lib/itch/checksum.ml, field for field and operation for operation.

    Order references fold into three separate accumulators rather than one.
    That is not redundancy: xor is commutative, so a single accumulator taking
    ``order_ref ^ match_number`` is unchanged when those two arguments are
    swapped, and swapping two adjacent 8-byte fields is exactly the mistake this
    file exists to catch. See the note in checksum.ml -- a real dispatch-site
    swap was measured to leave every test green and the aggregate byte-identical
    before it was split into three.
    """

    def __init__(self) -> None:
        for field in AGGREGATE_FIELDS:
            setattr(self, field, 0)
        # Extra accumulators for `stats` mode only; not part of the aggregate.
        self.counts: Counter = Counter()
        self.first_ts = None
        self.last_ts = None
        self.added_shares = 0
        self.executed_shares = 0
        self.cancelled_shares = 0
        self.notional = 0
        self.max_order_ref = 0
        self.consumed = 0

    def note(self, stock_locate: int, timestamp: int) -> None:
        self.messages += 1
        self.xor_timestamps ^= timestamp
        if stock_locate > self.max_locate:
            self.max_locate = stock_locate
        if self.first_ts is None:
            self.first_ts = timestamp
        self.last_ts = timestamp

    def note_order_ref(self, order_ref: int) -> None:
        if order_ref > self.max_order_ref:
            self.max_order_ref = order_ref

    def line(self) -> str:
        return " ".join(f"{f}={getattr(self, f)}" for f in AGGREGATE_FIELDS)


def check_length(message_type: str, actual: int) -> None:
    expected = SPEC_LENGTH[message_type]
    if expected != actual:
        raise SpecError(
            f"ITCH message length disagrees with the spec: type {message_type!r} "
            f"spec_length={expected} framed_length={actual}"
        )


def walk(data: bytes) -> Aggregate:
    """Decode every message, folding into the aggregate.

    Framing handling mirrors Reader.consume exactly, including where it stops:
    a zero length (files are padded with zeroes after the final message) and a
    trailing partial message both end the walk without raising, leaving the
    remaining bytes unconsumed.

    Header fields are read *after* the length check in every branch, never once
    up front. The framing permits a message shorter than the 11-byte header, and
    a message whose type byte says 'A' but whose framed length is 3 still
    reaches the 'A' branch -- so reading the header before the width is checked
    reads past the end of the message either way.
    """
    a = Aggregate()
    pos, limit = 0, len(data)

    while pos + 2 <= limit:
        length = u(data[pos:pos + 2])
        if length == 0 or pos + 2 + length > limit:
            break
        m = data[pos + 2:pos + 2 + length]
        t = chr(m[0])
        a.counts[t] += 1

        if t in ("A", "F"):
            check_length(t, length)
            attributed = t == "F"
            a.note(u(m[1:3]), u(m[5:11]))
            a.adds += 1
            order_ref, side, shares, price = u(m[11:19]), m[19], u(m[20:24]), u(m[32:36])
            a.sum_shares += shares + side + (1 if attributed else 0)
            a.sum_prices += price
            a.xor_order_refs ^= order_ref
            a.added_shares += shares
            a.notional += shares * price
            a.note_order_ref(order_ref)
        elif t == "E":
            check_length(t, length)
            a.note(u(m[1:3]), u(m[5:11]))
            a.executes += 1
            order_ref, executed, match_number = u(m[11:19]), u(m[19:23]), u(m[23:31])
            a.sum_shares += executed
            a.xor_order_refs ^= order_ref
            a.xor_match_numbers ^= match_number
            a.executed_shares += executed
            a.note_order_ref(order_ref)
        elif t == "C":
            check_length(t, length)
            a.note(u(m[1:3]), u(m[5:11]))
            a.executes += 1
            order_ref, executed, match_number = u(m[11:19]), u(m[19:23]), u(m[23:31])
            printable, execution_price = m[31] == ord("Y"), u(m[32:36])
            a.sum_shares += executed + (1 if printable else 0)
            a.sum_prices += execution_price
            a.xor_order_refs ^= order_ref
            a.xor_match_numbers ^= match_number
            a.executed_shares += executed
            a.note_order_ref(order_ref)
        elif t == "X":
            check_length(t, length)
            a.note(u(m[1:3]), u(m[5:11]))
            a.cancels += 1
            order_ref, cancelled = u(m[11:19]), u(m[19:23])
            a.sum_shares += cancelled
            a.xor_order_refs ^= order_ref
            a.cancelled_shares += cancelled
            a.note_order_ref(order_ref)
        elif t == "D":
            check_length(t, length)
            a.note(u(m[1:3]), u(m[5:11]))
            a.deletes += 1
            order_ref = u(m[11:19])
            a.xor_order_refs ^= order_ref
            a.note_order_ref(order_ref)
        elif t == "U":
            check_length(t, length)
            a.note(u(m[1:3]), u(m[5:11]))
            a.replaces += 1
            original, new = u(m[11:19]), u(m[19:27])
            shares, price = u(m[27:31]), u(m[31:35])
            a.sum_shares += shares
            a.sum_prices += price
            a.xor_order_refs ^= original
            a.xor_new_refs ^= new
            a.note_order_ref(original)
            a.note_order_ref(new)
        elif t == "S":
            check_length(t, length)
            a.note(u(m[1:3]), u(m[5:11]))
            a.system_events += 1
            a.sum_shares += u(m[3:5]) + m[11]
        elif t == "R":
            check_length(t, length)
            a.note(u(m[1:3]), u(m[5:11]))
            a.directories += 1
            a.sum_shares += u(m[21:25])
        else:
            # Mirrors Checksum.on_other: counts the message but touches neither
            # the timestamp nor the locate accumulator, and reads no header
            # field -- which is what makes a sub-header-length message safe.
            a.messages += 1
            a.others += 1
            a.sum_shares += ord(t) + length

        pos += 2 + length

    a.consumed = pos
    return a


def parse_expected(text: str) -> dict:
    """Read the aggregate out of `itch checksum` / `baseline` output.

    Tolerates the extra lines both of them print (consumed, timings) so the
    expectation file can be their output verbatim rather than a doctored slice.
    """
    found = {}
    for token in text.split():
        if "=" in token:
            key, _, value = token.partition("=")
            if key in AGGREGATE_FIELDS:
                found[key] = int(value)
    for line in text.splitlines():
        parts = line.split()
        if len(parts) >= 2 and parts[0] == "consumed":
            found["consumed"] = int(parts[1])
    missing = [f for f in AGGREGATE_FIELDS if f not in found]
    if missing:
        raise SpecError(f"expectation is missing fields: {', '.join(missing)}")
    return found


def compare(a: Aggregate, expected: dict) -> int:
    fields = list(AGGREGATE_FIELDS) + (["consumed"] if "consumed" in expected else [])
    bad = [(f, getattr(a, f), expected[f]) for f in fields if getattr(a, f) != expected[f]]
    if not bad:
        print(f"cross_check: agrees on all {len(fields)} accumulators")
        return 0
    print(f"cross_check: DISAGREES on {len(bad)} of {len(fields)} accumulators", file=sys.stderr)
    width = max(len(f) for f, _, _ in bad)
    for field, mine, theirs in bad:
        print(
            f"  {field:<{width}}  python={mine}  expected={theirs}  delta={mine - theirs}",
            file=sys.stderr,
        )
    print(
        "\nEach accumulator names the fields that feed it (see checksum.ml); the\n"
        "disagreeing ones localise the fault to a message type and an offset.",
        file=sys.stderr,
    )
    return 1


def print_stats(a: Aggregate, size: int) -> None:
    print(f"messages        {a.messages}")
    print(f"consumed        {a.consumed} bytes ({size - a.consumed} unconsumed tail)")
    print(f"first timestamp {a.first_ts}")
    print(f"last timestamp  {a.last_ts}")
    print(f"added shares    {a.added_shares}")
    print(f"executed shares {a.executed_shares}")
    print(f"cancelled share {a.cancelled_shares}")
    print(f"add notional    {a.notional}")
    print(f"max order ref   {a.max_order_ref}")
    print()
    for message_type in sorted(a.counts):
        print(f"  {message_type} {a.counts[message_type]}")


def main(argv: list) -> int:
    args = argv[1:]
    if not args:
        print("usage: cross_check.py [checksum|stats] FILE [--expect FILE]", file=sys.stderr)
        return 2
    mode = args[0] if args[0] in ("checksum", "stats") else "stats"
    if args[0] in ("checksum", "stats"):
        args = args[1:]
    expect_path = None
    if "--expect" in args:
        i = args.index("--expect")
        expect_path = args[i + 1]
        args = args[:i] + args[i + 2:]
    if len(args) != 1:
        print("usage: cross_check.py [checksum|stats] FILE [--expect FILE]", file=sys.stderr)
        return 2
    path = args[0]

    with open(path, "rb") as f:
        data = f.read()
    try:
        a = walk(data)
    except SpecError as e:
        print(f"cross_check: {e}", file=sys.stderr)
        return 1

    if mode == "stats":
        print_stats(a, len(data))
        return 0

    if expect_path is None:
        print(a.line())
        print(f"consumed        {a.consumed} bytes")
        return 0
    try:
        with open(expect_path) as f:
            expected = parse_expected(f.read())
    except SpecError as e:
        print(f"cross_check: {e}", file=sys.stderr)
        return 2
    return compare(a, expected)


if __name__ == "__main__":
    sys.exit(main(sys.argv))
