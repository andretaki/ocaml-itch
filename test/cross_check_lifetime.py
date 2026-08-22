#!/usr/bin/env python3
"""An independent implementation of the Level 4 order-lifetime analysis.

Same role as ``cross_check.py`` and ``cross_check_book.py``: a third decoder,
written from the ITCH 5.0 specification tables rather than from
``lib/itch/wire.ml``, so that agreeing with the OCaml is evidence and not a
tautology. Every byte offset below was transcribed by hand from the spec.

What it compares is deliberately *not* the percentile table. Percentiles come
out of a bucketed histogram, so two correct implementations can legitimately
disagree on one, and a comparison that tolerates disagreement is not a
comparison. Everything checked here has exactly one right answer:

    messages, decoded, timestamped   framing and header agreement
    executed/cancelled/deleted/       which of the four ways an order can
      replaced                        leave the book was taken, and how often
    lifetime_sum_hi/lo, min, max      the distribution's first moment and both
                                      ends, exact, unbucketed. The sum is
                                      carried in two limbs of base 2**30
                                      because a full day's lifetimes total
                                      about 6.2e18 nanoseconds and an OCaml int
                                      holds 4.6e18 -- Python's integers do not
                                      have that limit, which is exactly why the
                                      comparison has to be made in the
                                      representation the OCaml can express
    orphans, resting                  the book-keeping either side of the run
    peak_ms, peak_ms_count            the busiest millisecond of the session

The subtle ones are the terminal causes. A partial execution or partial cancel
is *not* a death -- the order rests on with fewer shares -- so getting the
share arithmetic backwards moves counts between buckets rather than producing
an error, and ``lifetime_sum`` moves with it. That is precisely the class of
mistake this file exists to catch, and it is the same class the order book
already got wrong once (a cancel carries shares to subtract, a replace carries
a new total).

    ./_build/default/bin/itch.exe analyze FILE -aggregate | head -1 > /tmp/ml.txt
    python3 test/cross_check_lifetime.py FILE --expect /tmp/ml.txt
"""
import sys
from array import array


class SpecError(Exception):
    """A message whose framed length is not the width the spec gives for it."""


def u(b: bytes) -> int:
    return int.from_bytes(b, "big")


# Widths from the ITCH 5.0 specification tables. A message whose framing length
# disagrees is refused rather than decoded, matching the OCaml: reading a field
# out of a message too short to hold it is the failure this comparison exists
# to catch.
SPEC_LENGTH = {
    "S": 12,
    "R": 39,
    "A": 36,
    "F": 40,
    "E": 31,
    "C": 36,
    "X": 23,
    "D": 19,
    "U": 35,
}

DECODED = set(SPEC_LENGTH)

HEADER_LENGTH = 11
MILLISECONDS_PER_DAY = 86_400_000

# Field offsets, counted from the first byte of the message (the type byte).
# Transcribed from the spec tables, section by section.
TIMESTAMP_AT = 5          # 6 bytes, nanoseconds since midnight

ADD_ORDER_REF = 11        # 4.3.1  Add Order, 8 bytes
ADD_SHARES = 20           #        4 bytes
EXEC_ORDER_REF = 11       # 4.4.1  Order Executed, 8 bytes
EXEC_SHARES = 19          #        4 bytes
EXEC_PRICE_ORDER_REF = 11 # 4.4.2  Order Executed With Price, 8 bytes
EXEC_PRICE_SHARES = 19    #        4 bytes
CANCEL_ORDER_REF = 11     # 4.4.3  Order Cancel, 8 bytes
CANCEL_SHARES = 19        #        4 bytes
DELETE_ORDER_REF = 11     # 4.4.4  Order Delete, 8 bytes
REPLACE_ORIGINAL_REF = 11 # 4.4.5  Order Replace, 8 bytes
REPLACE_NEW_REF = 19      #        8 bytes
REPLACE_SHARES = 27       #        4 bytes


class Analysis:
    def __init__(self):
        self.live = {}            # order reference -> [timestamp, shares]
        self.messages = 0
        self.decoded = 0
        self.timestamped = 0
        self.orphans = 0
        self.terminated = {"executed": 0, "cancelled": 0, "deleted": 0, "replaced": 0}
        self.lifetime_sum = 0
        self.lifetime_min = None
        self.lifetime_max = None
        # A flat array rather than a dict: a full session has 27 million
        # non-empty milliseconds, and a dict of those costs several gigabytes
        # where this costs 691 MB and indexes in constant time.
        self.per_ms = array("q", bytes(MILLISECONDS_PER_DAY * 8))
        self.first_timestamp = -1
        self.last_timestamp = -1

    def note_timestamp(self, timestamp):
        self.messages += 1
        if timestamp < 0:
            return
        if self.first_timestamp < 0:
            self.first_timestamp = timestamp
        self.last_timestamp = timestamp
        ms = timestamp // 1_000_000
        if ms < MILLISECONDS_PER_DAY:
            self.timestamped += 1
            self.per_ms[ms] += 1

    def note_death(self, cause, born, died):
        lifetime = died - born if died >= born else 0
        self.terminated[cause] += 1
        self.lifetime_sum += lifetime
        if self.lifetime_min is None or lifetime < self.lifetime_min:
            self.lifetime_min = lifetime
        if self.lifetime_max is None or lifetime > self.lifetime_max:
            self.lifetime_max = lifetime

    def add(self, ref, timestamp, shares):
        if ref in self.live:
            raise SpecError(f"add for an order reference already resting: {ref}")
        self.live[ref] = [timestamp, shares]

    def reduce(self, ref, shares, timestamp, cause):
        entry = self.live.get(ref)
        if entry is None:
            self.orphans += 1
            return
        remaining = entry[1] - shares
        if remaining > 0:
            entry[1] = remaining
        else:
            self.note_death(cause, entry[0], timestamp)
            del self.live[ref]

    def remove(self, ref, timestamp, cause):
        entry = self.live.get(ref)
        if entry is None:
            self.orphans += 1
            return None
        self.note_death(cause, entry[0], timestamp)
        del self.live[ref]
        return entry

    def peak_millisecond(self):
        """The busiest millisecond, earliest one winning a tie.

        Scanning the session span in ascending order rather than taking a max
        over a dict: ties have to break the same way on both sides or the
        comparison fails for a reason that is not a bug.
        """
        if self.first_timestamp < 0:
            return -1, -1
        first = self.first_timestamp // 1_000_000
        last = min(MILLISECONDS_PER_DAY - 1, self.last_timestamp // 1_000_000)
        best_ms, best_count = -1, -1
        for ms in range(first, last + 1):
            count = self.per_ms[ms]
            if count > best_count:
                best_count = count
                best_ms = ms
        return best_ms, best_count

    def to_line(self):
        return (
            "messages={} decoded={} timestamped={} orphans={} resting={} "
            "executed={} cancelled={} deleted={} replaced={} "
            "lifetime_sum_hi={} lifetime_sum_lo={} "
            "lifetime_min={} lifetime_max={} peak_ms={} peak_ms_count={}"
        ).format(
            self.messages,
            self.decoded,
            self.timestamped,
            self.orphans,
            len(self.live),
            self.terminated["executed"],
            self.terminated["cancelled"],
            self.terminated["deleted"],
            self.terminated["replaced"],
            self.lifetime_sum >> 30,
            self.lifetime_sum & 0x3FFFFFFF,
            self.lifetime_min if self.lifetime_min is not None else 0,
            self.lifetime_max if self.lifetime_max is not None else 0,
            *self.peak_millisecond(),
        )


def run(path, chunk_size=1 << 24):
    """Stream the file rather than reading it whole.

    A full trading session is 12.95 GB, which does not want to be a single
    bytes object on a machine that also has to hold the live-order table. Reads
    are chunked, and a message straddling a chunk boundary is carried into the
    next read rather than truncated -- getting that wrong would silently drop
    one message per chunk, which at this chunk size is 772 of them, and would
    look exactly like a decoding disagreement.
    """
    a = Analysis()
    with open(path, "rb") as handle:
        carry = b""
        done = False
        while not done:
            chunk = handle.read(chunk_size)
            if not chunk:
                break
            data = carry + chunk if carry else chunk
            pos = 0
            limit = len(data)
            while pos + 2 <= limit:
                length = u(data[pos : pos + 2])
                if length == 0:
                    # Some files are padded with zeroes after the final message.
                    done = True
                    break
                if pos + 2 + length > limit:
                    break
                consume(a, data[pos + 2 : pos + 2 + length], length)
                pos += 2 + length
            carry = data[pos:]
    return a


def consume(a, m, length):
    kind = chr(m[0])

    expected = SPEC_LENGTH.get(kind)
    if expected is not None and expected != length:
        raise SpecError(
            f"ITCH message length disagrees with the spec: {kind} "
            f"spec {expected}, framed {length}"
        )

    if kind not in DECODED:
        # Undecoded types still carry the standard header, and still count
        # towards the message rate: a feed handler has to keep up with the
        # whole stream, not just the part it understands.
        if length >= HEADER_LENGTH:
            a.note_timestamp(u(m[TIMESTAMP_AT : TIMESTAMP_AT + 6]))
        else:
            a.messages += 1
        return

    timestamp = u(m[TIMESTAMP_AT : TIMESTAMP_AT + 6])
    a.note_timestamp(timestamp)
    a.decoded += 1

    if kind in ("A", "F"):
        a.add(
            u(m[ADD_ORDER_REF : ADD_ORDER_REF + 8]),
            timestamp,
            u(m[ADD_SHARES : ADD_SHARES + 4]),
        )
    elif kind == "E":
        a.reduce(
            u(m[EXEC_ORDER_REF : EXEC_ORDER_REF + 8]),
            u(m[EXEC_SHARES : EXEC_SHARES + 4]),
            timestamp,
            "executed",
        )
    elif kind == "C":
        a.reduce(
            u(m[EXEC_PRICE_ORDER_REF : EXEC_PRICE_ORDER_REF + 8]),
            u(m[EXEC_PRICE_SHARES : EXEC_PRICE_SHARES + 4]),
            timestamp,
            "executed",
        )
    elif kind == "X":
        a.reduce(
            u(m[CANCEL_ORDER_REF : CANCEL_ORDER_REF + 8]),
            u(m[CANCEL_SHARES : CANCEL_SHARES + 4]),
            timestamp,
            "cancelled",
        )
    elif kind == "D":
        a.remove(u(m[DELETE_ORDER_REF : DELETE_ORDER_REF + 8]), timestamp, "deleted")
    elif kind == "U":
        original = u(m[REPLACE_ORIGINAL_REF : REPLACE_ORIGINAL_REF + 8])
        if a.remove(original, timestamp, "replaced") is not None:
            # Spec 4.4.5: the replacement carries a new total, not a delta, and
            # its life starts here.
            a.add(
                u(m[REPLACE_NEW_REF : REPLACE_NEW_REF + 8]),
                timestamp,
                u(m[REPLACE_SHARES : REPLACE_SHARES + 4]),
            )


def compare(actual, expected_path):
    """Report *which* field diverged, not merely that the lines differ.

    A bare diff of two fourteen-field lines says they disagree. This says it was
    ``cancelled`` and by how much, which names the message type and the field to
    go and look at.
    """
    with open(expected_path) as handle:
        expected_line = handle.readline().strip()

    def parse(line):
        out = {}
        for token in line.split():
            if "=" in token:
                key, value = token.split("=", 1)
                out[key] = value
        return out

    mine = parse(actual)
    theirs = parse(expected_line)
    keys = sorted(set(mine) | set(theirs))
    diverged = [k for k in keys if mine.get(k) != theirs.get(k)]
    if not diverged:
        print("agree on all {} fields".format(len(keys)))
        return 0
    print("DISAGREE on {} of {} fields".format(len(diverged), len(keys)), file=sys.stderr)
    for k in diverged:
        print(
            "  {:<16} python {:<24} ocaml {}".format(
                k, mine.get(k, "(absent)"), theirs.get(k, "(absent)")
            ),
            file=sys.stderr,
        )
    return 1


def main(argv):
    if len(argv) < 2:
        print(__doc__, file=sys.stderr)
        return 2
    path = argv[1]
    expected = None
    if "--expect" in argv:
        expected = argv[argv.index("--expect") + 1]
    a = run(path)
    line = a.to_line()
    if expected is None:
        print(line)
        return 0
    return compare(line, expected)


if __name__ == "__main__":
    sys.exit(main(sys.argv))
