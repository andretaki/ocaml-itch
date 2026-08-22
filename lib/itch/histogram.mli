open! Core

(** A log-linear histogram of non-negative ints, in the shape HdrHistogram uses.

    Order lifetimes span nine orders of magnitude -- the interesting ones are
    microseconds and the boring ones are hours -- so a linear histogram is
    hopeless and a plain power-of-two one is too coarse to say anything about
    the median. This keeps [significant_bits] bits of relative precision at
    every scale: the bucket holding a value is never wider than
    [value / 2 ** significant_bits], so a reported percentile is exact to that
    fraction no matter how large it is.

    {!record} is allocation-free, because it runs once per terminated order
    inside a [@@zero_alloc] handler callback. Bucket count is fixed at creation
    and covers the whole positive int range, so nothing is ever resized. *)

type t

(** [significant_bits] must be in [1, 16]. Five gives 32 buckets per octave, so
    about 3% worst-case relative error, in roughly 1,400 ints of storage. *)
val create : significant_bits:int -> t

val record : t -> int -> unit
[@@zero_alloc]
val count : t -> int

(** The exact sum of every recorded value, in two limbs of base 2 ** 30:
    [total = total_high * 2 ** 30 + total_low], with [total_low] in
    [0, 2 ** 30).

    Two limbs rather than one because a full trading day overflows one. The
    lifetimes of a day's 223 million terminated orders sum to about 6.2e18
    nanoseconds and an OCaml int holds 4.6e18, so the first full-day run
    reported a mean lifetime of -13.6 seconds and raised nothing. It is kept
    exact, rather than accumulated into a float, because an independent
    implementation can only check a quantity that has exactly one right
    answer. *)
val total_high : t -> int

val total_low : t -> int

(** The arithmetic mean, which does not inherit the bucketing error because it
    is computed from the exact total rather than from the buckets. *)
val mean : t -> float

(** Exact, not bucketed: tracked separately as values arrive. *)
val min_value : t -> int

val max_value : t -> int

(** The index of the bucket containing the [p]th percentile, or [-1] if nothing
    has been recorded. [p] is in [0, 100]. *)
val bucket_at_percentile : t -> float -> int

(** The inclusive bounds of a bucket. A percentile is honestly reported as this
    range rather than a single number: the histogram knows the value fell in
    here and does not know where. *)
val bucket_low : t -> int -> int

val bucket_high : t -> int -> int

(** How many recorded values were less than or equal to [value]. Exact when
    [value] is a bucket boundary, and rounded to the enclosing bucket otherwise,
    which is stated rather than hidden: use it for thresholds like "under a
    millisecond" by passing a value one below a boundary. *)
val count_at_or_below : t -> int -> int

val iter_buckets : t -> f:(low:int -> high:int -> count:int -> unit) -> unit
