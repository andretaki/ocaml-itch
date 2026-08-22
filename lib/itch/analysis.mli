open! Core

(** Level 4: what a full trading day looks like, measured rather than assumed.

    Two questions, both answered in a single pass through the zero-allocation
    handler path, because a second pass over 12.95 GB costs fourteen minutes.

    {b Order lifetime.} How long does a resting order survive between arriving
    and leaving? The book in {!Order_book} cannot answer this: it tracks shares
    at a price level and deliberately forgets arrival times. This keeps a
    {!Order_table} of live orders keyed by reference, and records
    [death - birth] into a {!Histogram} when the reference leaves the book.

    {b Message rate.} How much worse is the worst millisecond than the average
    one? Counted per millisecond of the session across {i every} message type,
    including the ones this parser does not decode, because a feed handler has
    to keep up with the whole stream and not just the part it understands.

    A reference leaves the book four ways, and they are kept apart rather than
    pooled, because pooling them would hide the interesting part -- an order
    that is cancelled and one that trades have very different lifetimes, and the
    ratio is the finding. A partial execution or partial cancel is {i not} a
    death: the order rests on with fewer shares, so the table tracks remaining
    shares to know the difference.

    A replaced order is counted as a death of the old reference and a birth of
    the new one, which is the literal reading of the wire and is stated here
    because the other reading -- one logical order that changes price -- gives
    a materially different answer. {!Cause.Replaced} is reported separately so
    both readings can be recovered from the same run. *)

module Cause : sig
  type t =
    | Executed (** shares went to zero through "E" or "C" *)
    | Cancelled (** shares went to zero through "X" *)
    | Deleted (** "D", the whole order withdrawn at once *)
    | Replaced (** "U", this reference retired in favour of a new one *)
  [@@deriving enumerate, sexp_of]

  val to_string : t -> string
end

type t

(** [capacity_log2] sizes the live-order table, which cannot grow mid-run; the
    default holds about 25 million resting orders, against a measured 1.9
    million at the busiest point of a full session. Check {!max_live_orders}
    after a run to see how much of it was needed.

    [milliseconds] sizes the per-millisecond counter array and defaults to a
    whole day, which costs 691 MB. It exists so that tests can ask for a small
    one instead of paying to zero that array on every construction; nothing on
    the command line shrinks it. A timestamp past the end is counted in
    {!messages} but not attributed to a millisecond, so a value set too small
    shows up as {!timestamped_messages} falling short of {!messages} rather than
    as a silently wrong rate. *)
val create
  :  ?capacity_log2:int
  -> ?significant_bits:int
  -> ?milliseconds:int
  -> unit
  -> t

include Handler.S with type t := t

val messages : t -> int
val decoded_messages : t -> int

(** Lifetimes of orders that left the book by this cause. *)
val lifetimes : t -> Cause.t -> Histogram.t

(** Every terminated order, whatever the cause. *)
val all_lifetimes : t -> Histogram.t

val terminated : t -> Cause.t -> int

(** Modifies naming a reference the table never saw. Expected to be zero when
    reading a session from its first byte, and a diagnostic when it is not. *)
val orphan_modifies : t -> int

(** Orders still resting when the file ran out. *)
val live_orders : t -> int

val max_live_orders : t -> int
val table_capacity : t -> int
val first_timestamp : t -> int
val last_timestamp : t -> int

(** Messages stamped with each millisecond of the session, over the populated
    span only. Every message type is counted, decoded or not. *)
val iter_milliseconds : t -> f:(ms:int -> count:int -> unit) -> unit

(** Total messages that carried a usable timestamp, which is the denominator the
    per-millisecond rates should be read against. *)
val timestamped_messages : t -> int
