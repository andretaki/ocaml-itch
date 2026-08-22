open! Core

module Cause = struct
  type t =
    | Executed
    | Cancelled
    | Deleted
    | Replaced
  [@@deriving enumerate, sexp_of]

  let to_string = function
    | Executed -> "executed"
    | Cancelled -> "cancelled"
    | Deleted -> "deleted"
    | Replaced -> "replaced"
  ;;

end

(* A trading session runs from pre-market to after-hours, but ITCH timestamps
   are nanoseconds since midnight, so the array is sized for a whole day rather
   than for the session. 86.4 million ints is 691 MB, which is the price of
   keeping every millisecond individually instead of pre-aggregating -- and
   keeping them is what makes it possible to say *when* the worst burst was,
   not merely how big. *)
let milliseconds_per_day = 86_400_000
let default_capacity_log2 = 25
let default_significant_bits = 5

(* One named field per cause rather than a [Histogram.t array] indexed by the
   cause, and this is not a matter of taste -- it is the second thing
   [@@zero_alloc] caught that vanilla OCaml compiled without a word.

   [Histogram.t] is abstract outside its own module, so at an array access the
   compiler cannot rule out that the array is a flat float array, and must emit
   the boxing read. OxCaml reported it exactly: "allocation of 16 bytes for
   float", once per terminated order, which is 223 million times over a trading
   day. A record field has a statically known type and reads flat.

   The existing runtime allocation test would not have found this either: it
   drives the [Checksum] handler, not this one. So there is now one for this
   handler too. *)
type t =
  { live : Order_table.t
  ; executed : Histogram.t
  ; cancelled : Histogram.t
  ; deleted : Histogram.t
  ; replaced : Histogram.t
  ; all_lifetimes : Histogram.t
  ; mutable n_executed : int
  ; mutable n_cancelled : int
  ; mutable n_deleted : int
  ; mutable n_replaced : int
  ; per_ms : int array
  ; mutable messages : int
  ; mutable decoded_messages : int
  ; mutable timestamped_messages : int
  ; mutable orphan_modifies : int
  ; mutable first_timestamp : int
  ; mutable last_timestamp : int
  }

let create
      ?(capacity_log2 = default_capacity_log2)
      ?(significant_bits = default_significant_bits)
      ?(milliseconds = milliseconds_per_day)
      ()
  =
  { live = Order_table.create ~capacity_log2
  ; executed = Histogram.create ~significant_bits
  ; cancelled = Histogram.create ~significant_bits
  ; deleted = Histogram.create ~significant_bits
  ; replaced = Histogram.create ~significant_bits
  ; all_lifetimes = Histogram.create ~significant_bits
  ; n_executed = 0
  ; n_cancelled = 0
  ; n_deleted = 0
  ; n_replaced = 0
  ; per_ms = Array.create ~len:milliseconds 0
  ; messages = 0
  ; decoded_messages = 0
  ; timestamped_messages = 0
  ; orphan_modifies = 0
  ; first_timestamp = -1
  ; last_timestamp = -1
  }
;;

(* Called once per message, so it is written to allocate nothing: integer
   division by a constant, a bounds test, and one array bump. A timestamp
   outside the day is counted as a message but not attributed to a millisecond,
   rather than being clamped into the first or last bucket where it would
   manufacture a burst that did not happen. *)
let[@zero_alloc] note_timestamp t timestamp =
  t.messages <- t.messages + 1;
  if timestamp >= 0
  then (
    if t.first_timestamp < 0 then t.first_timestamp <- timestamp;
    t.last_timestamp <- timestamp;
    let ms = timestamp / 1_000_000 in
    if ms < Array.length t.per_ms
    then (
      t.timestamped_messages <- t.timestamped_messages + 1;
      Array.unsafe_set t.per_ms ms (Array.unsafe_get t.per_ms ms + 1)))
;;

(* [cause] is a constructor without arguments, so it is an immediate and passing
   it costs nothing. *)
let[@zero_alloc] note_death t (cause : Cause.t) ~born ~died =
  (* Timestamps within a session are non-decreasing, but a defensive floor at
     zero costs nothing and keeps a negative from reaching Histogram.record,
     which would raise in the middle of a hundred-second run. *)
  let lifetime = if died >= born then died - born else 0 in
  (match cause with
   | Executed ->
     Histogram.record t.executed lifetime;
     t.n_executed <- t.n_executed + 1
   | Cancelled ->
     Histogram.record t.cancelled lifetime;
     t.n_cancelled <- t.n_cancelled + 1
   | Deleted ->
     Histogram.record t.deleted lifetime;
     t.n_deleted <- t.n_deleted + 1
   | Replaced ->
     Histogram.record t.replaced lifetime;
     t.n_replaced <- t.n_replaced + 1);
  Histogram.record t.all_lifetimes lifetime
;;

(* Shared by "E", "C" and "X": all three carry a share count to subtract, and
   differ only in what the spec calls it and what it means when the count
   empties the order. Getting the arithmetic backwards here -- treating the
   field as a new total rather than a delta -- is the same mistake Order_book
   documents, and it would show up as an implausibly short lifetime rather than
   as an error. *)
let[@zero_alloc] reduce t ~order_ref ~shares ~timestamp ~(cause : Cause.t) =
  let index = Order_table.slot t.live ~key:order_ref in
  if index < 0
  then t.orphan_modifies <- t.orphan_modifies + 1
  else (
    let remaining = Order_table.shares_at t.live index - shares in
    if remaining > 0
    then Order_table.set_shares_at t.live index remaining
    else (
      note_death t cause ~born:(Order_table.timestamp_at t.live index) ~died:timestamp;
      Order_table.delete_at t.live index))
;;

let[@zero_alloc] on_system_event t ~stock_locate:_ ~tracking_number:_ ~timestamp ~event_code:_ =
  note_timestamp t timestamp;
  t.decoded_messages <- t.decoded_messages + 1
;;

let[@zero_alloc] on_stock_directory t _buf ~pos:_ ~stock_locate:_ ~timestamp =
  note_timestamp t timestamp;
  t.decoded_messages <- t.decoded_messages + 1
;;

let[@zero_alloc] on_add_order
      t
      _buf
      ~pos:_
      ~stock_locate:_
      ~timestamp
      ~order_ref
      ~side:_
      ~shares
      ~price:_
      ~attributed:_
  =
  note_timestamp t timestamp;
  t.decoded_messages <- t.decoded_messages + 1;
  Order_table.insert t.live ~key:order_ref ~timestamp ~shares
;;

let[@zero_alloc] on_order_executed t ~stock_locate:_ ~timestamp ~order_ref ~executed_shares ~match_number:_ =
  note_timestamp t timestamp;
  t.decoded_messages <- t.decoded_messages + 1;
  reduce t ~order_ref ~shares:executed_shares ~timestamp ~cause:Executed
;;

let[@zero_alloc] on_order_executed_with_price
      t
      ~stock_locate:_
      ~timestamp
      ~order_ref
      ~executed_shares
      ~match_number:_
      ~printable:_
      ~execution_price:_
  =
  note_timestamp t timestamp;
  t.decoded_messages <- t.decoded_messages + 1;
  reduce t ~order_ref ~shares:executed_shares ~timestamp ~cause:Executed
;;

let[@zero_alloc] on_order_cancel t ~stock_locate:_ ~timestamp ~order_ref ~cancelled_shares =
  note_timestamp t timestamp;
  t.decoded_messages <- t.decoded_messages + 1;
  reduce t ~order_ref ~shares:cancelled_shares ~timestamp ~cause:Cancelled
;;

let[@zero_alloc] on_order_delete t ~stock_locate:_ ~timestamp ~order_ref =
  note_timestamp t timestamp;
  t.decoded_messages <- t.decoded_messages + 1;
  let index = Order_table.slot t.live ~key:order_ref in
  if index < 0
  then t.orphan_modifies <- t.orphan_modifies + 1
  else (
    note_death t Deleted ~born:(Order_table.timestamp_at t.live index) ~died:timestamp;
    Order_table.delete_at t.live index)
;;

let[@zero_alloc] on_order_replace
      t
      ~stock_locate:_
      ~timestamp
      ~original_order_ref
      ~new_order_ref
      ~shares
      ~price:_
  =
  note_timestamp t timestamp;
  t.decoded_messages <- t.decoded_messages + 1;
  let index = Order_table.slot t.live ~key:original_order_ref in
  if index < 0
  then t.orphan_modifies <- t.orphan_modifies + 1
  else (
    note_death t Replaced ~born:(Order_table.timestamp_at t.live index) ~died:timestamp;
    Order_table.delete_at t.live index;
    (* Spec 1.4.5: the replacement carries a new total, not a delta, and it
       starts its own life here. *)
    Order_table.insert t.live ~key:new_order_ref ~timestamp ~shares)
;;

(* Every ITCH 5.0 message carries the same 11-byte header, so a type this parser
   does not decode still contributes to the message rate -- and it must, because
   a feed handler has to keep up with the whole stream. The length guard is not
   decoration: [Reader] has established that [length] bytes are present, and
   nothing has established that [length] reaches the timestamp field. *)
let[@zero_alloc] on_other t buf ~pos ~message_type:_ ~length =
  if length >= Wire.header_length
  then note_timestamp t (Wire.timestamp buf ~pos)
  else t.messages <- t.messages + 1
;;

let messages t = t.messages
let decoded_messages t = t.decoded_messages
let timestamped_messages t = t.timestamped_messages
let lifetimes t : Cause.t -> Histogram.t = function
  | Executed -> t.executed
  | Cancelled -> t.cancelled
  | Deleted -> t.deleted
  | Replaced -> t.replaced
;;

let all_lifetimes t = t.all_lifetimes

let terminated t : Cause.t -> int = function
  | Executed -> t.n_executed
  | Cancelled -> t.n_cancelled
  | Deleted -> t.n_deleted
  | Replaced -> t.n_replaced
;;
let orphan_modifies t = t.orphan_modifies
let live_orders t = Order_table.length t.live
let max_live_orders t = Order_table.max_occupancy t.live
let table_capacity t = Order_table.capacity t.live
let first_timestamp t = t.first_timestamp
let last_timestamp t = t.last_timestamp

let iter_milliseconds t ~f =
  if t.first_timestamp >= 0
  then (
    let first = t.first_timestamp / 1_000_000 in
    let last = Int.min (Array.length t.per_ms - 1) (t.last_timestamp / 1_000_000) in
    for ms = first to last do
      f ~ms ~count:t.per_ms.(ms)
    done)
;;
