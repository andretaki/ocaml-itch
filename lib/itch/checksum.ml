open! Core

(** A reference handler that touches every field it decodes.

    Two jobs. It is a worked example of {!Handler.S}, and it is what makes a
    benchmark mean something: if the decoded fields are never consumed, a
    compiler is entitled to notice and delete the decoding, and the benchmark
    then measures an empty loop. Folding every field into an accumulator makes
    that impossible.

    It also makes cross-implementation comparison self-validating. A C++ or
    Python implementation computing this same aggregate over the same file must
    print the same numbers; if it does, the two programs demonstrably did the
    same work, and the timings can be compared. Identical output is the evidence.

    The accumulators are chosen so the arithmetic is exact and identical in any
    language with 64-bit integers: xor never overflows, and the sums stay far
    below 2^62 even over a full trading day. *)

type t =
  { mutable messages : int
  ; mutable adds : int
  ; mutable executes : int
  ; mutable cancels : int
  ; mutable deletes : int
  ; mutable replaces : int
  ; mutable directories : int
  ; mutable system_events : int
  ; mutable others : int
  ; mutable sum_shares : int
  ; mutable sum_prices : int
  ; mutable xor_order_refs : int
  ; mutable xor_timestamps : int
  ; mutable max_locate : int
  }

let create () =
  { messages = 0
  ; adds = 0
  ; executes = 0
  ; cancels = 0
  ; deletes = 0
  ; replaces = 0
  ; directories = 0
  ; system_events = 0
  ; others = 0
  ; sum_shares = 0
  ; sum_prices = 0
  ; xor_order_refs = 0
  ; xor_timestamps = 0
  ; max_locate = 0
  }
;;

let to_string t =
  sprintf
    "messages=%d adds=%d executes=%d cancels=%d deletes=%d replaces=%d directories=%d \
     system_events=%d others=%d sum_shares=%d sum_prices=%d xor_order_refs=%d \
     xor_timestamps=%d max_locate=%d"
    t.messages
    t.adds
    t.executes
    t.cancels
    t.deletes
    t.replaces
    t.directories
    t.system_events
    t.others
    t.sum_shares
    t.sum_prices
    t.xor_order_refs
    t.xor_timestamps
    t.max_locate
;;

(* Every callback below updates mutable fields of an already allocated record,
   which is a store, not an allocation. *)

let note t ~stock_locate ~timestamp =
  t.messages <- t.messages + 1;
  t.xor_timestamps <- t.xor_timestamps lxor timestamp;
  if stock_locate > t.max_locate then t.max_locate <- stock_locate
;;

let on_system_event t ~stock_locate ~tracking_number ~timestamp ~event_code =
  note t ~stock_locate ~timestamp;
  t.system_events <- t.system_events + 1;
  t.sum_shares <- t.sum_shares + tracking_number + Char.to_int event_code
;;

let add_stock_directory t ~stock_locate ~timestamp ~round_lot_size =
  note t ~stock_locate ~timestamp;
  t.directories <- t.directories + 1;
  t.sum_shares <- t.sum_shares + round_lot_size
;;

let on_stock_directory t buf ~pos ~stock_locate ~timestamp =
  add_stock_directory
    t
    ~stock_locate
    ~timestamp
    ~round_lot_size:(Wire.Stock_directory.round_lot_size buf ~pos)
;;

let add_add_order t ~stock_locate ~timestamp ~order_ref ~side ~shares ~price ~attributed =
  note t ~stock_locate ~timestamp;
  t.adds <- t.adds + 1;
  t.sum_shares <- t.sum_shares + shares + Char.to_int side + Bool.to_int attributed;
  t.sum_prices <- t.sum_prices + price;
  t.xor_order_refs <- t.xor_order_refs lxor order_ref
;;

let on_add_order
      t
      _buf
      ~pos:_
      ~stock_locate
      ~timestamp
      ~order_ref
      ~side
      ~shares
      ~price
      ~attributed
  =
  add_add_order t ~stock_locate ~timestamp ~order_ref ~side ~shares ~price ~attributed
;;

let on_order_executed t ~stock_locate ~timestamp ~order_ref ~executed_shares ~match_number
  =
  note t ~stock_locate ~timestamp;
  t.executes <- t.executes + 1;
  t.sum_shares <- t.sum_shares + executed_shares;
  t.xor_order_refs <- t.xor_order_refs lxor order_ref lxor match_number
;;

let on_order_executed_with_price
      t
      ~stock_locate
      ~timestamp
      ~order_ref
      ~executed_shares
      ~match_number
      ~printable
      ~execution_price
  =
  note t ~stock_locate ~timestamp;
  t.executes <- t.executes + 1;
  t.sum_shares <- t.sum_shares + executed_shares + Bool.to_int printable;
  t.sum_prices <- t.sum_prices + execution_price;
  t.xor_order_refs <- t.xor_order_refs lxor order_ref lxor match_number
;;

let on_order_cancel t ~stock_locate ~timestamp ~order_ref ~cancelled_shares =
  note t ~stock_locate ~timestamp;
  t.cancels <- t.cancels + 1;
  t.sum_shares <- t.sum_shares + cancelled_shares;
  t.xor_order_refs <- t.xor_order_refs lxor order_ref
;;

let on_order_delete t ~stock_locate ~timestamp ~order_ref =
  note t ~stock_locate ~timestamp;
  t.deletes <- t.deletes + 1;
  t.xor_order_refs <- t.xor_order_refs lxor order_ref
;;

let on_order_replace
      t
      ~stock_locate
      ~timestamp
      ~original_order_ref
      ~new_order_ref
      ~shares
      ~price
  =
  note t ~stock_locate ~timestamp;
  t.replaces <- t.replaces + 1;
  t.sum_shares <- t.sum_shares + shares;
  t.sum_prices <- t.sum_prices + price;
  t.xor_order_refs <- t.xor_order_refs lxor original_order_ref lxor new_order_ref
;;

let add_other t ~message_type ~length =
  t.messages <- t.messages + 1;
  t.others <- t.others + 1;
  t.sum_shares <- t.sum_shares + Char.to_int message_type + length
;;

let on_other t _buf ~pos:_ ~message_type ~length = add_other t ~message_type ~length

(** Drive the same arithmetic from an already decoded {!Message.t}.

    This is what makes the two decode paths comparable: run both over the same
    bytes, and the resulting accumulators must be identical. Note what that does
    and does not prove. Both paths read their bytes through {!Wire}, so this is
    not a re-check of the offsets -- those are covered by the encoder round trip,
    which keeps its own independent offsets, and by the cross-check against an
    independent implementation. What it checks is the dispatch and the plumbing,
    which is where new code goes wrong. *)
let of_message t (message : Message.t) =
  let open Types in
  match message with
  | System_event m ->
    on_system_event
      t
      ~stock_locate:(Locate.to_int m.stock_locate)
      ~tracking_number:m.tracking_number
      ~timestamp:(Timestamp.to_ns_since_midnight m.timestamp)
      ~event_code:(Message.Event_code.to_char m.event_code)
  | Stock_directory m ->
    add_stock_directory
      t
      ~stock_locate:(Locate.to_int m.stock_locate)
      ~timestamp:(Timestamp.to_ns_since_midnight m.timestamp)
      ~round_lot_size:m.round_lot_size
  | Add_order m ->
    add_add_order
      t
      ~stock_locate:(Locate.to_int m.stock_locate)
      ~timestamp:(Timestamp.to_ns_since_midnight m.timestamp)
      ~order_ref:(Order_ref.to_int m.order_ref)
      ~side:(Side.to_char m.side)
      ~shares:(Shares.to_int m.shares)
      ~price:(Price.to_raw_4 m.price)
      ~attributed:(Option.is_some m.attribution)
  | Order_executed m ->
    on_order_executed
      t
      ~stock_locate:(Locate.to_int m.stock_locate)
      ~timestamp:(Timestamp.to_ns_since_midnight m.timestamp)
      ~order_ref:(Order_ref.to_int m.order_ref)
      ~executed_shares:(Shares.to_int m.executed_shares)
      ~match_number:(Match_number.to_int m.match_number)
  | Order_executed_with_price m ->
    on_order_executed_with_price
      t
      ~stock_locate:(Locate.to_int m.stock_locate)
      ~timestamp:(Timestamp.to_ns_since_midnight m.timestamp)
      ~order_ref:(Order_ref.to_int m.order_ref)
      ~executed_shares:(Shares.to_int m.executed_shares)
      ~match_number:(Match_number.to_int m.match_number)
      ~printable:m.printable
      ~execution_price:(Price.to_raw_4 m.execution_price)
  | Order_cancel m ->
    on_order_cancel
      t
      ~stock_locate:(Locate.to_int m.stock_locate)
      ~timestamp:(Timestamp.to_ns_since_midnight m.timestamp)
      ~order_ref:(Order_ref.to_int m.order_ref)
      ~cancelled_shares:(Shares.to_int m.cancelled_shares)
  | Order_delete m ->
    on_order_delete
      t
      ~stock_locate:(Locate.to_int m.stock_locate)
      ~timestamp:(Timestamp.to_ns_since_midnight m.timestamp)
      ~order_ref:(Order_ref.to_int m.order_ref)
  | Order_replace m ->
    on_order_replace
      t
      ~stock_locate:(Locate.to_int m.stock_locate)
      ~timestamp:(Timestamp.to_ns_since_midnight m.timestamp)
      ~original_order_ref:(Order_ref.to_int m.original_order_ref)
      ~new_order_ref:(Order_ref.to_int m.new_order_ref)
      ~shares:(Shares.to_int m.shares)
      ~price:(Price.to_raw_4 m.price)
  | Unparsed m -> add_other t ~message_type:m.message_type ~length:m.length
;;
