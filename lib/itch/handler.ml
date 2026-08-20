open! Core

(** The zero-allocation consumer interface.

    Every callback takes only immediates -- [int], [char], [bool] -- plus the
    buffer and the message offset. OCaml represents all of those unboxed, and
    labels are resolved at compile time, so dispatching a message through a
    handler allocates nothing at all. Contrast {!Parser.parse_exn}, which must
    build a {!Message.t} record to hand you the same information.

    Alpha fields are deliberately {i not} decoded. Reading one means building a
    string, and a string is an allocation. What the callback gets instead is the
    buffer and the message position, so a handler that genuinely needs the symbol
    can pay for it with {!Wire.padded_string} and one that does not pays nothing.

    In practice a handler rarely needs the symbol at all: every message header
    carries [stock_locate], a small dense integer that identifies the security
    for the day, and the Stock Directory spin at the start of the session maps
    locate to symbol once. Keying off the locate is not a workaround for the
    allocation -- it is what the field is for.

    Every callback below carries [@@zero_alloc], which turns the paragraph above
    from a claim into a contract. On OxCaml the compiler refuses to accept an
    implementation of this signature whose callbacks can allocate, and refuses
    to accept {!Reader.Make} unless the whole dispatch loop is allocation free.
    Vanilla OCaml ignores the attribute, so the same source builds on both; what
    differs is whether the property is proved or merely tested. It is worth
    having both: the checker found a 64-byte closure in [Reader.consume] that
    the runtime test could not see, because that test asserts only that
    allocation does not grow with message count.

    Note that [@@zero_alloc] without [strict] permits allocation on the
    exceptional path, which is what is wanted here -- {!Reader.check_length}
    raises on a malformed length, and building that exception allocates. *)

module type S = sig
  (** Handler state. Threaded explicitly rather than captured in a closure, so a
      single functor application can drive many independent consumers. *)
  type t

  val on_system_event
    :  t
    -> stock_locate:int
    -> tracking_number:int
    -> timestamp:int
    -> event_code:char
    -> unit
  [@@zero_alloc]

  (** Once per symbol per session, so this one hands over the buffer and expects
      the handler to read the fields it wants through {!Wire.Stock_directory}. *)
  val on_stock_directory
    :  t
    -> Bigstring.t
    -> pos:int
    -> stock_locate:int
    -> timestamp:int
    -> unit
  [@@zero_alloc]

  (** [attributed] is true for an "F" message, in which case the four byte MPID
      sits at [Wire.Add_order.attribution_pos ~pos]. *)
  val on_add_order
    :  t
    -> Bigstring.t
    -> pos:int
    -> stock_locate:int
    -> timestamp:int
    -> order_ref:int
    -> side:char
    -> shares:int
    -> price:int
    -> attributed:bool
    -> unit
  [@@zero_alloc]

  val on_order_executed
    :  t
    -> stock_locate:int
    -> timestamp:int
    -> order_ref:int
    -> executed_shares:int
    -> match_number:int
    -> unit
  [@@zero_alloc]

  val on_order_executed_with_price
    :  t
    -> stock_locate:int
    -> timestamp:int
    -> order_ref:int
    -> executed_shares:int
    -> match_number:int
    -> printable:bool
    -> execution_price:int
    -> unit
  [@@zero_alloc]

  val on_order_cancel
    :  t
    -> stock_locate:int
    -> timestamp:int
    -> order_ref:int
    -> cancelled_shares:int
    -> unit
  [@@zero_alloc]

  val on_order_delete : t -> stock_locate:int -> timestamp:int -> order_ref:int -> unit
  [@@zero_alloc]

  val on_order_replace
    :  t
    -> stock_locate:int
    -> timestamp:int
    -> original_order_ref:int
    -> new_order_ref:int
    -> shares:int
    -> price:int
    -> unit
  [@@zero_alloc]

  (** A message type this parser does not decode. Surfaced rather than skipped
      silently, matching the [Unparsed] case of {!Message.t}. *)
  val on_other : t -> Bigstring.t -> pos:int -> message_type:char -> length:int -> unit
  [@@zero_alloc]
end

(** No-op implementations, so a handler can [include Ignore_all] and then define
    only the callbacks it cares about -- a later definition shadows the one that
    came in through the include. *)
module Ignore_all (T : sig
    type t
  end) : S with type t = T.t = struct
  type t = T.t

  let on_system_event _ ~stock_locate:_ ~tracking_number:_ ~timestamp:_ ~event_code:_ = ()
  let on_stock_directory _ _ ~pos:_ ~stock_locate:_ ~timestamp:_ = ()

  let on_add_order
        _
        _
        ~pos:_
        ~stock_locate:_
        ~timestamp:_
        ~order_ref:_
        ~side:_
        ~shares:_
        ~price:_
        ~attributed:_
    =
    ()
  ;;

  let on_order_executed
        _
        ~stock_locate:_
        ~timestamp:_
        ~order_ref:_
        ~executed_shares:_
        ~match_number:_
    =
    ()
  ;;

  let on_order_executed_with_price
        _
        ~stock_locate:_
        ~timestamp:_
        ~order_ref:_
        ~executed_shares:_
        ~match_number:_
        ~printable:_
        ~execution_price:_
    =
    ()
  ;;

  let on_order_cancel _ ~stock_locate:_ ~timestamp:_ ~order_ref:_ ~cancelled_shares:_ = ()
  let on_order_delete _ ~stock_locate:_ ~timestamp:_ ~order_ref:_ = ()

  let on_order_replace
        _
        ~stock_locate:_
        ~timestamp:_
        ~original_order_ref:_
        ~new_order_ref:_
        ~shares:_
        ~price:_
    =
    ()
  ;;

  let on_other _ _ ~pos:_ ~message_type:_ ~length:_ = ()
end
