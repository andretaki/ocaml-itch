open! Core

(** Raw field accessors: the single place any ITCH byte offset is written down.

    Both decoding paths go through here -- the allocating {!Parser} that builds a
    {!Message.t}, and the zero-allocation {!Reader.Make} functor that hands
    immediates to a handler. Two hand-maintained copies of an offset table is how
    the fast path and the correct path quietly stop agreeing.

    Everything here returns an immediate (an [int] or a [char]) and so allocates
    nothing. Offsets are relative to the first byte of the message, i.e. the
    message type byte itself, matching the tables in the specification. *)

(** {1 Header, common to every message} *)

(** Core's [Bigstring.get_uint32_be] reads through a boxed [Int32.t]
    (base_bigstring.ml: [get_uint32_be t ~pos = uint32_of_int32_t (get_int32_t_be
    t ~pos)]) and so allocates three words on every call. Since a 32-bit field is
    read for a share count, a price or a timestamp in nearly every ITCH message,
    that alone put roughly six words per message on the parse path.

    [get_int32_be] reads straight to an unboxed [int]; masking recovers the
    unsigned value exactly, including above 2^31 where the signed read comes back
    negative. The two are checked equal across the range in
    [test/test_zero_alloc.ml]. *)

(** {2 Bounds checking}

    Every accessor below uses an unsafe (unchecked) read. That is sound because
    of where they are called from: [Reader.consume] establishes from the framing
    that the message's declared bytes are present in the buffer, and
    [check_length] establishes that the declared length is exactly the width the
    specification gives for that message type. Every offset here is smaller than
    that width, so it is in bounds by construction rather than by hope. Callers
    outside that path should go through {!Parser}, which checks. *)

let[@inline] uint32_be buf ~pos = Bigstring.unsafe_get_int32_be buf ~pos land 0xFFFF_FFFF
let[@inline] uint16_be buf ~pos = Bigstring.unsafe_get_uint16_be buf ~pos
let[@inline] uint64_be buf ~pos = Bigstring.unsafe_get_uint64_be_exn buf ~pos
let[@inline] char buf ~pos = Bigstring.unsafe_get buf pos
let[@inline] message_type buf ~pos = char buf ~pos
let[@inline] stock_locate buf ~pos = uint16_be buf ~pos:(pos + 1)
let[@inline] tracking_number buf ~pos = uint16_be buf ~pos:(pos + 3)

(** 48 bits, for which there is no native accessor: assembled from a 16-bit and a
    32-bit read. Both halves are unsigned and the total is 48 bits, so this fits
    an OCaml [int] with room to spare and never overflows. *)
let[@inline] timestamp buf ~pos =
  let high = uint16_be buf ~pos:(pos + 5) in
  let low = uint32_be buf ~pos:(pos + 7) in
  (high lsl 32) lor low
;;

let header_length = 11

(** {1 Per message fields}

    Alpha fields expose their {i position} rather than their value: reading one
    means building a string, which allocates, so the choice belongs to the caller
    rather than to this module. *)

module System_event = struct
  let length = 12
  let event_code buf ~pos = char buf ~pos:(pos + 11)
end

module Stock_directory = struct
  let length = 39
  let stock_pos ~pos = pos + 11
  let market_category buf ~pos = char buf ~pos:(pos + 19)
  let financial_status buf ~pos = char buf ~pos:(pos + 20)
  let round_lot_size buf ~pos = uint32_be buf ~pos:(pos + 21)
  let round_lots_only buf ~pos = char buf ~pos:(pos + 25)
  let issue_classification buf ~pos = char buf ~pos:(pos + 26)
  let issue_sub_type_pos ~pos = pos + 27
  let authenticity buf ~pos = char buf ~pos:(pos + 29)
  let short_sale_threshold_indicator buf ~pos = char buf ~pos:(pos + 30)
  let ipo_flag buf ~pos = char buf ~pos:(pos + 31)
  let luld_reference_price_tier buf ~pos = char buf ~pos:(pos + 32)
  let etp_flag buf ~pos = char buf ~pos:(pos + 33)
  let etp_leverage_factor buf ~pos = uint32_be buf ~pos:(pos + 34)
  let inverse_indicator buf ~pos = char buf ~pos:(pos + 38)
end

module Add_order = struct
  let length_without_mpid = 36
  let length_with_mpid = 40
  let[@inline] order_ref buf ~pos = uint64_be buf ~pos:(pos + 11)
  let[@inline] side buf ~pos = char buf ~pos:(pos + 19)
  let[@inline] shares buf ~pos = uint32_be buf ~pos:(pos + 20)
  let stock_pos ~pos = pos + 24
  let[@inline] price buf ~pos = uint32_be buf ~pos:(pos + 32)
  let attribution_pos ~pos = pos + 36
end

module Order_executed = struct
  let length = 31
  let[@inline] order_ref buf ~pos = uint64_be buf ~pos:(pos + 11)
  let executed_shares buf ~pos = uint32_be buf ~pos:(pos + 19)
  let match_number buf ~pos = uint64_be buf ~pos:(pos + 23)
end

module Order_executed_with_price = struct
  let length = 36
  let[@inline] order_ref buf ~pos = uint64_be buf ~pos:(pos + 11)
  let executed_shares buf ~pos = uint32_be buf ~pos:(pos + 19)
  let match_number buf ~pos = uint64_be buf ~pos:(pos + 23)
  let printable buf ~pos = char buf ~pos:(pos + 31)
  let execution_price buf ~pos = uint32_be buf ~pos:(pos + 32)
end

module Order_cancel = struct
  let length = 23
  let[@inline] order_ref buf ~pos = uint64_be buf ~pos:(pos + 11)
  let cancelled_shares buf ~pos = uint32_be buf ~pos:(pos + 19)
end

module Order_delete = struct
  let length = 19
  let[@inline] order_ref buf ~pos = uint64_be buf ~pos:(pos + 11)
end

module Order_replace = struct
  let length = 35
  let original_order_ref buf ~pos = uint64_be buf ~pos:(pos + 11)
  let new_order_ref buf ~pos = uint64_be buf ~pos:(pos + 19)
  let[@inline] shares buf ~pos = uint32_be buf ~pos:(pos + 27)
  let[@inline] price buf ~pos = uint32_be buf ~pos:(pos + 31)
end

(** Read a space padded alpha field as a string. This allocates, which is why it
    is here and not inlined into a hot loop. *)
let padded_string buf ~pos ~len =
  String.rstrip ~drop:(Char.equal ' ') (Bigstring.To_string.sub buf ~pos ~len)
;;
