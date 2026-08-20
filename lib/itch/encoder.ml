open! Core
open Types

(** Encode a {!Message.t} back to its wire representation.

    This exists to make the decoder testable. A decoder can only be checked
    against vectors someone wrote by hand, and hand-written vectors test the
    cases you already thought of. With an encoder, [decode (encode m) = m] can be
    checked against thousands of generated messages, including the field values
    nobody would think to write down.

    It is deliberately the plain, obvious implementation: the decoder is the side
    that has to be fast. If both sides shared a clever offset table, a mistake in
    the table would cancel out and the round trip would still pass. *)

let set_timestamp buf ~pos timestamp =
  let ns = Timestamp.to_ns_since_midnight timestamp in
  Bigstring.set_uint16_be_exn buf ~pos (ns lsr 32);
  Bigstring.set_uint32_be_exn buf ~pos:(pos + 2) (ns land 0xFFFF_FFFF)
;;

(** Alpha fields are fixed width and right padded with spaces. *)
let set_padded buf ~pos ~len string =
  let string_length = String.length string in
  if string_length > len
  then
    raise_s
      [%message
        "string too long for ITCH alpha field" (string : string) ~field_width:(len : int)];
  Bigstring.From_string.blit
    ~src:string
    ~src_pos:0
    ~dst:buf
    ~dst_pos:pos
    ~len:string_length;
  for i = string_length to len - 1 do
    Bigstring.set buf (pos + i) ' '
  done
;;

let set_header buf ~pos ~stock_locate ~tracking_number ~timestamp =
  Bigstring.set_uint16_be_exn buf ~pos:(pos + 1) (Locate.to_int stock_locate);
  Bigstring.set_uint16_be_exn buf ~pos:(pos + 3) tracking_number;
  set_timestamp buf ~pos:(pos + 5) timestamp
;;

let length (message : Message.t) =
  match message with
  | System_event _ -> Message.System_event.length
  | Stock_directory _ -> Message.Stock_directory.length
  | Add_order { attribution = None; _ } -> Message.Add_order.length_without_mpid
  | Add_order { attribution = Some _; _ } -> Message.Add_order.length_with_mpid
  | Order_executed _ -> Message.Order_executed.length
  | Order_executed_with_price _ -> Message.Order_executed_with_price.length
  | Order_cancel _ -> Message.Order_cancel.length
  | Order_delete _ -> Message.Order_delete.length
  | Order_replace _ -> Message.Order_replace.length
  | Unparsed { message_type; _ } ->
    raise_s
      [%message
        "cannot encode an Unparsed message: its bytes were never decoded"
          (message_type : char)]
;;

let encode buf ~pos (message : Message.t) =
  Bigstring.set buf pos (Message.message_type message);
  match message with
  | System_event m ->
    set_header
      buf
      ~pos
      ~stock_locate:m.stock_locate
      ~tracking_number:m.tracking_number
      ~timestamp:m.timestamp;
    Bigstring.set buf (pos + 11) (Message.Event_code.to_char m.event_code)
  | Stock_directory m ->
    set_header
      buf
      ~pos
      ~stock_locate:m.stock_locate
      ~tracking_number:m.tracking_number
      ~timestamp:m.timestamp;
    set_padded buf ~pos:(pos + 11) ~len:8 (Stock.to_string m.stock);
    Bigstring.set buf (pos + 19) (Message.Market_category.to_char m.market_category);
    Bigstring.set buf (pos + 20) (Message.Financial_status.to_char m.financial_status);
    Bigstring.set_uint32_be_exn buf ~pos:(pos + 21) m.round_lot_size;
    Bigstring.set buf (pos + 25) (if m.round_lots_only then 'Y' else 'N');
    Bigstring.set buf (pos + 26) m.issue_classification;
    set_padded buf ~pos:(pos + 27) ~len:2 m.issue_sub_type;
    Bigstring.set buf (pos + 29) (Message.Authenticity.to_char m.authenticity);
    Bigstring.set buf (pos + 30) (Message.Yes_no.to_char m.short_sale_threshold_indicator);
    Bigstring.set buf (pos + 31) (Message.Yes_no.to_char m.ipo_flag);
    Bigstring.set
      buf
      (pos + 32)
      (Message.Luld_reference_price_tier.to_char m.luld_reference_price_tier);
    Bigstring.set buf (pos + 33) (Message.Yes_no.to_char m.etp_flag);
    Bigstring.set_uint32_be_exn buf ~pos:(pos + 34) m.etp_leverage_factor;
    Bigstring.set buf (pos + 38) (if m.inverse_indicator then 'Y' else 'N')
  | Add_order m ->
    set_header
      buf
      ~pos
      ~stock_locate:m.stock_locate
      ~tracking_number:m.tracking_number
      ~timestamp:m.timestamp;
    Bigstring.set_uint64_be_exn buf ~pos:(pos + 11) (Order_ref.to_int m.order_ref);
    Bigstring.set buf (pos + 19) (Side.to_char m.side);
    Bigstring.set_uint32_be_exn buf ~pos:(pos + 20) (Shares.to_int m.shares);
    set_padded buf ~pos:(pos + 24) ~len:8 (Stock.to_string m.stock);
    Bigstring.set_uint32_be_exn buf ~pos:(pos + 32) (Price.to_raw_4 m.price);
    Option.iter m.attribution ~f:(set_padded buf ~pos:(pos + 36) ~len:4)
  | Order_executed m ->
    set_header
      buf
      ~pos
      ~stock_locate:m.stock_locate
      ~tracking_number:m.tracking_number
      ~timestamp:m.timestamp;
    Bigstring.set_uint64_be_exn buf ~pos:(pos + 11) (Order_ref.to_int m.order_ref);
    Bigstring.set_uint32_be_exn buf ~pos:(pos + 19) (Shares.to_int m.executed_shares);
    Bigstring.set_uint64_be_exn buf ~pos:(pos + 23) (Match_number.to_int m.match_number)
  | Order_executed_with_price m ->
    set_header
      buf
      ~pos
      ~stock_locate:m.stock_locate
      ~tracking_number:m.tracking_number
      ~timestamp:m.timestamp;
    Bigstring.set_uint64_be_exn buf ~pos:(pos + 11) (Order_ref.to_int m.order_ref);
    Bigstring.set_uint32_be_exn buf ~pos:(pos + 19) (Shares.to_int m.executed_shares);
    Bigstring.set_uint64_be_exn buf ~pos:(pos + 23) (Match_number.to_int m.match_number);
    Bigstring.set buf (pos + 31) (if m.printable then 'Y' else 'N');
    Bigstring.set_uint32_be_exn buf ~pos:(pos + 32) (Price.to_raw_4 m.execution_price)
  | Order_cancel m ->
    set_header
      buf
      ~pos
      ~stock_locate:m.stock_locate
      ~tracking_number:m.tracking_number
      ~timestamp:m.timestamp;
    Bigstring.set_uint64_be_exn buf ~pos:(pos + 11) (Order_ref.to_int m.order_ref);
    Bigstring.set_uint32_be_exn buf ~pos:(pos + 19) (Shares.to_int m.cancelled_shares)
  | Order_delete m ->
    set_header
      buf
      ~pos
      ~stock_locate:m.stock_locate
      ~tracking_number:m.tracking_number
      ~timestamp:m.timestamp;
    Bigstring.set_uint64_be_exn buf ~pos:(pos + 11) (Order_ref.to_int m.order_ref)
  | Order_replace m ->
    set_header
      buf
      ~pos
      ~stock_locate:m.stock_locate
      ~tracking_number:m.tracking_number
      ~timestamp:m.timestamp;
    Bigstring.set_uint64_be_exn
      buf
      ~pos:(pos + 11)
      (Order_ref.to_int m.original_order_ref);
    Bigstring.set_uint64_be_exn buf ~pos:(pos + 19) (Order_ref.to_int m.new_order_ref);
    Bigstring.set_uint32_be_exn buf ~pos:(pos + 27) (Shares.to_int m.shares);
    Bigstring.set_uint32_be_exn buf ~pos:(pos + 31) (Price.to_raw_4 m.price)
  | Unparsed _ -> ignore (length message : int)
;;

let to_bigstring message =
  let len = length message in
  let buf = Bigstring.create len in
  encode buf ~pos:0 message;
  buf
;;

(** With the 2-byte big-endian length prefix, as used by the downloadable files. *)
let to_framed_bigstring message =
  let len = length message in
  let buf = Bigstring.create (len + Reader.length_prefix_bytes) in
  Bigstring.set_uint16_be_exn buf ~pos:0 len;
  encode buf ~pos:Reader.length_prefix_bytes message;
  buf
;;
