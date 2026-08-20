open! Core
open Types

(* Spec, "Data Types": "All integer fields are big endian (network byte order)
   binary encoded numbers. Unless otherwise noted, they are unsigned." *)

(** Timestamps are a 48-bit field, for which there is no native accessor.
    Assembled from a 16-bit and a 32-bit read; 48 bits fits an OCaml [int]
    with room to spare, so this never overflows. *)
let get_timestamp buf ~pos =
  let high = Bigstring.get_uint16_be buf ~pos in
  let low = Bigstring.get_uint32_be buf ~pos:(pos + 2) in
  Timestamp.of_ns_since_midnight ((high lsl 32) lor low)
;;

let get_stock buf ~pos = Stock.of_padded_string (Bigstring.To_string.sub buf ~pos ~len:8)

let check_length ~message_type ~expected ~actual =
  if expected <> actual
  then
    raise_s
      [%message
        "ITCH message length disagrees with the spec"
          (message_type : char)
          ~spec_length:(expected : int)
          ~framed_length:(actual : int)]
;;

let parse_system_event buf ~pos ~len : Message.System_event.t =
  check_length ~message_type:'S' ~expected:Message.System_event.length ~actual:len;
  { stock_locate = Locate.of_int (Bigstring.get_uint16_be buf ~pos:(pos + 1))
  ; tracking_number = Bigstring.get_uint16_be buf ~pos:(pos + 3)
  ; timestamp = get_timestamp buf ~pos:(pos + 5)
  ; event_code = Message.Event_code.of_char_exn (Bigstring.get buf (pos + 11))
  }
;;

let parse_stock_directory buf ~pos ~len : Message.Stock_directory.t =
  check_length ~message_type:'R' ~expected:Message.Stock_directory.length ~actual:len;
  { stock_locate = Locate.of_int (Bigstring.get_uint16_be buf ~pos:(pos + 1))
  ; tracking_number = Bigstring.get_uint16_be buf ~pos:(pos + 3)
  ; timestamp = get_timestamp buf ~pos:(pos + 5)
  ; stock = get_stock buf ~pos:(pos + 11)
  ; market_category = Message.Market_category.of_char_exn (Bigstring.get buf (pos + 19))
  ; financial_status = Message.Financial_status.of_char_exn (Bigstring.get buf (pos + 20))
  ; round_lot_size = Bigstring.get_uint32_be buf ~pos:(pos + 21)
  ; round_lots_only =
      (match Bigstring.get buf (pos + 25) with
       | 'Y' -> true
       | 'N' -> false
       | c -> raise_s [%message "unknown ITCH round lots only flag" (c : char)])
  ; issue_classification = Bigstring.get buf (pos + 26)
  ; issue_sub_type = Bigstring.To_string.sub buf ~pos:(pos + 27) ~len:2
  ; authenticity = Message.Authenticity.of_char_exn (Bigstring.get buf (pos + 29))
  ; short_sale_threshold_indicator =
      Message.Yes_no.of_char_exn (Bigstring.get buf (pos + 30))
  ; ipo_flag = Message.Yes_no.of_char_exn (Bigstring.get buf (pos + 31))
  ; luld_reference_price_tier =
      Message.Luld_reference_price_tier.of_char_exn (Bigstring.get buf (pos + 32))
  ; etp_flag = Message.Yes_no.of_char_exn (Bigstring.get buf (pos + 33))
  ; etp_leverage_factor = Bigstring.get_uint32_be buf ~pos:(pos + 34)
  ; inverse_indicator =
      (match Bigstring.get buf (pos + 38) with
       | 'Y' -> true
       | 'N' -> false
       | c -> raise_s [%message "unknown ITCH inverse indicator" (c : char)])
  }
;;

let parse_add_order buf ~pos ~len ~with_mpid : Message.Add_order.t =
  let message_type, expected =
    match with_mpid with
    | false -> 'A', Message.Add_order.length_without_mpid
    | true -> 'F', Message.Add_order.length_with_mpid
  in
  check_length ~message_type ~expected ~actual:len;
  { stock_locate = Locate.of_int (Bigstring.get_uint16_be buf ~pos:(pos + 1))
  ; tracking_number = Bigstring.get_uint16_be buf ~pos:(pos + 3)
  ; timestamp = get_timestamp buf ~pos:(pos + 5)
  ; order_ref = Order_ref.of_uint64_exn (Bigstring.get_uint64_be_exn buf ~pos:(pos + 11))
  ; side = Side.of_char_exn (Bigstring.get buf (pos + 19))
  ; shares = Shares.of_int (Bigstring.get_uint32_be buf ~pos:(pos + 20))
  ; stock = get_stock buf ~pos:(pos + 24)
  ; price = Price.of_raw_4 (Bigstring.get_uint32_be buf ~pos:(pos + 32))
  ; attribution =
      (if with_mpid
       then
         Some
           (String.rstrip
              ~drop:(Char.equal ' ')
              (Bigstring.To_string.sub buf ~pos:(pos + 36) ~len:4))
       else None)
  }
;;

let get_order_ref buf ~pos =
  Order_ref.of_uint64_exn (Bigstring.get_uint64_be_exn buf ~pos)
;;

let get_printable buf ~pos =
  match Bigstring.get buf pos with
  | 'Y' -> true
  | 'N' -> false
  | c -> raise_s [%message "unknown ITCH printable flag" (c : char)]
;;

let parse_order_executed buf ~pos ~len : Message.Order_executed.t =
  check_length ~message_type:'E' ~expected:Message.Order_executed.length ~actual:len;
  { stock_locate = Locate.of_int (Bigstring.get_uint16_be buf ~pos:(pos + 1))
  ; tracking_number = Bigstring.get_uint16_be buf ~pos:(pos + 3)
  ; timestamp = get_timestamp buf ~pos:(pos + 5)
  ; order_ref = get_order_ref buf ~pos:(pos + 11)
  ; executed_shares = Shares.of_int (Bigstring.get_uint32_be buf ~pos:(pos + 19))
  ; match_number =
      Match_number.of_uint64_exn (Bigstring.get_uint64_be_exn buf ~pos:(pos + 23))
  }
;;

let parse_order_executed_with_price buf ~pos ~len : Message.Order_executed_with_price.t =
  check_length
    ~message_type:'C'
    ~expected:Message.Order_executed_with_price.length
    ~actual:len;
  { stock_locate = Locate.of_int (Bigstring.get_uint16_be buf ~pos:(pos + 1))
  ; tracking_number = Bigstring.get_uint16_be buf ~pos:(pos + 3)
  ; timestamp = get_timestamp buf ~pos:(pos + 5)
  ; order_ref = get_order_ref buf ~pos:(pos + 11)
  ; executed_shares = Shares.of_int (Bigstring.get_uint32_be buf ~pos:(pos + 19))
  ; match_number =
      Match_number.of_uint64_exn (Bigstring.get_uint64_be_exn buf ~pos:(pos + 23))
  ; printable = get_printable buf ~pos:(pos + 31)
  ; execution_price = Price.of_raw_4 (Bigstring.get_uint32_be buf ~pos:(pos + 32))
  }
;;

let parse_order_cancel buf ~pos ~len : Message.Order_cancel.t =
  check_length ~message_type:'X' ~expected:Message.Order_cancel.length ~actual:len;
  { stock_locate = Locate.of_int (Bigstring.get_uint16_be buf ~pos:(pos + 1))
  ; tracking_number = Bigstring.get_uint16_be buf ~pos:(pos + 3)
  ; timestamp = get_timestamp buf ~pos:(pos + 5)
  ; order_ref = get_order_ref buf ~pos:(pos + 11)
  ; cancelled_shares = Shares.of_int (Bigstring.get_uint32_be buf ~pos:(pos + 19))
  }
;;

let parse_order_delete buf ~pos ~len : Message.Order_delete.t =
  check_length ~message_type:'D' ~expected:Message.Order_delete.length ~actual:len;
  { stock_locate = Locate.of_int (Bigstring.get_uint16_be buf ~pos:(pos + 1))
  ; tracking_number = Bigstring.get_uint16_be buf ~pos:(pos + 3)
  ; timestamp = get_timestamp buf ~pos:(pos + 5)
  ; order_ref = get_order_ref buf ~pos:(pos + 11)
  }
;;

let parse_order_replace buf ~pos ~len : Message.Order_replace.t =
  check_length ~message_type:'U' ~expected:Message.Order_replace.length ~actual:len;
  { stock_locate = Locate.of_int (Bigstring.get_uint16_be buf ~pos:(pos + 1))
  ; tracking_number = Bigstring.get_uint16_be buf ~pos:(pos + 3)
  ; timestamp = get_timestamp buf ~pos:(pos + 5)
  ; original_order_ref = get_order_ref buf ~pos:(pos + 11)
  ; new_order_ref = get_order_ref buf ~pos:(pos + 19)
  ; shares = Shares.of_int (Bigstring.get_uint32_be buf ~pos:(pos + 27))
  ; price = Price.of_raw_4 (Bigstring.get_uint32_be buf ~pos:(pos + 31))
  }
;;

let parse_exn buf ~pos ~len : Message.t =
  match Bigstring.get buf pos with
  | 'S' -> System_event (parse_system_event buf ~pos ~len)
  | 'R' -> Stock_directory (parse_stock_directory buf ~pos ~len)
  | 'A' -> Add_order (parse_add_order buf ~pos ~len ~with_mpid:false)
  | 'F' -> Add_order (parse_add_order buf ~pos ~len ~with_mpid:true)
  | 'E' -> Order_executed (parse_order_executed buf ~pos ~len)
  | 'C' -> Order_executed_with_price (parse_order_executed_with_price buf ~pos ~len)
  | 'X' -> Order_cancel (parse_order_cancel buf ~pos ~len)
  | 'D' -> Order_delete (parse_order_delete buf ~pos ~len)
  | 'U' -> Order_replace (parse_order_replace buf ~pos ~len)
  | message_type -> Unparsed { message_type; length = len }
;;

let parse buf ~pos ~len = Or_error.try_with (fun () -> parse_exn buf ~pos ~len)
