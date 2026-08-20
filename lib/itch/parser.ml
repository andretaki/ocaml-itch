open! Core
open Types

(** Decode a message into a {!Message.t}.

    This is the ergonomic path: it returns a real value you can pattern match on,
    and it allocates to do so -- a record per message, plus a string for every
    alpha field it reads. For the path that allocates nothing, see
    {!Reader.Make}. Both read their bytes through {!Wire}, so the two cannot
    drift apart on an offset. *)

let get_stock buf ~pos = Stock.of_padded_string (Wire.padded_string buf ~pos ~len:8)

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

let yes_no_exn c ~field =
  match c with
  | 'Y' -> true
  | 'N' -> false
  | c -> raise_s [%message "unknown ITCH Y/N field" (field : string) (c : char)]
;;

let parse_system_event buf ~pos ~len : Message.System_event.t =
  check_length ~message_type:'S' ~expected:Wire.System_event.length ~actual:len;
  { stock_locate = Locate.of_int (Wire.stock_locate buf ~pos)
  ; tracking_number = Wire.tracking_number buf ~pos
  ; timestamp = Timestamp.of_ns_since_midnight (Wire.timestamp buf ~pos)
  ; event_code = Message.Event_code.of_char_exn (Wire.System_event.event_code buf ~pos)
  }
;;

let parse_stock_directory buf ~pos ~len : Message.Stock_directory.t =
  check_length ~message_type:'R' ~expected:Wire.Stock_directory.length ~actual:len;
  let module F = Wire.Stock_directory in
  { stock_locate = Locate.of_int (Wire.stock_locate buf ~pos)
  ; tracking_number = Wire.tracking_number buf ~pos
  ; timestamp = Timestamp.of_ns_since_midnight (Wire.timestamp buf ~pos)
  ; stock = get_stock buf ~pos:(F.stock_pos ~pos)
  ; market_category = Message.Market_category.of_char_exn (F.market_category buf ~pos)
  ; financial_status = Message.Financial_status.of_char_exn (F.financial_status buf ~pos)
  ; round_lot_size = F.round_lot_size buf ~pos
  ; round_lots_only = yes_no_exn (F.round_lots_only buf ~pos) ~field:"round lots only"
  ; issue_classification = F.issue_classification buf ~pos
  ; issue_sub_type = Wire.padded_string buf ~pos:(F.issue_sub_type_pos ~pos) ~len:2
  ; authenticity = Message.Authenticity.of_char_exn (F.authenticity buf ~pos)
  ; short_sale_threshold_indicator =
      Message.Yes_no.of_char_exn (F.short_sale_threshold_indicator buf ~pos)
  ; ipo_flag = Message.Yes_no.of_char_exn (F.ipo_flag buf ~pos)
  ; luld_reference_price_tier =
      Message.Luld_reference_price_tier.of_char_exn (F.luld_reference_price_tier buf ~pos)
  ; etp_flag = Message.Yes_no.of_char_exn (F.etp_flag buf ~pos)
  ; etp_leverage_factor = F.etp_leverage_factor buf ~pos
  ; inverse_indicator =
      yes_no_exn (F.inverse_indicator buf ~pos) ~field:"inverse indicator"
  }
;;

let parse_add_order buf ~pos ~len ~with_mpid : Message.Add_order.t =
  let module F = Wire.Add_order in
  let message_type, expected =
    match with_mpid with
    | false -> 'A', F.length_without_mpid
    | true -> 'F', F.length_with_mpid
  in
  check_length ~message_type ~expected ~actual:len;
  { stock_locate = Locate.of_int (Wire.stock_locate buf ~pos)
  ; tracking_number = Wire.tracking_number buf ~pos
  ; timestamp = Timestamp.of_ns_since_midnight (Wire.timestamp buf ~pos)
  ; order_ref = Order_ref.of_uint64_exn (F.order_ref buf ~pos)
  ; side = Side.of_char_exn (F.side buf ~pos)
  ; shares = Shares.of_int (F.shares buf ~pos)
  ; stock = get_stock buf ~pos:(F.stock_pos ~pos)
  ; price = Price.of_raw_4 (F.price buf ~pos)
  ; attribution =
      (if with_mpid
       then Some (Wire.padded_string buf ~pos:(F.attribution_pos ~pos) ~len:4)
       else None)
  }
;;

let parse_order_executed buf ~pos ~len : Message.Order_executed.t =
  check_length ~message_type:'E' ~expected:Wire.Order_executed.length ~actual:len;
  let module F = Wire.Order_executed in
  { stock_locate = Locate.of_int (Wire.stock_locate buf ~pos)
  ; tracking_number = Wire.tracking_number buf ~pos
  ; timestamp = Timestamp.of_ns_since_midnight (Wire.timestamp buf ~pos)
  ; order_ref = Order_ref.of_uint64_exn (F.order_ref buf ~pos)
  ; executed_shares = Shares.of_int (F.executed_shares buf ~pos)
  ; match_number = Match_number.of_uint64_exn (F.match_number buf ~pos)
  }
;;

let parse_order_executed_with_price buf ~pos ~len : Message.Order_executed_with_price.t =
  check_length
    ~message_type:'C'
    ~expected:Wire.Order_executed_with_price.length
    ~actual:len;
  let module F = Wire.Order_executed_with_price in
  { stock_locate = Locate.of_int (Wire.stock_locate buf ~pos)
  ; tracking_number = Wire.tracking_number buf ~pos
  ; timestamp = Timestamp.of_ns_since_midnight (Wire.timestamp buf ~pos)
  ; order_ref = Order_ref.of_uint64_exn (F.order_ref buf ~pos)
  ; executed_shares = Shares.of_int (F.executed_shares buf ~pos)
  ; match_number = Match_number.of_uint64_exn (F.match_number buf ~pos)
  ; printable = yes_no_exn (F.printable buf ~pos) ~field:"printable"
  ; execution_price = Price.of_raw_4 (F.execution_price buf ~pos)
  }
;;

let parse_order_cancel buf ~pos ~len : Message.Order_cancel.t =
  check_length ~message_type:'X' ~expected:Wire.Order_cancel.length ~actual:len;
  let module F = Wire.Order_cancel in
  { stock_locate = Locate.of_int (Wire.stock_locate buf ~pos)
  ; tracking_number = Wire.tracking_number buf ~pos
  ; timestamp = Timestamp.of_ns_since_midnight (Wire.timestamp buf ~pos)
  ; order_ref = Order_ref.of_uint64_exn (F.order_ref buf ~pos)
  ; cancelled_shares = Shares.of_int (F.cancelled_shares buf ~pos)
  }
;;

let parse_order_delete buf ~pos ~len : Message.Order_delete.t =
  check_length ~message_type:'D' ~expected:Wire.Order_delete.length ~actual:len;
  { stock_locate = Locate.of_int (Wire.stock_locate buf ~pos)
  ; tracking_number = Wire.tracking_number buf ~pos
  ; timestamp = Timestamp.of_ns_since_midnight (Wire.timestamp buf ~pos)
  ; order_ref = Order_ref.of_uint64_exn (Wire.Order_delete.order_ref buf ~pos)
  }
;;

let parse_order_replace buf ~pos ~len : Message.Order_replace.t =
  check_length ~message_type:'U' ~expected:Wire.Order_replace.length ~actual:len;
  let module F = Wire.Order_replace in
  { stock_locate = Locate.of_int (Wire.stock_locate buf ~pos)
  ; tracking_number = Wire.tracking_number buf ~pos
  ; timestamp = Timestamp.of_ns_since_midnight (Wire.timestamp buf ~pos)
  ; original_order_ref = Order_ref.of_uint64_exn (F.original_order_ref buf ~pos)
  ; new_order_ref = Order_ref.of_uint64_exn (F.new_order_ref buf ~pos)
  ; shares = Shares.of_int (F.shares buf ~pos)
  ; price = Price.of_raw_4 (F.price buf ~pos)
  }
;;

let parse_exn buf ~pos ~len : Message.t =
  match Wire.message_type buf ~pos with
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
