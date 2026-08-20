open! Core
open Itch
module G = Base_quickcheck.Generator

(* Generators produce wire-legal values only. That constraint is the interesting
   part: a generator that emits a 40-bit locate code or a stock symbol with a
   trailing space would fail the round trip without either side being wrong, and
   chasing those is how property tests get abandoned. Each bound below is the
   width of the field in the specification. *)

let locate = G.map (G.int_inclusive 0 0xFFFF) ~f:Types.Locate.of_int
let tracking_number = G.int_inclusive 0 0xFFFF
let uint32 = G.int_inclusive 0 0xFFFF_FFFF

let timestamp =
  (* 48 bits: nanoseconds since midnight. *)
  G.map (G.int_inclusive 0 ((1 lsl 48) - 1)) ~f:Types.Timestamp.of_ns_since_midnight
;;

let price = G.map uint32 ~f:Types.Price.of_raw_4
let shares = G.map uint32 ~f:Types.Shares.of_int

(* 64 bit unsigned on the wire, but the parser rejects anything that does not fit
   a native int, so that is the range worth generating. *)
let order_ref = G.map (G.int_inclusive 0 Int.max_value) ~f:Types.Order_ref.of_uint64_exn

let match_number =
  G.map (G.int_inclusive 0 Int.max_value) ~f:Types.Match_number.of_uint64_exn
;;

let alpha_char = G.of_list (String.to_list "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789.")

(* Alpha fields are right padded with spaces and stripped on the way back in, so
   a generated value must not end in one -- it would not survive the round trip,
   and that is the format's behaviour rather than a defect. *)
let alpha ~max_length =
  G.bind (G.int_inclusive 1 max_length) ~f:(fun length ->
    G.map (G.list_with_length ~length alpha_char) ~f:String.of_char_list)
;;

let stock = G.map (alpha ~max_length:8) ~f:Types.Stock.of_padded_string
let yes_no = G.of_list [ Some true; Some false; None ]

let header f =
  G.bind locate ~f:(fun stock_locate ->
    G.bind tracking_number ~f:(fun tracking_number ->
      G.bind timestamp ~f:(fun timestamp -> f ~stock_locate ~tracking_number ~timestamp)))
;;

let system_event =
  header (fun ~stock_locate ~tracking_number ~timestamp ->
    G.map (G.of_list Message.Event_code.all) ~f:(fun event_code : Message.t ->
      System_event { stock_locate; tracking_number; timestamp; event_code }))
;;

let stock_directory =
  header (fun ~stock_locate ~tracking_number ~timestamp ->
    let open G.Let_syntax in
    let%bind stock = stock in
    let%bind market_category = G.of_list Message.Market_category.all in
    let%bind financial_status = G.of_list Message.Financial_status.all in
    let%bind round_lot_size = uint32 in
    let%bind round_lots_only = G.bool in
    let%bind issue_classification = alpha_char in
    let%bind issue_sub_type = alpha ~max_length:2 in
    let%bind authenticity = G.of_list Message.Authenticity.all in
    let%bind short_sale_threshold_indicator = yes_no in
    let%bind ipo_flag = yes_no in
    let%bind luld_reference_price_tier =
      G.of_list Message.Luld_reference_price_tier.all
    in
    let%bind etp_flag = yes_no in
    let%bind etp_leverage_factor = uint32 in
    let%map inverse_indicator = G.bool in
    (Stock_directory
       { stock_locate
       ; tracking_number
       ; timestamp
       ; stock
       ; market_category
       ; financial_status
       ; round_lot_size
       ; round_lots_only
       ; issue_classification
       ; issue_sub_type
       ; authenticity
       ; short_sale_threshold_indicator
       ; ipo_flag
       ; luld_reference_price_tier
       ; etp_flag
       ; etp_leverage_factor
       ; inverse_indicator
       }
     : Message.t))
;;

let add_order =
  header (fun ~stock_locate ~tracking_number ~timestamp ->
    let open G.Let_syntax in
    let%bind order_ref = order_ref in
    let%bind side = G.of_list Types.Side.all in
    let%bind shares = shares in
    let%bind stock = stock in
    let%bind price = price in
    let%map attribution = G.option (alpha ~max_length:4) in
    (Add_order
       { stock_locate
       ; tracking_number
       ; timestamp
       ; order_ref
       ; side
       ; shares
       ; stock
       ; price
       ; attribution
       }
     : Message.t))
;;

let order_executed =
  header (fun ~stock_locate ~tracking_number ~timestamp ->
    let open G.Let_syntax in
    let%bind order_ref = order_ref in
    let%bind executed_shares = shares in
    let%map match_number = match_number in
    (Order_executed
       { stock_locate
       ; tracking_number
       ; timestamp
       ; order_ref
       ; executed_shares
       ; match_number
       }
     : Message.t))
;;

let order_executed_with_price =
  header (fun ~stock_locate ~tracking_number ~timestamp ->
    let open G.Let_syntax in
    let%bind order_ref = order_ref in
    let%bind executed_shares = shares in
    let%bind match_number = match_number in
    let%bind printable = G.bool in
    let%map execution_price = price in
    (Order_executed_with_price
       { stock_locate
       ; tracking_number
       ; timestamp
       ; order_ref
       ; executed_shares
       ; match_number
       ; printable
       ; execution_price
       }
     : Message.t))
;;

let order_cancel =
  header (fun ~stock_locate ~tracking_number ~timestamp ->
    let open G.Let_syntax in
    let%bind order_ref = order_ref in
    let%map cancelled_shares = shares in
    (Order_cancel
       { stock_locate; tracking_number; timestamp; order_ref; cancelled_shares }
     : Message.t))
;;

let order_delete =
  header (fun ~stock_locate ~tracking_number ~timestamp ->
    G.map order_ref ~f:(fun order_ref : Message.t ->
      Order_delete { stock_locate; tracking_number; timestamp; order_ref }))
;;

let order_replace =
  header (fun ~stock_locate ~tracking_number ~timestamp ->
    let open G.Let_syntax in
    let%bind original_order_ref = order_ref in
    let%bind new_order_ref = order_ref in
    let%bind shares = shares in
    let%map price = price in
    (Order_replace
       { stock_locate
       ; tracking_number
       ; timestamp
       ; original_order_ref
       ; new_order_ref
       ; shares
       ; price
       }
     : Message.t))
;;

let message =
  G.union
    [ system_event
    ; stock_directory
    ; add_order
    ; order_executed
    ; order_executed_with_price
    ; order_cancel
    ; order_delete
    ; order_replace
    ]
;;

module Message_for_test = struct
  type t = Message.t [@@deriving sexp_of]

  let quickcheck_generator = message
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

(* The framed encoding is what the file format uses, so the round trip goes
   through the framing too: encode, then read it back with the same Reader that
   walks a real file. That way the length prefix is under test as well. *)
let%expect_test "encode then decode is the identity, through the framing" =
  Base_quickcheck.Test.run_exn
    (module Message_for_test)
    ~config:{ Base_quickcheck.Test.default_config with test_count = 20_000 }
    ~f:(fun message ->
      let framed = Encoder.to_framed_bigstring message in
      let decoded = ref [] in
      let consumed =
        Reader.iter framed ~pos:0 ~len:(Bigstring.length framed) ~f:(fun message ->
          decoded := message :: !decoded)
      in
      match !decoded with
      | [ only ] when Message.equal only message && consumed = Bigstring.length framed ->
        ()
      | other ->
        raise_s
          [%message
            "round trip failed"
              ~original:(message : Message.t)
              ~decoded:(other : Message.t list)
              (consumed : int)]);
  [%expect {| |}]
;;
