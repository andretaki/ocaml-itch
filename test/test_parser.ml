open! Core
open Itch

let bigstring_of_hex hex =
  let hex = String.filter hex ~f:(fun c -> not (Char.is_whitespace c)) in
  let len = String.length hex / 2 in
  let buf = Bigstring.create len in
  for i = 0 to len - 1 do
    Bigstring.set
      buf
      i
      (Char.of_int_exn (Int.of_string ("0x" ^ String.sub hex ~pos:(i * 2) ~len:2)))
  done;
  buf
;;

let dump hex =
  let buf = bigstring_of_hex hex in
  let consumed =
    Reader.iter buf ~pos:0 ~len:(Bigstring.length buf) ~f:(fun message ->
      print_s [%sexp (message : Message.t)])
  in
  printf "consumed %d of %d bytes\n" consumed (Bigstring.length buf)
;;

(* The first two messages of 01302020.NASDAQ_ITCH50, byte for byte, as served by
   emi.nasdaq.com. Ground truth for both the 2-byte length framing and the field
   offsets: symbol "A" is Agilent, NYSE listed, 100 share round lot, LULD tier 1. *)
let%expect_test "real file prefix: system event and stock directory" =
  dump
    {|
      000c 5300 0000 0009 f649 c80c d34f
      0027 5200 0100 000a 37d4 c805 0b41 2020 2020
      2020 204e 2000 0000 644e 435a 2050 4e20
      314e 0000 0000 4e
    |};
  [%expect
    {|
    (System_event
     ((stock_locate 0) (tracking_number 0) (timestamp 10953404452051)
      (event_code Start_of_messages)))
    (Stock_directory
     ((stock_locate 1) (tracking_number 0) (timestamp 11234909357323) (stock A)
      (market_category Nyse) (financial_status Not_available)
      (round_lot_size 100) (round_lots_only false) (issue_classification C)
      (issue_sub_type "Z ") (authenticity Live)
      (short_sale_threshold_indicator (false)) (ipo_flag ())
      (luld_reference_price_tier Tier_1) (etp_flag (false))
      (etp_leverage_factor 0) (inverse_indicator false)))
    consumed 55 of 55 bytes
    |}]
;;

(* Built by hand from the spec's section 1.3 offset tables: 100 shares of AAPL
   bid at $123.45, order reference 42, stamped 09:30:00.000000000. *)
let%expect_test "add order, without and with MPID attribution" =
  dump
    {|
      0024 4100 0100 001f 1ace d9f0 0000 0000
      0000 0000 2a42 0000 0064 4141 504c 2020
      2020 0012 d644
    |};
  dump
    {|
      0028 4600 0100 001f 1ace d9f0 0000 0000
      0000 0000 2a42 0000 0064 4141 504c 2020
      2020 0012 d644 4e53 4451
    |};
  [%expect
    {|
    (Add_order
     ((stock_locate 1) (tracking_number 0) (timestamp 34200000000000)
      (order_ref 42) (side Buy) (shares 100) (stock AAPL) (price 1234500)
      (attribution ())))
    consumed 38 of 38 bytes
    (Add_order
     ((stock_locate 1) (tracking_number 0) (timestamp 34200000000000)
      (order_ref 42) (side Buy) (shares 100) (stock AAPL) (price 1234500)
      (attribution (NSDQ))))
    consumed 42 of 42 bytes
    |}]
;;

(* A message type the parser does not decode yet must be surfaced, not skipped
   silently and not fatal: this is what lets a fold over a full trading day
   complete while reporting honestly what it did not understand. *)
let%expect_test "unknown message type is surfaced, framing still advances" =
  dump
    {| 0005 5800 0100 00 0024 4100 0100 001f 1ace d9f0 0000 0000 0000 0000 2a42 0000 0064 4141 504c 2020 2020 0012 d644 |};
  [%expect
    {|
    (Unparsed ((message_type X) (length 5)))
    (Add_order
     ((stock_locate 1) (tracking_number 0) (timestamp 34200000000000)
      (order_ref 42) (side Buy) (shares 100) (stock AAPL) (price 1234500)
      (attribution ())))
    consumed 45 of 45 bytes
    |}]
;;

(* A truncated trailing message is left unconsumed rather than raising, so a
   caller streaming a file in chunks can carry the remainder forward. *)
let%expect_test "partial trailing message is not consumed" =
  dump {| 000c 5300 0000 0009 f649 c80c d34f 0027 5200 01 |};
  [%expect
    {|
    (System_event
     ((stock_locate 0) (tracking_number 0) (timestamp 10953404452051)
      (event_code Start_of_messages)))
    consumed 14 of 19 bytes
    |}]
;;
