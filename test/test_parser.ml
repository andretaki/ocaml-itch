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
      (issue_sub_type Z) (authenticity Live)
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
   complete while reporting honestly what it did not understand. 0x4c is "L",
   Market Participant Position, which is not decoded yet. *)
let%expect_test "unknown message type is surfaced, framing still advances" =
  dump
    {| 0005 4c00 0100 00 0024 4100 0100 001f 1ace d9f0 0000 0000 0000 0000 2a42 0000 0064 4141 504c 2020 2020 0012 d644 |};
  [%expect
    {|
    (Unparsed ((message_type L) (length 5)))
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

(* Every decoded message type has a fixed length in the spec, and the file's own
   framing states a length independently. When those two disagree the bytes are
   not what they claim to be, so the parser refuses rather than decoding
   whatever happens to be at the offsets. *)
let%expect_test "a framed length that disagrees with the spec is rejected" =
  let body = bigstring_of_hex {| 5800 0100 00 |} in
  print_s
    [%sexp (Parser.parse body ~pos:0 ~len:(Bigstring.length body) : Message.t Or_error.t)];
  [%expect
    {|
    (Error
     ("ITCH message length disagrees with the spec" (message_type X)
      (spec_length 23) (framed_length 5)))
    |}]
;;

(* The five modify-order messages, built by hand from the spec's section 1.4
   offset tables against order reference 42. *)
let%expect_test "modify order messages" =
  dump
    {|
      001f 4500 0100 001f 1ace d9f0 0000 0000
      0000 0000 2a00 0000 3200 0000 0000 0007
      b1
    |};
  dump
    {|
      0024 4300 0100 001f 1ace d9f0 0000 0000
      0000 0000 2a00 0000 3200 0000 0000 0007
      b159 0012 d5d0
    |};
  dump {| 0017 5800 0100 001f 1ace d9f0 0000 0000 0000 0000 2a00 0000 19 |};
  dump {| 0013 4400 0100 001f 1ace d9f0 0000 0000 0000 0000 2a |};
  dump
    {|
      0023 5500 0100 001f 1ace d9f0 0000 0000
      0000 0000 2a00 0000 0000 0000 2b00 0000
      c800 12d6 44
    |};
  [%expect
    {|
    (Order_executed
     ((stock_locate 1) (tracking_number 0) (timestamp 34200000000000)
      (order_ref 42) (executed_shares 50) (match_number 1969)))
    consumed 33 of 33 bytes
    (Order_executed_with_price
     ((stock_locate 1) (tracking_number 0) (timestamp 34200000000000)
      (order_ref 42) (executed_shares 50) (match_number 1969) (printable true)
      (execution_price 1234384)))
    consumed 38 of 38 bytes
    (Order_cancel
     ((stock_locate 1) (tracking_number 0) (timestamp 34200000000000)
      (order_ref 42) (cancelled_shares 25)))
    consumed 25 of 25 bytes
    (Order_delete
     ((stock_locate 1) (tracking_number 0) (timestamp 34200000000000)
      (order_ref 42)))
    consumed 21 of 21 bytes
    (Order_replace
     ((stock_locate 1) (tracking_number 0) (timestamp 34200000000000)
      (original_order_ref 42) (new_order_ref 43) (shares 200) (price 1234500)))
    consumed 37 of 37 bytes
    |}]
;;
