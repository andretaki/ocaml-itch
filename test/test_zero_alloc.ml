open! Core
open Itch

(* The corpus is built with the encoder rather than read from a file, so this
   runs in CI with no data dependency. *)

let locate n = Types.Locate.of_int n
let stock = Types.Stock.of_padded_string "TEST"
let ts n = Types.Timestamp.of_ns_since_midnight n
let order_ref n = Types.Order_ref.of_uint64_exn n
let shares n = Types.Shares.of_int n
let price n = Types.Price.of_raw_4 n

let sample_messages : Message.t list =
  [ System_event
      { stock_locate = locate 0
      ; tracking_number = 0
      ; timestamp = ts 1
      ; event_code = Start_of_messages
      }
  ; Stock_directory
      { stock_locate = locate 7
      ; tracking_number = 1
      ; timestamp = ts 2
      ; stock
      ; market_category = Nasdaq_global_select
      ; financial_status = Normal
      ; round_lot_size = 100
      ; round_lots_only = false
      ; issue_classification = 'C'
      ; issue_sub_type = "Z"
      ; authenticity = Live
      ; short_sale_threshold_indicator = Some false
      ; ipo_flag = None
      ; luld_reference_price_tier = Tier_1
      ; etp_flag = Some false
      ; etp_leverage_factor = 0
      ; inverse_indicator = false
      }
  ; Add_order
      { stock_locate = locate 7
      ; tracking_number = 2
      ; timestamp = ts 3
      ; order_ref = order_ref 1001
      ; side = Buy
      ; shares = shares 100
      ; stock
      ; price = price 1_000_000
      ; attribution = None
      }
  ; Add_order
      { stock_locate = locate 7
      ; tracking_number = 3
      ; timestamp = ts 4
      ; order_ref = order_ref 1002
      ; side = Sell
      ; shares = shares 250
      ; stock
      ; price = price 1_010_000
      ; attribution = Some "NSDQ"
      }
  ; Order_executed
      { stock_locate = locate 7
      ; tracking_number = 4
      ; timestamp = ts 5
      ; order_ref = order_ref 1001
      ; executed_shares = shares 50
      ; match_number = Types.Match_number.of_uint64_exn 9
      }
  ; Order_executed_with_price
      { stock_locate = locate 7
      ; tracking_number = 5
      ; timestamp = ts 6
      ; order_ref = order_ref 1001
      ; executed_shares = shares 10
      ; match_number = Types.Match_number.of_uint64_exn 10
      ; printable = true
      ; execution_price = price 999_000
      }
  ; Order_cancel
      { stock_locate = locate 7
      ; tracking_number = 6
      ; timestamp = ts 7
      ; order_ref = order_ref 1002
      ; cancelled_shares = shares 25
      }
  ; Order_delete
      { stock_locate = locate 7
      ; tracking_number = 7
      ; timestamp = ts 8
      ; order_ref = order_ref 1002
      }
  ; Order_replace
      { stock_locate = locate 7
      ; tracking_number = 8
      ; timestamp = ts 9
      ; original_order_ref = order_ref 1001
      ; new_order_ref = order_ref 1003
      ; shares = shares 500
      ; price = price 1_005_000
      }
  ]
;;

let corpus ~repeats =
  let one_round = List.map sample_messages ~f:Encoder.to_framed_bigstring in
  let round_length =
    List.sum (module Int) one_round ~f:(fun buf -> Bigstring.length buf)
  in
  let buf = Bigstring.create (round_length * repeats) in
  let pos = ref 0 in
  for _ = 1 to repeats do
    List.iter one_round ~f:(fun src ->
      let len = Bigstring.length src in
      Bigstring.blit ~src ~src_pos:0 ~dst:buf ~dst_pos:!pos ~len;
      pos := !pos + len)
  done;
  buf
;;

module R = Reader.Make (Checksum)

(* Measure one pass and then several, and take the difference. Any fixed cost in
   the harness itself is identical in both and cancels exactly; what survives is
   the allocation attributable to parsing, which must be zero. Core's
   [Gc.minor_words] returns an [int], so the measurement is exact and does not
   itself allocate a boxed float. *)
let words_for ~passes ~corpus ~state =
  let len = Bigstring.length corpus in
  let before = Gc.minor_words () in
  for _ = 1 to passes do
    ignore (R.consume state corpus ~pos:0 ~len : int)
  done;
  Gc.minor_words () - before
;;

(* "Allocates nothing" has to be stated carefully. Any harness has some fixed
   cost, so the claim that actually matters is that allocation does not grow with
   the number of messages parsed. Measuring two corpus sizes and differencing
   cancels the fixed cost exactly: whatever is left is allocation attributable to
   the extra messages, and it must be zero. *)
let%expect_test "parsing through a handler allocates nothing per message" =
  let small = corpus ~repeats:1_000 in
  let large = corpus ~repeats:4_000 in
  let state = Checksum.create () in
  ignore (words_for ~passes:1 ~corpus:small ~state : int);
  ignore (words_for ~passes:1 ~corpus:large ~state : int);
  let per_pass corpus =
    let one = words_for ~passes:1 ~corpus ~state in
    let nine = words_for ~passes:9 ~corpus ~state in
    (nine - one) / 8
  in
  let small_words = per_pass small in
  let large_words = per_pass large in
  let extra_messages = List.length sample_messages * 3_000 in
  printf "words per pass over  9,000 messages: %d\n" small_words;
  printf "words per pass over 36,000 messages: %d\n" large_words;
  printf "extra messages: %d\n" extra_messages;
  printf "words attributable to them: %d\n" (large_words - small_words);
  [%expect
    {|
    words per pass over  9,000 messages: 8
    words per pass over 36,000 messages: 8
    extra messages: 27000
    words attributable to them: 0
    |}]
;;

(* The allocating path is expected to allocate; this pins roughly how much, so
   the contrast is on the record and a regression in either direction shows up. *)
let%expect_test "for contrast, the Message.t path allocates" =
  let corpus = corpus ~repeats:2_000 in
  let len = Bigstring.length corpus in
  let count = ref 0 in
  let run () = ignore (Reader.iter corpus ~pos:0 ~len ~f:(fun _ -> incr count) : int) in
  run ();
  let before = Gc.minor_words () in
  run ();
  let one = Gc.minor_words () - before in
  let before = Gc.minor_words () in
  for _ = 1 to 9 do
    run ()
  done;
  let nine = Gc.minor_words () - before in
  let per_pass = (nine - one) / 8 in
  printf "minor words per message: %d\n" (per_pass / (List.length sample_messages * 2_000));
  [%expect {| minor words per message: 28 |}]
;;

(* The parse path reads a 32-bit field for a share count, a price or a timestamp
   in nearly every message. Core's [Bigstring.get_uint32_be] converts through a
   boxed [Int32.t] and allocates three words each time, which was the entire
   reason the handler path was not free. [Wire.uint32_be] reads straight to an
   unboxed int and masks. This pins that the substitution is exact, including
   above 2^31 where the signed read comes back negative. *)
let%expect_test "Wire.uint32_be agrees with Core across the unsigned range" =
  let buf = Bigstring.create 8 in
  let mismatches =
    List.filter
      [ 0
      ; 1
      ; 255
      ; 65535
      ; 65536
      ; 0x7FFF_FFFF
      ; 0x8000_0000
      ; 0xFFFF_FFFE
      ; 0xFFFF_FFFF
      ; 2_000_000_000
      ]
      ~f:(fun value ->
        Bigstring.set_uint32_be_exn buf ~pos:0 value;
        let core = Bigstring.get_uint32_be buf ~pos:0 in
        let ours = Wire.uint32_be buf ~pos:0 in
        core <> ours || ours <> value)
  in
  print_s [%sexp (mismatches : int list)];
  [%expect {| () |}]
;;
