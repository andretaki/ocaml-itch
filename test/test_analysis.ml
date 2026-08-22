open! Core
open Itch

(* Lifetimes are computed from timestamps the test chooses, so every number
   below is known in advance and asserted exactly. That matters more than usual
   here: on real pre-market data the "cancelled" bucket comes back empty,
   because NASDAQ sends a full withdrawal as "D" and uses "X" only to shrink an
   order. An empty bucket looks identical whether the convention is real or the
   code path is broken, so the path is exercised synthetically. *)

let locate n = Types.Locate.of_int n
let stock = Types.Stock.of_padded_string "TEST"
let ts n = Types.Timestamp.of_ns_since_midnight n
let order_ref n = Types.Order_ref.of_uint64_exn n
let shares n = Types.Shares.of_int n
let price n = Types.Price.of_raw_4 n
let match_number n = Types.Match_number.of_uint64_exn n

let add ~at ~ref_ ~shares:s : Message.t =
  Add_order
    { stock_locate = locate 1
    ; tracking_number = 0
    ; timestamp = ts at
    ; order_ref = order_ref ref_
    ; side = Buy
    ; shares = shares s
    ; stock
    ; price = price 1_000_000
    ; attribution = None
    }
;;

let execute ~at ~ref_ ~shares:s : Message.t =
  Order_executed
    { stock_locate = locate 1
    ; tracking_number = 0
    ; timestamp = ts at
    ; order_ref = order_ref ref_
    ; executed_shares = shares s
    ; match_number = match_number 1
    }
;;

let cancel ~at ~ref_ ~shares:s : Message.t =
  Order_cancel
    { stock_locate = locate 1
    ; tracking_number = 0
    ; timestamp = ts at
    ; order_ref = order_ref ref_
    ; cancelled_shares = shares s
    }
;;

let delete ~at ~ref_ : Message.t =
  Order_delete
    { stock_locate = locate 1; tracking_number = 0; timestamp = ts at; order_ref = order_ref ref_ }
;;

let replace ~at ~from ~to_ ~shares:s : Message.t =
  Order_replace
    { stock_locate = locate 1
    ; tracking_number = 0
    ; timestamp = ts at
    ; original_order_ref = order_ref from
    ; new_order_ref = order_ref to_
    ; shares = shares s
    ; price = price 1_010_000
    }
;;

let concat encoded =
  let total = List.sum (module Int) encoded ~f:Bigstring.length in
  let buf = Bigstring.create total in
  let pos = ref 0 in
  List.iter encoded ~f:(fun src ->
    let len = Bigstring.length src in
    Bigstring.blit ~src ~src_pos:0 ~dst:buf ~dst_pos:!pos ~len;
    pos := !pos + len);
  buf
;;

let corpus messages = concat (List.map messages ~f:Encoder.to_framed_bigstring)

(* An undecoded message cannot come from the encoder: [Message.Unparsed] keeps
   only the type and the length, having never decoded the rest, and the encoder
   refuses it for exactly that reason. So the bytes are laid out by hand. That
   is the stronger test anyway -- it exercises the real 11-byte header that
   [Analysis.on_other] reads the timestamp out of, rather than a value the
   library round-tripped through itself. *)
let framed_other ~message_type ~length ~timestamp =
  assert (length >= 11);
  let buf = Bigstring.create (2 + length) in
  Bigstring.memset buf ~pos:0 ~len:(Bigstring.length buf) '\000';
  Bigstring.set_uint16_be_exn buf ~pos:0 length;
  Bigstring.set buf 2 message_type;
  Bigstring.set_uint16_be_exn buf ~pos:3 1 (* stock_locate *);
  Bigstring.set_uint16_be_exn buf ~pos:5 0 (* tracking number *);
  (* 48-bit big-endian timestamp at message offset 5, so buffer offset 7. *)
  Bigstring.set_uint16_be_exn buf ~pos:7 (timestamp lsr 32);
  Bigstring.set_uint32_be_exn buf ~pos:9 (timestamp land 0xFFFF_FFFF);
  buf
;;

(* A whole day of millisecond counters is 691 MB, and zeroing one per test
   dominated the suite's run time -- nine seconds, seven of them in the kernel.
   These corpora span milliseconds, not hours. *)
let test_milliseconds = 100_000

module R = Reader.Make (Analysis)

let run messages =
  let state = Analysis.create ~capacity_log2:10 ~milliseconds:test_milliseconds () in
  let buf = corpus messages in
  ignore (R.consume state buf ~pos:0 ~len:(Bigstring.length buf) : int);
  state
;;

(* Every recorded lifetime, in order, so an assertion can name the exact value
   rather than a bucket. The histogram is lossy by design, so this reads the
   bucket bounds and requires the known answer to sit inside them. *)
let show_lifetimes state cause =
  let h = Analysis.lifetimes state cause in
  printf "%-10s count %d" (Analysis.Cause.to_string cause) (Histogram.count h);
  if Histogram.count h > 0
  then printf "  min %d  max %d" (Histogram.min_value h) (Histogram.max_value h);
  printf "\n"
;;

let%expect_test "each of the four terminal causes is counted, with exact lifetimes" =
  let state =
    run
      [ (* executed out: born 1_000, dies 3_000 *)
        add ~at:1_000 ~ref_:1 ~shares:100
      ; execute ~at:3_000 ~ref_:1 ~shares:100
      ; (* cancelled out via X taking it to exactly zero: born 2_000, dies 9_000 *)
        add ~at:2_000 ~ref_:2 ~shares:50
      ; cancel ~at:9_000 ~ref_:2 ~shares:50
      ; (* deleted: born 4_000, dies 10_000 *)
        add ~at:4_000 ~ref_:3 ~shares:70
      ; delete ~at:10_000 ~ref_:3
      ; (* replaced: ref 4 born 5_000 dies 6_000; ref 5 born 6_000 dies 20_000 *)
        add ~at:5_000 ~ref_:4 ~shares:80
      ; replace ~at:6_000 ~from:4 ~to_:5 ~shares:90
      ; delete ~at:20_000 ~ref_:5
      ]
  in
  List.iter Analysis.Cause.all ~f:(show_lifetimes state);
  printf "orphans %d, still resting %d\n" (Analysis.orphan_modifies state) (Analysis.live_orders state);
  [%expect
    {|
    executed   count 1  min 2000  max 2000
    cancelled  count 1  min 7000  max 7000
    deleted    count 2  min 6000  max 14000
    replaced   count 1  min 1000  max 1000
    orphans 0, still resting 0
    |}]
;;

let%expect_test "a partial execution or partial cancel is not a death" =
  let state =
    run
      [ add ~at:1_000 ~ref_:1 ~shares:100
      ; execute ~at:2_000 ~ref_:1 ~shares:40 (* 60 left, still resting *)
      ; cancel ~at:3_000 ~ref_:1 ~shares:10 (* 50 left, still resting *)
      ; execute ~at:9_000 ~ref_:1 ~shares:50 (* now empty: executed, lived 8_000 *)
      ]
  in
  List.iter Analysis.Cause.all ~f:(show_lifetimes state);
  printf "still resting %d\n" (Analysis.live_orders state);
  [%expect
    {|
    executed   count 1  min 8000  max 8000
    cancelled  count 0
    deleted    count 0
    replaced   count 0
    still resting 0
    |}]
;;

let%expect_test "an over-large execution empties the order rather than going negative" =
  (* The wire should never carry this, but "should never" is not a bound. A
     subtraction past zero must still terminate the order exactly once. *)
  let state =
    run [ add ~at:1_000 ~ref_:1 ~shares:100; execute ~at:5_000 ~ref_:1 ~shares:500 ]
  in
  List.iter Analysis.Cause.all ~f:(show_lifetimes state);
  printf "still resting %d\n" (Analysis.live_orders state);
  [%expect
    {|
    executed   count 1  min 4000  max 4000
    cancelled  count 0
    deleted    count 0
    replaced   count 0
    still resting 0
    |}]
;;

let%expect_test "a modify for an unknown reference is counted, not raised on" =
  let state = run [ delete ~at:1_000 ~ref_:99; execute ~at:2_000 ~ref_:98 ~shares:10 ] in
  printf "orphans %d\n" (Analysis.orphan_modifies state);
  [%expect {| orphans 2 |}]
;;

let%expect_test "message rate counts undecoded types too" =
  (* Message rate is a property of the whole feed, not of the part this parser
     understands, so a type that reaches [on_other] must still land in its
     millisecond. Three messages in millisecond 1, one in millisecond 5. *)
  let state = Analysis.create ~capacity_log2:10 ~milliseconds:test_milliseconds () in
  let buf =
    concat
      [ Encoder.to_framed_bigstring (add ~at:1_000_000 ~ref_:1 ~shares:100)
      ; framed_other ~message_type:'I' ~length:50 ~timestamp:1_500_000
      ; Encoder.to_framed_bigstring (delete ~at:1_900_000 ~ref_:1)
      ; Encoder.to_framed_bigstring (add ~at:5_000_000 ~ref_:2 ~shares:100)
      ]
  in
  ignore (R.consume state buf ~pos:0 ~len:(Bigstring.length buf) : int);
  printf
    "messages %d, decoded %d\n"
    (Analysis.messages state)
    (Analysis.decoded_messages state);
  Analysis.iter_milliseconds state ~f:(fun ~ms ~count ->
    if count > 0 then printf "ms %d: %d\n" ms count);
  [%expect
    {|
    messages 4, decoded 3
    ms 1: 3
    ms 5: 1
    |}]
;;

let%expect_test "a message too short to hold a header is counted but not timestamped" =
  (* [Reader] guarantees the framed bytes are present; it does not guarantee
     they reach the timestamp field. Reading one anyway would take the value
     from whatever follows in the mapping. *)
  let state = Analysis.create ~capacity_log2:10 ~milliseconds:test_milliseconds () in
  let buf =
    concat
      [ Encoder.to_framed_bigstring (add ~at:1_000_000 ~ref_:1 ~shares:100)
      ; framed_other ~message_type:'Z' ~length:11 ~timestamp:2_000_000
      ; (let short = Bigstring.create 5 in
         Bigstring.set_uint16_be_exn short ~pos:0 3;
         Bigstring.set short 2 'Z';
         Bigstring.set short 3 '\000';
         Bigstring.set short 4 '\000';
         short)
      ]
  in
  ignore (R.consume state buf ~pos:0 ~len:(Bigstring.length buf) : int);
  printf
    "messages %d, timestamped %d\n"
    (Analysis.messages state)
    (Analysis.timestamped_messages state);
  [%expect {| messages 3, timestamped 2 |}]
;;

(* The allocation gate for this handler.

   [test_zero_alloc.ml] drives [Checksum], not this module, so it says nothing
   about the analysis path -- and the analysis path is where the interesting
   allocation hid. OxCaml's checker found a 16-byte float box on every
   terminated order here, from indexing a [Histogram.t array] whose element type
   is abstract at the point of use; vanilla OCaml compiled it without comment.
   This is the vanilla-side gate for the same property, so the two switches both
   have something to say about it.

   Every order the round creates is also removed by the end of it, so the state
   can be driven over and over: a corpus that leaves orders resting would insert
   a duplicate reference on the second pass and raise. *)
let self_clearing_round =
  [ add ~at:1_000 ~ref_:1 ~shares:100
  ; add ~at:1_100 ~ref_:2 ~shares:250
  ; execute ~at:2_000 ~ref_:1 ~shares:100 (* executed out *)
  ; cancel ~at:2_100 ~ref_:2 ~shares:250 (* cancelled out *)
  ; add ~at:3_000 ~ref_:4 ~shares:80
  ; replace ~at:4_000 ~from:4 ~to_:5 ~shares:90 (* replaced out *)
  ; delete ~at:5_000 ~ref_:5 (* deleted out *)
  ]
;;

let repeated_corpus ~rounds =
  concat (List.concat (List.init rounds ~f:(fun _ ->
    List.map self_clearing_round ~f:Encoder.to_framed_bigstring)))
;;

let words_for ~passes ~corpus ~state =
  let len = Bigstring.length corpus in
  let before = Gc.minor_words () in
  for _ = 1 to passes do
    ignore (R.consume state corpus ~pos:0 ~len : int)
  done;
  Gc.minor_words () - before
;;

let%expect_test "the analysis handler allocates nothing per message" =
  let small = repeated_corpus ~rounds:500 in
  let large = repeated_corpus ~rounds:2_000 in
  let state = Analysis.create ~capacity_log2:12 ~milliseconds:test_milliseconds () in
  (* Warm both, so nothing lazy is paid for inside a measured region. *)
  ignore (words_for ~passes:1 ~corpus:small ~state : int);
  ignore (words_for ~passes:1 ~corpus:large ~state : int);
  let per_pass corpus =
    let one = words_for ~passes:1 ~corpus ~state in
    let nine = words_for ~passes:9 ~corpus ~state in
    (nine - one) / 8
  in
  let small_words = per_pass small in
  let large_words = per_pass large in
  printf "words per pass over  3,500 messages: %d\n" small_words;
  printf "words per pass over 14,000 messages: %d\n" large_words;
  printf "words attributable to the extra 10,500: %d\n" (large_words - small_words);
  (* And the handler really did do the work, rather than skipping it: the state
     accumulated terminations throughout. *)
  printf
    "terminations recorded: %b\n"
    (Histogram.count (Analysis.all_lifetimes state) > 0);
  [%expect {|
    words per pass over  3,500 messages: 0
    words per pass over 14,000 messages: 0
    words attributable to the extra 10,500: 0
    terminations recorded: true
    |}]
;;
