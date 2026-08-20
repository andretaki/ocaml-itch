open! Core
open Itch

(* The two decode paths must agree. The zero-allocation functor path and the
   allocating Message.t path are separate code with separate dispatch, so this is
   where a mistake in the new one shows up -- a message routed to the wrong
   callback, an argument passed in the wrong position, a length check applied to
   the wrong type. *)

module R = Reader.Make (Checksum)

let both_paths corpus =
  let len = Bigstring.length corpus in
  let fast = Checksum.create () in
  let fast_consumed = R.consume fast corpus ~pos:0 ~len in
  let slow = Checksum.create () in
  let slow_consumed = Reader.iter corpus ~pos:0 ~len ~f:(Checksum.of_message slow) in
  printf
    "%s\n"
    (if
       String.equal (Checksum.to_string fast) (Checksum.to_string slow)
       && fast_consumed = slow_consumed
     then "paths agree"
     else "PATHS DISAGREE");
  printf "consumed %d bytes\n" fast_consumed;
  printf "%s\n" (Checksum.to_string fast)
;;

let%expect_test "zero-allocation path agrees with the Message.t path" =
  both_paths (Test_zero_alloc.corpus ~repeats:50);
  [%expect
    {|
    paths agree
    consumed 14450 bytes
    messages=450 adds=100 executes=100 cancels=50 deletes=50 replaces=50 directories=50 system_events=50 others=0 sum_shares=63250 sum_prices=200700000 xor_order_refs=0 xor_timestamps=0 max_locate=7
    |}]
;;

(* Generated messages reach field values the hand-written corpus does not --
   order references near the native int ceiling, full width prices -- which is
   exactly where a misplaced argument would hide. *)
let%expect_test "the two paths agree on generated messages too" =
  let messages = ref [] in
  Base_quickcheck.Test.with_sample_exn
    ~config:{ Base_quickcheck.Test.default_config with test_count = 2_000 }
    Test_roundtrip.message
    ~f:(fun sequence -> messages := Sequence.to_list sequence);
  let messages = !messages in
  let framed = List.map messages ~f:Encoder.to_framed_bigstring in
  let total = List.sum (module Int) framed ~f:Bigstring.length in
  let corpus = Bigstring.create total in
  let pos = ref 0 in
  List.iter framed ~f:(fun src ->
    let len = Bigstring.length src in
    Bigstring.blit ~src ~src_pos:0 ~dst:corpus ~dst_pos:!pos ~len;
    pos := !pos + len);
  both_paths corpus;
  [%expect
    {|
    paths agree
    consumed 62483 bytes
    messages=2000 adds=272 executes=453 cancels=234 deletes=226 replaces=282 directories=259 system_events=274 others=0 sum_shares=3202042149125 sum_prices=1647707133215 xor_order_refs=2844687346362742526 xor_timestamps=77315418369805 max_locate=65535
    |}]
;;
