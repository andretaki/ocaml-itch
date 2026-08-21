(** Write a deterministic synthetic ITCH 5.0 corpus.

    This exists so that cross-implementation agreement can be checked in CI,
    where the real exchange files are not available -- they are gigabytes and
    gitignored. Agreement between two implementations does not need real data;
    it needs the same bytes on both sides.

    It reuses the round-trip generators rather than growing a second, weaker
    generator: those already cover the full field ranges, every enum case, and
    both the attributed and unattributed forms of Add Order.

    A tail of undecodable message types is appended deliberately. Nothing else
    in the suite exercises the [on_other] path, yet unparsed types are 92% of
    the pre-market prefix and about 1.4% of a full session. *)

open! Core
open Itch

let unparsed_types = [| 'P'; 'Q'; 'I'; 'L'; 'H'; 'Y'; 'B'; 'N' |]

(* A message type the parser does not decode, framed like any other. The body is
   filled deterministically from the index so the bytes are reproducible. *)
let unparsed_message index =
  let length = 11 + (index mod 20) in
  let buf = Bigstring.create (2 + length) in
  Bigstring.set_uint16_be_exn buf ~pos:0 length;
  Bigstring.set buf 2 unparsed_types.(index % Array.length unparsed_types);
  for i = 1 to length - 1 do
    Bigstring.set buf (2 + i) (Char.of_int_exn ((index + (i * 31)) land 0xFF))
  done;
  buf
;;

let () =
  let argv = Stdlib.Sys.argv in
  let path = argv.(1) in
  let count = if Array.length argv > 2 then Int.of_string argv.(2) else 20_000 in
  let messages = ref [] in
  Base_quickcheck.Test.with_sample_exn
    ~config:{ Base_quickcheck.Test.default_config with test_count = count }
    Itch_tests.Test_roundtrip.message
    ~f:(fun sequence -> messages := Sequence.to_list sequence);
  let encoded = List.map !messages ~f:Encoder.to_framed_bigstring in
  let encoded = encoded @ List.init (count / 10) ~f:unparsed_message in
  let total = List.sum (module Int) encoded ~f:Bigstring.length in
  let corpus = Bigstring.create total in
  let pos = ref 0 in
  List.iter encoded ~f:(fun src ->
    let len = Bigstring.length src in
    Bigstring.blit ~src ~src_pos:0 ~dst:corpus ~dst_pos:!pos ~len;
    pos := !pos + len);
  Out_channel.with_file path ~f:(fun out ->
    Out_channel.output_string out (Bigstring.to_string corpus));
  printf "wrote %d bytes, %d messages (%d undecodable) to %s\n"
    total (List.length encoded) (count / 10) path
;;
