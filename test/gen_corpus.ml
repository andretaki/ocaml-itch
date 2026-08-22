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
    the pre-market prefix and about 1.4% of a full session.

    Some of that tail is deliberately shorter than the 11-byte common header.
    The framing permits it -- the length prefix is free to say 3 -- even though
    no real ITCH message is that short, and a decoder that reads the header
    before it knows the message type reads past the end of such a message, and
    past the end of the mapping if it is the last one in the file. Both decoders
    here have had exactly that bug. Generating the case is what stops it coming
    back silently. *)

open! Core
open Itch

let unparsed_types = [| 'P'; 'Q'; 'I'; 'L'; 'H'; 'Y'; 'B'; 'N' |]

(* A message type the parser does not decode, framed like any other. The body is
   filled deterministically from the index so the bytes are reproducible.

   Lengths run from 1 upwards, so roughly a third of these are shorter than the
   11-byte header. See the note above: that is the interesting case, not an
   oversight. *)
let unparsed_message index =
  let length = 1 + (index mod 30) in
  let buf = Bigstring.create (2 + length) in
  Bigstring.set_uint16_be_exn buf ~pos:0 length;
  Bigstring.set buf 2 unparsed_types.(index % Array.length unparsed_types);
  for i = 1 to length - 1 do
    Bigstring.set buf (2 + i) (Char.of_int_exn ((index + (i * 31)) land 0xFF))
  done;
  buf
;;

(* A file whose final message declares a type the parser decodes but is framed
   too short to contain that type's fields. Every implementation is supposed to
   refuse this rather than read past the end of the message, and each of them has
   failed to at some point, so CI asserts it rather than trusting it. The
   preceding filler makes the file page aligned, which is what turns the read
   past the end into a read past the end of the mapping. *)
let write_malformed path =
  let page = 4096 in
  let tail_length = 12 in
  (* 'A' is Add Order, whose spec width is 36. *)
  let tail = Bigstring.create (2 + tail_length) in
  Bigstring.set_uint16_be_exn tail ~pos:0 tail_length;
  Bigstring.set tail 2 'A';
  for i = 1 to tail_length - 1 do
    Bigstring.set tail (2 + i) '\000'
  done;
  let filler_length = page - 2 - (2 + tail_length) in
  let filler = Bigstring.create (2 + filler_length) in
  Bigstring.set_uint16_be_exn filler ~pos:0 filler_length;
  Bigstring.set filler 2 'P';
  for i = 1 to filler_length - 1 do
    Bigstring.set filler (2 + i) (Char.of_int_exn (i * 31 land 0xFF))
  done;
  Out_channel.with_file path ~f:(fun out ->
    Out_channel.output_string out (Bigstring.to_string filler);
    Out_channel.output_string out (Bigstring.to_string tail));
  printf
    "wrote %d bytes to %s; final message declares type 'A' (spec width 36) framed at %d\n"
    page
    path
    tail_length
;;

let () =
  let argv = Stdlib.Sys.argv in
  match argv with
  | [| _; "-malformed"; path |] -> write_malformed path
  | _ ->
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
