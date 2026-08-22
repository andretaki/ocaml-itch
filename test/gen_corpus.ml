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
open Types

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

(** A coherent order flow, for cross-checking the book rather than the aggregate.

    The corpus above is drawn from the round-trip generators, which are
    deliberately unconstrained: order references are random, so a modify almost
    never names an order that exists, and the locate on a modify has nothing to
    do with the locate on the add that created it. That is exactly right for the
    aggregate -- every message folds independently and the point is field
    coverage -- and useless for the book, which is stateful. Measured on it:
    12,124 orphans out of 14,921 book messages, and 94 locates where the shares
    resting at price levels disagree with the shares recorded against live
    orders. Neither number indicates a bug; they say the input was never a
    plausible order flow.

    This generator keeps the same model the book keeps. Every modify names a
    live order and carries that order's locate, no execution or cancel takes
    more shares than the order has left, and a replace carries side and locate
    forward from the order it replaces. The invariant check then means
    something, and the resulting book can be diffed against an independent
    replay.

    Deterministic: the seed is fixed, so the file is byte-reproducible and a CI
    failure can be reproduced locally without shipping the corpus. *)
let write_book_corpus path count =
  let random = Random.State.make [| 20200130 |] in
  let int bound = Random.State.int random bound in
  let live = Int.Table.create () in
  (* Live references, for uniform random selection. A hash table alone would
     mean scanning to pick one; this array plus swap-removal of references that
     have since died keeps it amortised constant. *)
  let refs = Array.create ~len:(count + 1) 0 in
  let n_refs = ref 0 in
  let push_ref r =
    refs.(!n_refs) <- r;
    incr n_refs
  in
  let pick_live () =
    let rec go attempts =
      if !n_refs = 0 || attempts = 0
      then None
      else (
        let i = int !n_refs in
        let r = refs.(i) in
        if Hashtbl.mem live r
        then Some r
        else (
          refs.(i) <- refs.(!n_refs - 1);
          decr n_refs;
          go (attempts - 1)))
    in
    go 32
  in
  let next_ref = ref 1 in
  let fresh_ref () =
    let r = !next_ref in
    incr next_ref;
    r
  in
  let clock = ref 34_200_000_000_000 in
  let tick () =
    clock := !clock + 1 + int 1_000_000;
    Timestamp.of_ns_since_midnight !clock
  in
  (* Prices land on a coarse grid so that several orders share a level and the
     best bid and ask are a sum rather than a single order -- which is what
     makes the level bookkeeping worth checking at all. *)
  let some_price () = 1_000_000 + (int 200 * 1_000) in
  let some_shares () = 1 + int 1_000 in
  let messages = ref [] in
  let emit message = messages := message :: !messages in
  let add () =
    let order_ref = fresh_ref () in
    let locate = 1 + int 64 in
    let side : Side.t = if int 2 = 0 then Buy else Sell in
    let price = some_price () in
    let shares = some_shares () in
    Hashtbl.set live ~key:order_ref ~data:(locate, side, price, shares);
    push_ref order_ref;
    emit
      (Message.Add_order
         { stock_locate = Locate.of_int locate
         ; tracking_number = 0
         ; timestamp = tick ()
         ; order_ref = Order_ref.of_uint64_exn order_ref
         ; side
         ; shares = Shares.of_int shares
         ; stock = Stock.of_padded_string (sprintf "SYM%03d" locate)
         ; price = Price.of_raw_4 price
         ; attribution = (if int 100 < 5 then Some "MPID" else None)
         })
  in
  (* Executions and partial cancels differ in what the spec calls the field and
     in nothing else, so they share this. Taking at most what is left is the
     part that matters: an over-execution drives a price level negative, which
     both implementations then clamp to zero, and after that they are comparing
     two different books for reasons that have nothing to do with decoding. *)
  let take order_ref ~f =
    let locate, side, price, have = Hashtbl.find_exn live order_ref in
    let taken = 1 + int have in
    let remaining = have - taken in
    if remaining <= 0
    then Hashtbl.remove live order_ref
    else Hashtbl.set live ~key:order_ref ~data:(locate, side, price, remaining);
    f ~locate ~taken
  in
  let step () =
    match pick_live () with
    | None -> add ()
    | Some order_ref ->
      (match int 100 with
       | n when n < 45 -> add ()
       | n when n < 57 ->
         take order_ref ~f:(fun ~locate ~taken ->
           emit
             (Message.Order_executed
                { stock_locate = Locate.of_int locate
                ; tracking_number = 0
                ; timestamp = tick ()
                ; order_ref = Order_ref.of_uint64_exn order_ref
                ; executed_shares = Shares.of_int taken
                ; match_number = Match_number.of_uint64_exn (fresh_ref ())
                }))
       | n when n < 63 ->
         take order_ref ~f:(fun ~locate ~taken ->
           emit
             (Message.Order_executed_with_price
                { stock_locate = Locate.of_int locate
                ; tracking_number = 0
                ; timestamp = tick ()
                ; order_ref = Order_ref.of_uint64_exn order_ref
                ; executed_shares = Shares.of_int taken
                ; match_number = Match_number.of_uint64_exn (fresh_ref ())
                ; printable = int 2 = 0
                ; execution_price = Price.of_raw_4 (some_price ())
                }))
       | n when n < 75 ->
         take order_ref ~f:(fun ~locate ~taken ->
           emit
             (Message.Order_cancel
                { stock_locate = Locate.of_int locate
                ; tracking_number = 0
                ; timestamp = tick ()
                ; order_ref = Order_ref.of_uint64_exn order_ref
                ; cancelled_shares = Shares.of_int taken
                }))
       | n when n < 93 ->
         let locate, _, _, _ = Hashtbl.find_exn live order_ref in
         Hashtbl.remove live order_ref;
         emit
           (Message.Order_delete
              { stock_locate = Locate.of_int locate
              ; tracking_number = 0
              ; timestamp = tick ()
              ; order_ref = Order_ref.of_uint64_exn order_ref
              })
       | _ ->
         (* Spec 1.4.5: side and symbol are not repeated, so they come from the
            order being replaced, and [shares] is the new total rather than a
            delta. Both are easy to get backwards and produce a book that looks
            plausible. *)
         let locate, side, _, _ = Hashtbl.find_exn live order_ref in
         Hashtbl.remove live order_ref;
         let new_ref = fresh_ref () in
         let price = some_price () in
         let shares = some_shares () in
         Hashtbl.set live ~key:new_ref ~data:(locate, side, price, shares);
         push_ref new_ref;
         emit
           (Message.Order_replace
              { stock_locate = Locate.of_int locate
              ; tracking_number = 0
              ; timestamp = tick ()
              ; original_order_ref = Order_ref.of_uint64_exn order_ref
              ; new_order_ref = Order_ref.of_uint64_exn new_ref
              ; shares = Shares.of_int shares
              ; price = Price.of_raw_4 price
              }))
  in
  for _ = 1 to count do
    step ()
  done;
  let encoded = List.rev_map !messages ~f:Encoder.to_framed_bigstring in
  let total = List.sum (module Int) encoded ~f:Bigstring.length in
  let corpus = Bigstring.create total in
  let pos = ref 0 in
  List.iter encoded ~f:(fun src ->
    let len = Bigstring.length src in
    Bigstring.blit ~src ~src_pos:0 ~dst:corpus ~dst_pos:!pos ~len;
    pos := !pos + len);
  Out_channel.with_file path ~f:(fun out ->
    Out_channel.output_string out (Bigstring.to_string corpus));
  printf
    "wrote %d bytes, %d book messages, %d still live to %s\n"
    total
    count
    (Hashtbl.length live)
    path
;;

let () =
  let argv = Stdlib.Sys.argv in
  match argv with
  | [| _; "-malformed"; path |] -> write_malformed path
  | [| _; "-book"; path |] -> write_book_corpus path 20_000
  | [| _; "-book"; path; count |] -> write_book_corpus path (Int.of_string count)
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
