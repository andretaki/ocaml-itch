open! Core

(** Framing for the downloadable [.NASDAQ_ITCH50] files: each message is
    preceded by a 2-byte big-endian length. This framing is a property of the
    file distribution, not of the ITCH payload spec (which is carried over
    MoldUDP64 or SoupBinTCP on the wire), so it is verified against real data
    rather than quoted from the specification -- see the tests. *)

let length_prefix_bytes = 2

(** Fold over every complete message in [buf].

    Returns the accumulator and the number of bytes consumed. A trailing partial
    message is left unconsumed rather than raising, so a caller streaming a file
    in chunks can carry the remainder into the next buffer. *)
let fold buf ~pos ~len ~init ~f =
  let limit = pos + len in
  let rec loop acc p =
    if p + length_prefix_bytes > limit
    then acc, p - pos
    else (
      let message_length = Bigstring.get_uint16_be buf ~pos:p in
      (* Some files are padded with zeroes after the final message. *)
      if message_length = 0
      then acc, p - pos
      else if p + length_prefix_bytes + message_length > limit
      then acc, p - pos
      else (
        let message =
          Parser.parse_exn buf ~pos:(p + length_prefix_bytes) ~len:message_length
        in
        loop (f acc message) (p + length_prefix_bytes + message_length)))
  in
  loop init pos
;;

let iter buf ~pos ~len ~f =
  let (), consumed = fold buf ~pos ~len ~init:() ~f:(fun () message -> f message) in
  consumed
;;
