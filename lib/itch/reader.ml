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

let check_length ~expected ~actual ~message_type =
  if expected <> actual
  then
    raise_s
      [%message
        "ITCH message length disagrees with the spec"
          (message_type : char)
          ~spec_length:(expected : int)
          ~framed_length:(actual : int)]
;;

(** A reader specialised to one handler.

    Applying the functor lets the compiler emit direct calls to [H]'s callbacks
    instead of jumping through a closure, and lets it inline them outright. The
    resulting loop decodes straight out of the mapped buffer and allocates
    nothing -- see [test/test_zero_alloc.ml], which asserts exactly that over a
    real file and fails the build if it ever stops being true. *)
module Make (H : Handler.S) = struct
  (* [check_length] comes first in every branch, and the header fields are read
     after it. Both halves of that matter, and the second was got wrong once
     already.

     Reading the header once up front, before the message type is known, is
     wrong because the framing permits a message shorter than the 11-byte header
     even though no real ITCH message is that short: its timestamp would be read
     from bytes past the end of the message, and past the end of the mapping
     entirely if it were the last one in the file.

     Moving those reads inside the branches is not sufficient on its own, which
     is what this comment used to claim. A message whose type byte says 'A' but
     whose framed length is 3 still reaches the 'A' branch, and if the header is
     read before [check_length] runs then the unchecked read happens anyway --
     the exception is raised a moment too late to prevent it. Ordering the check
     first closes that window; it costs nothing, because the check was already
     on the path.

     Doing it this way also buys the safety argument that lets the field
     accessors skip bounds checks: [consume] has already established that
     [message_length] bytes are present, and [check_length] has established that
     [message_length] is exactly the width the spec gives for this type, so
     every field offset below is in bounds by construction.

     [check_length] is called fully applied at each site rather than through a
     local helper: a local function capturing [len] would be a closure, and a
     closure is a heap allocation on every single message. *)
  let[@zero_alloc] dispatch state buf ~pos ~len =
    match Wire.message_type buf ~pos with
    | 'A' | 'F' ->
      let attributed = Char.equal (Wire.message_type buf ~pos) 'F' in
      check_length
        ~expected:
          (if attributed
           then Wire.Add_order.length_with_mpid
           else Wire.Add_order.length_without_mpid)
        ~actual:len
        ~message_type:'A';
      let stock_locate = Wire.stock_locate buf ~pos in
      let timestamp = Wire.timestamp buf ~pos in
      H.on_add_order
        state
        buf
        ~pos
        ~stock_locate
        ~timestamp
        ~order_ref:(Wire.Add_order.order_ref buf ~pos)
        ~side:(Wire.Add_order.side buf ~pos)
        ~shares:(Wire.Add_order.shares buf ~pos)
        ~price:(Wire.Add_order.price buf ~pos)
        ~attributed
    | 'E' ->
      check_length ~expected:Wire.Order_executed.length ~actual:len ~message_type:'E';
      let stock_locate = Wire.stock_locate buf ~pos in
      let timestamp = Wire.timestamp buf ~pos in
      H.on_order_executed
        state
        ~stock_locate
        ~timestamp
        ~order_ref:(Wire.Order_executed.order_ref buf ~pos)
        ~executed_shares:(Wire.Order_executed.executed_shares buf ~pos)
        ~match_number:(Wire.Order_executed.match_number buf ~pos)
    | 'C' ->
      check_length
        ~expected:Wire.Order_executed_with_price.length
        ~actual:len
        ~message_type:'C';
      let stock_locate = Wire.stock_locate buf ~pos in
      let timestamp = Wire.timestamp buf ~pos in
      H.on_order_executed_with_price
        state
        ~stock_locate
        ~timestamp
        ~order_ref:(Wire.Order_executed_with_price.order_ref buf ~pos)
        ~executed_shares:(Wire.Order_executed_with_price.executed_shares buf ~pos)
        ~match_number:(Wire.Order_executed_with_price.match_number buf ~pos)
        ~printable:(Char.equal (Wire.Order_executed_with_price.printable buf ~pos) 'Y')
        ~execution_price:(Wire.Order_executed_with_price.execution_price buf ~pos)
    | 'X' ->
      check_length ~expected:Wire.Order_cancel.length ~actual:len ~message_type:'X';
      let stock_locate = Wire.stock_locate buf ~pos in
      let timestamp = Wire.timestamp buf ~pos in
      H.on_order_cancel
        state
        ~stock_locate
        ~timestamp
        ~order_ref:(Wire.Order_cancel.order_ref buf ~pos)
        ~cancelled_shares:(Wire.Order_cancel.cancelled_shares buf ~pos)
    | 'D' ->
      check_length ~expected:Wire.Order_delete.length ~actual:len ~message_type:'D';
      let stock_locate = Wire.stock_locate buf ~pos in
      let timestamp = Wire.timestamp buf ~pos in
      H.on_order_delete
        state
        ~stock_locate
        ~timestamp
        ~order_ref:(Wire.Order_delete.order_ref buf ~pos)
    | 'U' ->
      check_length ~expected:Wire.Order_replace.length ~actual:len ~message_type:'U';
      let stock_locate = Wire.stock_locate buf ~pos in
      let timestamp = Wire.timestamp buf ~pos in
      H.on_order_replace
        state
        ~stock_locate
        ~timestamp
        ~original_order_ref:(Wire.Order_replace.original_order_ref buf ~pos)
        ~new_order_ref:(Wire.Order_replace.new_order_ref buf ~pos)
        ~shares:(Wire.Order_replace.shares buf ~pos)
        ~price:(Wire.Order_replace.price buf ~pos)
    | 'S' ->
      check_length ~expected:Wire.System_event.length ~actual:len ~message_type:'S';
      let stock_locate = Wire.stock_locate buf ~pos in
      let timestamp = Wire.timestamp buf ~pos in
      H.on_system_event
        state
        ~stock_locate
        ~tracking_number:(Wire.tracking_number buf ~pos)
        ~timestamp
        ~event_code:(Wire.System_event.event_code buf ~pos)
    | 'R' ->
      check_length ~expected:Wire.Stock_directory.length ~actual:len ~message_type:'R';
      let stock_locate = Wire.stock_locate buf ~pos in
      let timestamp = Wire.timestamp buf ~pos in
      H.on_stock_directory state buf ~pos ~stock_locate ~timestamp
    | message_type -> H.on_other state buf ~pos ~message_type ~length:len
  ;;

  (* Implementation detail of [consume]. It takes its state as arguments rather
     than capturing it: written the obvious way, as a local [let rec] closing
     over [state], [buf], [pos] and [limit], it costs a 64-byte closure on every
     call to [consume]. That is once per call rather than once per message, so
     the runtime allocation test -- which asserts only that allocation does not
     grow with message count -- cannot see it. OxCaml's [@zero_alloc] checker
     does, and reports it as "allocation of 64 bytes for closure". *)
  let rec consume_loop state buf ~pos ~limit p =
    if p + length_prefix_bytes > limit
    then p - pos
    else (
      let message_length = Bigstring.get_uint16_be buf ~pos:p in
      if message_length = 0
      then p - pos
      else if p + length_prefix_bytes + message_length > limit
      then p - pos
      else (
        dispatch state buf ~pos:(p + length_prefix_bytes) ~len:message_length;
        consume_loop state buf ~pos ~limit (p + length_prefix_bytes + message_length)))
  ;;

  (** Returns the number of bytes consumed, leaving any trailing partial message
      unconsumed exactly as {!fold} does. *)
  let[@zero_alloc] consume state buf ~pos ~len =
    consume_loop state buf ~pos ~limit:(pos + len) pos
  ;;
end
