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
  let dispatch state buf ~pos ~len =
    let stock_locate = Wire.stock_locate buf ~pos in
    let timestamp = Wire.timestamp buf ~pos in
    (* [check_length] is called fully applied at each site rather than through a
       local helper: a local function capturing [len] would be a closure, and a
       closure is a heap allocation on every single message. Exactly what
       test_zero_alloc.ml exists to catch. *)
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
      H.on_order_cancel
        state
        ~stock_locate
        ~timestamp
        ~order_ref:(Wire.Order_cancel.order_ref buf ~pos)
        ~cancelled_shares:(Wire.Order_cancel.cancelled_shares buf ~pos)
    | 'D' ->
      check_length ~expected:Wire.Order_delete.length ~actual:len ~message_type:'D';
      H.on_order_delete
        state
        ~stock_locate
        ~timestamp
        ~order_ref:(Wire.Order_delete.order_ref buf ~pos)
    | 'U' ->
      check_length ~expected:Wire.Order_replace.length ~actual:len ~message_type:'U';
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
      H.on_system_event
        state
        ~stock_locate
        ~tracking_number:(Wire.tracking_number buf ~pos)
        ~timestamp
        ~event_code:(Wire.System_event.event_code buf ~pos)
    | 'R' ->
      check_length ~expected:Wire.Stock_directory.length ~actual:len ~message_type:'R';
      H.on_stock_directory state buf ~pos ~stock_locate ~timestamp
    | message_type -> H.on_other state buf ~pos ~message_type ~length:len
  ;;

  (** Returns the number of bytes consumed, leaving any trailing partial message
      unconsumed exactly as {!fold} does. *)
  let consume state buf ~pos ~len =
    let limit = pos + len in
    let rec loop p =
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
          loop (p + length_prefix_bytes + message_length)))
    in
    loop pos
  ;;
end
