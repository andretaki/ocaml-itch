open! Core
open Itch

(* The book is driven by messages, so the tests build messages rather than bytes:
   what is under test here is the arithmetic and the carry-forward rules, not the
   decoding, which test_parser covers. *)

let locate = Types.Locate.of_int 1
let stock = Types.Stock.of_padded_string "TEST    "
let timestamp = Types.Timestamp.of_ns_since_midnight 0
let order_ref = Types.Order_ref.of_uint64_exn
let price = Types.Price.of_raw_4
let shares = Types.Shares.of_int

let add ~ref_ ~side ~price:p ~shares:n : Message.t =
  Add_order
    { stock_locate = locate
    ; tracking_number = 0
    ; timestamp
    ; order_ref = order_ref ref_
    ; side
    ; shares = shares n
    ; stock
    ; price = price p
    ; attribution = None
    }
;;

let execute ~ref_ ~shares:n : Message.t =
  Order_executed
    { stock_locate = locate
    ; tracking_number = 0
    ; timestamp
    ; order_ref = order_ref ref_
    ; executed_shares = shares n
    ; match_number = Types.Match_number.of_uint64_exn 1
    }
;;

let cancel ~ref_ ~shares:n : Message.t =
  Order_cancel
    { stock_locate = locate
    ; tracking_number = 0
    ; timestamp
    ; order_ref = order_ref ref_
    ; cancelled_shares = shares n
    }
;;

let delete ~ref_ : Message.t =
  Order_delete
    { stock_locate = locate; tracking_number = 0; timestamp; order_ref = order_ref ref_ }
;;

let replace ~ref_ ~new_ref ~shares:n ~price:p : Message.t =
  Order_replace
    { stock_locate = locate
    ; tracking_number = 0
    ; timestamp
    ; original_order_ref = order_ref ref_
    ; new_order_ref = order_ref new_ref
    ; shares = shares n
    ; price = price p
    }
;;

let replay messages =
  let books = Order_book.create () in
  List.iter messages ~f:(Order_book.apply books);
  books
;;

let show books =
  (match Order_book.book_for books locate with
   | None -> print_endline "  (no book)"
   | Some book ->
     let line label = function
       | None -> printf "  %s (empty)\n" label
       | Some (p, n) -> printf "  %s %s x %d\n" label (Types.Price.to_string p) n
     in
     line "bid" (Order_book.Book.best_bid book);
     line "ask" (Order_book.Book.best_ask book));
  printf
    "  live orders %d, orphans %d\n"
    (Order_book.orders_live books)
    (Order_book.orphans books);
  match Order_book.check_invariants books with
  | Ok () -> printf "  invariants ok\n"
  | Error error -> print_s [%sexp (error : Error.t)]
;;

let%expect_test "add builds both sides" =
  show
    (replay
       [ add ~ref_:1 ~side:Buy ~price:100_0000 ~shares:100
       ; add ~ref_:2 ~side:Buy ~price:99_0000 ~shares:50
       ; add ~ref_:3 ~side:Sell ~price:101_0000 ~shares:75
       ]);
  [%expect
    {|
    bid 100.0000 x 100
    ask 101.0000 x 75
    live orders 3, orphans 0
    invariants ok
    |}]
;;

(* Spec 1.4.3: cancelled shares are "removed from the display size", so this is a
   delta. Reading it as the new size would leave 25 on the book, not 75. *)
let%expect_test "partial cancel subtracts, it does not assign" =
  show
    (replay
       [ add ~ref_:1 ~side:Buy ~price:100_0000 ~shares:100; cancel ~ref_:1 ~shares:25 ]);
  [%expect
    {|
    bid 100.0000 x 75
    ask (empty)
    live orders 1, orphans 0
    invariants ok
    |}]
;;

(* Spec 1.4.5: shares here is "the new total displayed quantity", the opposite
   convention from a cancel, and side and symbol carry forward from the original
   because the replace message does not repeat them. *)
let%expect_test "replace assigns a new total and carries the side forward" =
  show
    (replay
       [ add ~ref_:1 ~side:Buy ~price:100_0000 ~shares:100
       ; replace ~ref_:1 ~new_ref:2 ~shares:200 ~price:99_5000
       ]);
  [%expect
    {|
    bid 99.5000 x 200
    ask (empty)
    live orders 1, orphans 0
    invariants ok
    |}]
;;

let%expect_test "executing the full size removes the order and the level" =
  show
    (replay
       [ add ~ref_:1 ~side:Buy ~price:100_0000 ~shares:100
       ; add ~ref_:2 ~side:Buy ~price:99_0000 ~shares:50
       ; execute ~ref_:1 ~shares:100
       ]);
  [%expect
    {|
    bid 99.0000 x 50
    ask (empty)
    live orders 1, orphans 0
    invariants ok
    |}]
;;

let%expect_test "cumulative executions drain an order" =
  show
    (replay
       [ add ~ref_:1 ~side:Sell ~price:100_0000 ~shares:100
       ; execute ~ref_:1 ~shares:30
       ; execute ~ref_:1 ~shares:70
       ]);
  [%expect
    {|
    bid (empty)
    ask (empty)
    live orders 0, orphans 0
    invariants ok
    |}]
;;

let%expect_test "delete removes all remaining size" =
  show
    (replay
       [ add ~ref_:1 ~side:Sell ~price:100_0000 ~shares:100
       ; execute ~ref_:1 ~shares:30
       ; delete ~ref_:1
       ]);
  [%expect
    {|
    bid (empty)
    ask (empty)
    live orders 0, orphans 0
    invariants ok
    |}]
;;

(* Starting mid stream means modifies arrive for orders whose Add Order went past
   before the reader started. That is expected, so it is counted rather than
   raised on -- but it must not corrupt the book either. *)
let%expect_test "modify for an unknown order is counted, not applied" =
  show
    (replay
       [ add ~ref_:1 ~side:Buy ~price:100_0000 ~shares:100
       ; cancel ~ref_:999 ~shares:10
       ; execute ~ref_:998 ~shares:10
       ; delete ~ref_:997
       ; replace ~ref_:996 ~new_ref:995 ~shares:10 ~price:100_0000
       ]);
  [%expect
    {|
    bid 100.0000 x 100
    ask (empty)
    live orders 1, orphans 4
    invariants ok
    |}]
;;

let%expect_test "same price from two orders aggregates at one level" =
  show
    (replay
       [ add ~ref_:1 ~side:Buy ~price:100_0000 ~shares:100
       ; add ~ref_:2 ~side:Buy ~price:100_0000 ~shares:40
       ; cancel ~ref_:1 ~shares:100
       ]);
  [%expect
    {|
    bid 100.0000 x 40
    ask (empty)
    live orders 1, orphans 0
    invariants ok
    |}]
;;
