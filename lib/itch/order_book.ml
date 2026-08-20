open! Core
open Types

(** Limit order book reconstruction from the ITCH message stream.

    Two things make this less mechanical than it looks.

    First, the modify messages ("E", "C", "X", "D", "U") do not carry the side,
    the symbol or the price of the order they modify -- only its reference
    number. The book therefore has to remember every live order, and a modify
    for an unknown reference cannot be applied at all. That is expected rather
    than exceptional when starting mid-stream: the Add Order it refers to went
    past before the reader started. Those are counted, not raised on, so that
    the count itself becomes a diagnostic.

    Second, the arithmetic differs between messages that look alike. A cancel
    carries shares to {i subtract}; a replace carries the new {i total}. Getting
    that backwards produces a book that looks plausible and is wrong, which is
    why the invariant check below is worth running over real data. *)

module Price_map = Map.Make_plain (Price)

module Order = struct
  type t =
    { locate : Locate.t
    ; side : Side.t
    ; price : Price.t
    ; shares : int
    }
  [@@deriving sexp_of]
end

module Book = struct
  type t =
    { mutable bids : int Price_map.t
    ; mutable asks : int Price_map.t
    }

  let create () = { bids = Price_map.empty; asks = Price_map.empty }

  let side_map t : Side.t -> int Price_map.t = function
    | Buy -> t.bids
    | Sell -> t.asks
  ;;

  let set_side_map t (side : Side.t) map =
    match side with
    | Buy -> t.bids <- map
    | Sell -> t.asks <- map
  ;;

  (** Shares are added and subtracted at a level; a level that reaches zero is
      removed rather than left as an empty rung, so [Map.max_elt bids] is always
      the real best bid. *)
  let adjust t side price ~delta =
    let map = side_map t side in
    let updated =
      Map.update map price ~f:(function
        | None -> delta
        | Some shares -> shares + delta)
    in
    let updated =
      match Map.find updated price with
      | Some shares when shares <= 0 -> Map.remove updated price
      | _ -> updated
    in
    set_side_map t side updated
  ;;

  let best_bid t = Map.max_elt t.bids
  let best_ask t = Map.min_elt t.asks

  let depth t (side : Side.t) ~levels =
    let map = side_map t side in
    let ordered =
      match side with
      | Buy -> List.rev (Map.to_alist map)
      | Sell -> Map.to_alist map
    in
    List.take ordered levels
  ;;
end

type t =
  { orders : Order.t Int.Table.t
  ; books : Book.t Int.Table.t
  ; mutable applied : int
  ; mutable orphans : int
  }

let create () =
  { orders = Int.Table.create (); books = Int.Table.create (); applied = 0; orphans = 0 }
;;

let book t locate =
  Hashtbl.find_or_add t.books (Locate.to_int locate) ~default:Book.create
;;

let orders_live t = Hashtbl.length t.orders
let applied t = t.applied
let orphans t = t.orphans
let book_for t locate = Hashtbl.find t.books (Locate.to_int locate)

(** Every locate this book has seen, in ascending order, so that a dump is
    stable and diffable against another implementation. *)
let locates t =
  Hashtbl.keys t.books |> List.sort ~compare:Int.compare |> List.map ~f:Locate.of_int
;;

(** Reduce an order's displayed size by [shares], removing it once nothing is
    left. Used by both the execution and the partial cancel paths, which differ
    only in what the spec calls the field. *)
let reduce t order_ref ~shares =
  let key = Order_ref.to_int order_ref in
  match Hashtbl.find t.orders key with
  | None -> t.orphans <- t.orphans + 1
  | Some order ->
    let remaining = order.shares - shares in
    Book.adjust (book t order.locate) order.side order.price ~delta:(-shares);
    if remaining <= 0
    then Hashtbl.remove t.orders key
    else Hashtbl.set t.orders ~key ~data:{ order with shares = remaining }
;;

let remove t order_ref =
  let key = Order_ref.to_int order_ref in
  match Hashtbl.find t.orders key with
  | None ->
    t.orphans <- t.orphans + 1;
    None
  | Some order ->
    Book.adjust (book t order.locate) order.side order.price ~delta:(-order.shares);
    Hashtbl.remove t.orders key;
    Some order
;;

let add t ~order_ref ~locate ~side ~price ~shares =
  Hashtbl.set
    t.orders
    ~key:(Order_ref.to_int order_ref)
    ~data:{ Order.locate; side; price; shares };
  Book.adjust (book t locate) side price ~delta:shares
;;

let apply t (message : Message.t) =
  t.applied <- t.applied + 1;
  match message with
  | Add_order m ->
    add
      t
      ~order_ref:m.order_ref
      ~locate:m.stock_locate
      ~side:m.side
      ~price:m.price
      ~shares:(Shares.to_int m.shares)
  | Order_executed m -> reduce t m.order_ref ~shares:(Shares.to_int m.executed_shares)
  | Order_executed_with_price m ->
    reduce t m.order_ref ~shares:(Shares.to_int m.executed_shares)
  | Order_cancel m -> reduce t m.order_ref ~shares:(Shares.to_int m.cancelled_shares)
  | Order_delete m -> (ignore : Order.t option -> unit) (remove t m.order_ref)
  | Order_replace m ->
    (* Spec 1.4.5: side, symbol and attribution are not repeated, so they are
       carried forward from the order being replaced. [shares] here is the new
       total, not a delta. *)
    (match remove t m.original_order_ref with
     | None -> ()
     | Some original ->
       add
         t
         ~order_ref:m.new_order_ref
         ~locate:original.locate
         ~side:original.side
         ~price:m.price
         ~shares:(Shares.to_int m.shares))
  | System_event _ | Stock_directory _ | Unparsed _ -> t.applied <- t.applied - 1
;;

(** Two independent statements of the same quantity must agree: the shares
    resting at every price level, and the shares recorded against every live
    order. They are maintained by different code paths, so a disagreement means
    one of the adjustments above is wrong. *)
let check_invariants t =
  let from_levels = Int.Table.create () in
  Hashtbl.iteri t.books ~f:(fun ~key:locate ~data:book ->
    let total =
      List.sum (module Int) (Map.data book.bids @ Map.data book.asks) ~f:Fn.id
    in
    if total <> 0 then Hashtbl.set from_levels ~key:locate ~data:total);
  let from_orders = Int.Table.create () in
  Hashtbl.iter t.orders ~f:(fun order ->
    Hashtbl.update from_orders (Locate.to_int order.locate) ~f:(function
      | None -> order.shares
      | Some total -> total + order.shares));
  let negative_levels =
    Hashtbl.count t.books ~f:(fun book ->
      Map.exists book.bids ~f:(fun shares -> shares <= 0)
      || Map.exists book.asks ~f:(fun shares -> shares <= 0))
  in
  let mismatched =
    Hashtbl.fold from_orders ~init:[] ~f:(fun ~key:locate ~data:expected acc ->
      match Hashtbl.find from_levels locate with
      | Some actual when actual = expected -> acc
      | other -> (locate, expected, other) :: acc)
  in
  if negative_levels > 0 || not (List.is_empty mismatched)
  then
    Or_error.error_s
      [%message
        "order book invariants violated"
          (negative_levels : int)
          ~mismatched_locates:(List.length mismatched : int)
          ~first_few:(List.take mismatched 5 : (int * int * int option) list)]
  else Ok ()
;;
