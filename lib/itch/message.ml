open! Core
open Types

(** Y / N / <space> tri-state, which the spec uses for most boolean-ish fields.
    A space means "not available", which is genuinely different from "no" --
    e.g. IPO Flag is a space for every non-Nasdaq-listed issue. *)
module Yes_no = struct
  let of_char_exn = function
    | 'Y' -> Some true
    | 'N' -> Some false
    | ' ' -> None
    | c -> raise_s [%message "unknown ITCH Y/N/space field" (c : char)]
  ;;

  let to_char = function
    | Some true -> 'Y'
    | Some false -> 'N'
    | None -> ' '
  ;;
end

module Event_code = struct
  type t =
    | Start_of_messages
    | Start_of_system_hours
    | Start_of_market_hours
    | End_of_market_hours
    | End_of_system_hours
    | End_of_messages
  [@@deriving compare, equal, sexp_of, enumerate]

  let of_char_exn = function
    | 'O' -> Start_of_messages
    | 'S' -> Start_of_system_hours
    | 'Q' -> Start_of_market_hours
    | 'M' -> End_of_market_hours
    | 'E' -> End_of_system_hours
    | 'C' -> End_of_messages
    | c -> raise_s [%message "unknown ITCH system event code" (c : char)]
  ;;

  let to_char = function
    | Start_of_messages -> 'O'
    | Start_of_system_hours -> 'S'
    | Start_of_market_hours -> 'Q'
    | End_of_market_hours -> 'M'
    | End_of_system_hours -> 'E'
    | End_of_messages -> 'C'
  ;;
end

module Market_category = struct
  type t =
    | Nasdaq_global_select
    | Nasdaq_global
    | Nasdaq_capital
    | Nyse
    | Nyse_american
    | Nyse_arca
    | Bats_z
    | Investors_exchange
    | Not_available
  [@@deriving compare, equal, sexp_of, enumerate]

  let of_char_exn = function
    | 'Q' -> Nasdaq_global_select
    | 'G' -> Nasdaq_global
    | 'S' -> Nasdaq_capital
    | 'N' -> Nyse
    | 'A' -> Nyse_american
    | 'P' -> Nyse_arca
    | 'Z' -> Bats_z
    | 'V' -> Investors_exchange
    | ' ' -> Not_available
    | c -> raise_s [%message "unknown ITCH market category" (c : char)]
  ;;

  let to_char = function
    | Nasdaq_global_select -> 'Q'
    | Nasdaq_global -> 'G'
    | Nasdaq_capital -> 'S'
    | Nyse -> 'N'
    | Nyse_american -> 'A'
    | Nyse_arca -> 'P'
    | Bats_z -> 'Z'
    | Investors_exchange -> 'V'
    | Not_available -> ' '
  ;;
end

module Financial_status = struct
  type t =
    | Deficient
    | Delinquent
    | Bankrupt
    | Suspended
    | Deficient_and_bankrupt
    | Deficient_and_delinquent
    | Delinquent_and_bankrupt
    | Deficient_delinquent_and_bankrupt
    | Creations_and_redemptions_suspended
    | Normal
    | Not_available
  [@@deriving compare, equal, sexp_of, enumerate]

  let of_char_exn = function
    | 'D' -> Deficient
    | 'E' -> Delinquent
    | 'Q' -> Bankrupt
    | 'S' -> Suspended
    | 'G' -> Deficient_and_bankrupt
    | 'H' -> Deficient_and_delinquent
    | 'J' -> Delinquent_and_bankrupt
    | 'K' -> Deficient_delinquent_and_bankrupt
    | 'C' -> Creations_and_redemptions_suspended
    | 'N' -> Normal
    | ' ' -> Not_available
    | c -> raise_s [%message "unknown ITCH financial status indicator" (c : char)]
  ;;

  let to_char = function
    | Deficient -> 'D'
    | Delinquent -> 'E'
    | Bankrupt -> 'Q'
    | Suspended -> 'S'
    | Deficient_and_bankrupt -> 'G'
    | Deficient_and_delinquent -> 'H'
    | Delinquent_and_bankrupt -> 'J'
    | Deficient_delinquent_and_bankrupt -> 'K'
    | Creations_and_redemptions_suspended -> 'C'
    | Normal -> 'N'
    | Not_available -> ' '
  ;;
end

module Authenticity = struct
  type t =
    | Live
    | Test
  [@@deriving compare, equal, sexp_of, enumerate]

  let of_char_exn = function
    | 'P' -> Live
    | 'T' -> Test
    | c -> raise_s [%message "unknown ITCH authenticity" (c : char)]
  ;;

  let to_char = function
    | Live -> 'P'
    | Test -> 'T'
  ;;
end

module Luld_reference_price_tier = struct
  type t =
    | Tier_1
    | Tier_2
    | Not_available
  [@@deriving compare, equal, sexp_of, enumerate]

  let of_char_exn = function
    | '1' -> Tier_1
    | '2' -> Tier_2
    | ' ' -> Not_available
    | c -> raise_s [%message "unknown ITCH LULD reference price tier" (c : char)]
  ;;

  let to_char = function
    | Tier_1 -> '1'
    | Tier_2 -> '2'
    | Not_available -> ' '
  ;;
end

(* Every message carries the same four-field header; keeping it flat in each
   record mirrors the wire layout and keeps parsing branch-free. *)

module System_event = struct
  type t =
    { stock_locate : Locate.t
    ; tracking_number : int
    ; timestamp : Timestamp.t
    ; event_code : Event_code.t
    }
  [@@deriving compare, equal, sexp_of]

  let length = 12
end

module Stock_directory = struct
  type t =
    { stock_locate : Locate.t
    ; tracking_number : int
    ; timestamp : Timestamp.t
    ; stock : Stock.t
    ; market_category : Market_category.t
    ; financial_status : Financial_status.t
    ; round_lot_size : int
    ; round_lots_only : bool
    ; issue_classification : char
    ; issue_sub_type : string
    ; authenticity : Authenticity.t
    ; short_sale_threshold_indicator : bool option
    ; ipo_flag : bool option
    ; luld_reference_price_tier : Luld_reference_price_tier.t
    ; etp_flag : bool option
    ; etp_leverage_factor : int
    ; inverse_indicator : bool
    }
  [@@deriving compare, equal, sexp_of]

  let length = 39
end

module Add_order = struct
  (* Spec 1.3.1 ("A") and 1.3.2 ("F") are the same message but for a trailing
     4-byte MPID, so they collapse into one variant with an optional
     attribution. The wire message type is recoverable: [Some _] is "F". *)
  type t =
    { stock_locate : Locate.t
    ; tracking_number : int
    ; timestamp : Timestamp.t
    ; order_ref : Order_ref.t
    ; side : Side.t
    ; shares : Shares.t
    ; stock : Stock.t
    ; price : Price.t
    ; attribution : string option
    }
  [@@deriving compare, equal, sexp_of]

  let length_without_mpid = 36
  let length_with_mpid = 40
end

(* Spec 1.4: "Modify Order messages always include the Order Reference Number of
   the Add Order to which the update applies. [...] Nasdaq may send multiple
   Modify Order messages for the same order reference number and the effects are
   cumulative. When the number of display shares for an order reaches zero, the
   order is dead and should be removed from the book." *)

module Order_executed = struct
  type t =
    { stock_locate : Locate.t
    ; tracking_number : int
    ; timestamp : Timestamp.t
    ; order_ref : Order_ref.t
    ; executed_shares : Shares.t
    ; match_number : Match_number.t
    }
  [@@deriving compare, equal, sexp_of]

  let length = 31
end

(* Kept distinct from [Order_executed] rather than folded together the way "A"
   and "F" are: the trailing fields here are not decoration but semantics. The
   execution happened away from the order's display price, and a non-printable
   execution must be excluded from time-and-sales and volume or it is counted
   twice when the later bulk print arrives. *)
module Order_executed_with_price = struct
  type t =
    { stock_locate : Locate.t
    ; tracking_number : int
    ; timestamp : Timestamp.t
    ; order_ref : Order_ref.t
    ; executed_shares : Shares.t
    ; match_number : Match_number.t
    ; printable : bool
    ; execution_price : Price.t
    }
  [@@deriving compare, equal, sexp_of]

  let length = 36
end

(** A partial cancel: [cancelled_shares] comes {i off} the order's display size,
    it is not the new size. *)
module Order_cancel = struct
  type t =
    { stock_locate : Locate.t
    ; tracking_number : int
    ; timestamp : Timestamp.t
    ; order_ref : Order_ref.t
    ; cancelled_shares : Shares.t
    }
  [@@deriving compare, equal, sexp_of]

  let length = 23
end

module Order_delete = struct
  type t =
    { stock_locate : Locate.t
    ; tracking_number : int
    ; timestamp : Timestamp.t
    ; order_ref : Order_ref.t
    }
  [@@deriving compare, equal, sexp_of]

  let length = 19
end

(** Spec 1.4.5: "Since the side, stock symbol and attribution (if any) cannot be
    changed by an Order Replace event, these fields are not included in the
    message. Firms should retain the side, stock symbol and MPID from the
    original Add Order message."

    So this message alone is not enough to place the replacement order: the book
    has to carry side and symbol forward from [original_order_ref]. Unlike a
    cancel, [shares] here {i is} the new total, not a delta. *)
module Order_replace = struct
  type t =
    { stock_locate : Locate.t
    ; tracking_number : int
    ; timestamp : Timestamp.t
    ; original_order_ref : Order_ref.t
    ; new_order_ref : Order_ref.t
    ; shares : Shares.t
    ; price : Price.t
    }
  [@@deriving compare, equal, sexp_of]

  let length = 35
end

(** A message whose type is recognised by the framing but not yet decoded.

    ITCH 5.0 has around twenty message types; this parser currently decodes
    three. The remainder are surfaced rather than swallowed, so a fold over a
    real trading day completes and reports honestly what it skipped. *)
module Unparsed = struct
  type t =
    { message_type : char
    ; length : int
    }
  [@@deriving compare, equal, sexp_of]
end

type t =
  | System_event of System_event.t
  | Stock_directory of Stock_directory.t
  | Add_order of Add_order.t
  | Order_executed of Order_executed.t
  | Order_executed_with_price of Order_executed_with_price.t
  | Order_cancel of Order_cancel.t
  | Order_delete of Order_delete.t
  | Order_replace of Order_replace.t
  | Unparsed of Unparsed.t
[@@deriving compare, equal, sexp_of, variants]

let message_type = function
  | System_event _ -> 'S'
  | Stock_directory _ -> 'R'
  | Add_order { attribution = None; _ } -> 'A'
  | Add_order { attribution = Some _; _ } -> 'F'
  | Order_executed _ -> 'E'
  | Order_executed_with_price _ -> 'C'
  | Order_cancel _ -> 'X'
  | Order_delete _ -> 'D'
  | Order_replace _ -> 'U'
  | Unparsed { message_type; _ } -> message_type
;;

(** The timestamp every message carries in its header. *)
let timestamp = function
  | System_event m -> Some m.timestamp
  | Stock_directory m -> Some m.timestamp
  | Add_order m -> Some m.timestamp
  | Order_executed m -> Some m.timestamp
  | Order_executed_with_price m -> Some m.timestamp
  | Order_cancel m -> Some m.timestamp
  | Order_delete m -> Some m.timestamp
  | Order_replace m -> Some m.timestamp
  | Unparsed _ -> None
;;
