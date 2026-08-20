(** Scalar domain types for Nasdaq TotalView-ITCH 5.0.

    Every scalar the wire format carries gets its own abstract type. The protocol
    is a flat sea of big-endian integers -- prices, share counts, order reference
    numbers and locate codes are all just [int] on the wire -- and nothing but a
    type system stops you from adding a price to a share count. So each one is
    abstract here, and the only way in is through an explicit constructor.

    Sizes are stated in terms of the spec's field widths, which matters because
    two of them do not fit OCaml's native [int] comfortably:

    - Timestamps are 48-bit (6 bytes), which fits fine in a 63-bit [int].
    - Order reference and match numbers are 64-bit unsigned, which does {i not}
      fit. See {!Order_ref.of_uint64_exn}. *)

open! Core

(** Nanoseconds since midnight (Eastern), as carried in every message header. *)
module Timestamp : sig
  type t [@@deriving compare, equal, hash, sexp_of]

  val of_ns_since_midnight : int -> t
  val to_ns_since_midnight : t -> int

  (** [HH:MM:SS.nnnnnnnnn] *)
  val to_string_hum : t -> string
end

(** A price with four implied decimal places -- the spec's [Price (4)].

    Stored as the raw wire integer, so [Price.of_raw_4 1_234_500] is $123.45.
    Never converted to [float] except for display: the whole point of a fixed
    point representation is that $123.45 has no exact binary float. *)
module Price : sig
  type t [@@deriving compare, equal, hash, sexp_of]

  (** The spec caps Price (4) at 200,000.0000 (0x77359400). *)
  val max_raw_4 : int

  val of_raw_4 : int -> t
  val to_raw_4 : t -> int
  val to_string : t -> string
end

(** An 8-byte alpha stock symbol, space padded on the right by the protocol.

    Padding is stripped on the way in, so ["AAPL    "] and ["AAPL"] compare
    equal. *)
module Stock : sig
  type t [@@deriving compare, equal, hash, sexp_of]

  val of_padded_string : string -> t
  val to_string : t -> string
end

(** A day-unique order reference number. 64-bit unsigned on the wire. *)
module Order_ref : sig
  type t [@@deriving compare, equal, hash, sexp_of]

  (** Raises if the wire value has bit 63 set, i.e. does not fit in OCaml's
      63-bit native [int]. Nasdaq assigns these sequentially from a small base,
      so in practice they stay far below that -- but the field is nominally
      [uint64], and silently truncating an order id would corrupt the book in a
      way that is very hard to trace back. Fail loudly instead. *)
  val of_uint64_exn : int -> t

  val to_int : t -> int
end

(** A day-unique execution match number. 64-bit unsigned on the wire. *)
module Match_number : sig
  type t [@@deriving compare, equal, hash, sexp_of]

  val of_uint64_exn : int -> t
  val to_int : t -> int
end

(** A number of shares. *)
module Shares : sig
  type t [@@deriving compare, equal, hash, sexp_of]

  val of_int : int -> t
  val to_int : t -> int
end

(** The per-day locate code identifying a security, carried in every header.

    Cheaper to key on than {!Stock.t}: it is a small dense integer, so a book
    keyed by locate can be a flat array rather than a hashtable. *)
module Locate : sig
  type t [@@deriving compare, equal, hash, sexp_of]

  val of_int : int -> t
  val to_int : t -> int
end

module Side : sig
  type t =
    | Buy
    | Sell
  [@@deriving compare, equal, hash, sexp_of, enumerate]

  val of_char_exn : char -> t
  val to_char : t -> char
end
