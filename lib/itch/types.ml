open! Core

module Timestamp = struct
  type t = int [@@deriving compare, equal, hash, sexp_of]

  let of_ns_since_midnight ns =
    if ns < 0 then raise_s [%message "negative ITCH timestamp" (ns : int)];
    ns
  ;;

  let to_ns_since_midnight t = t

  let to_string_hum t =
    let ns_per_second = 1_000_000_000 in
    let total_seconds = t / ns_per_second in
    let ns = t % ns_per_second in
    sprintf
      "%02d:%02d:%02d.%09d"
      (total_seconds / 3600)
      (total_seconds % 3600 / 60)
      (total_seconds % 60)
      ns
  ;;
end

module Price = struct
  type t = int [@@deriving compare, equal, hash, sexp_of]

  (* Spec, "Data Types": "The maximum value of price (4) in TotalView ITCH is
     200,000.0000 (decimal, 77359400 hex)." *)
  let max_raw_4 = 0x77359400

  let of_raw_4 raw =
    if raw < 0 then raise_s [%message "negative ITCH price" (raw : int)];
    raw
  ;;

  let to_raw_4 t = t
  let to_string t = sprintf "%d.%04d" (t / 10_000) (t % 10_000)
end

module Stock = struct
  type t = string [@@deriving compare, equal, hash, sexp_of]

  (* Spec, "Data Types": "All alpha fields are ASCII fields which are left
     justified and padded on the right with spaces." *)
  let of_padded_string s = String.rstrip ~drop:(Char.equal ' ') s
  let to_string t = t
end

module Uint64_id = struct
  type t = int [@@deriving compare, equal, hash, sexp_of]

  let of_uint64_exn n =
    if n < 0
    then
      raise_s
        [%message
          "ITCH 64-bit id does not fit in a native OCaml int (bit 63 set)" (n : int)];
    n
  ;;

  let to_int t = t
end

module Order_ref = Uint64_id
module Match_number = Uint64_id

module Shares = struct
  type t = int [@@deriving compare, equal, hash, sexp_of]

  let of_int n =
    if n < 0 then raise_s [%message "negative ITCH share count" (n : int)];
    n
  ;;

  let to_int t = t
end

module Locate = struct
  type t = int [@@deriving compare, equal, hash, sexp_of]

  let of_int n =
    if n < 0 then raise_s [%message "negative ITCH locate code" (n : int)];
    n
  ;;

  let to_int t = t
end

module Side = struct
  type t =
    | Buy
    | Sell
  [@@deriving compare, equal, hash, sexp_of, enumerate]

  let of_char_exn = function
    | 'B' -> Buy
    | 'S' -> Sell
    | c -> raise_s [%message "unknown ITCH buy/sell indicator" (c : char)]
  ;;

  let to_char = function
    | Buy -> 'B'
    | Sell -> 'S'
  ;;
end
