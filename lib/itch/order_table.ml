open! Core

(* Order references are non-negative, so a negative sentinel cannot collide with
   a real key and no parallel occupancy bitmap is needed. *)
let empty_key = -1

type t =
  { keys : int array
  ; timestamps : int array
  ; shares : int array
  ; mask : int
  ; capacity_log2 : int
  (* Refuse new entries past this, not at [capacity]: see [insert]. *)
  ; load_limit : int
  ; mutable length : int
  ; mutable max_occupancy : int
  }

let create ~capacity_log2 =
  if capacity_log2 < 1 || capacity_log2 > 40
  then
    raise_s
      [%message "Order_table.create: capacity_log2 out of range" (capacity_log2 : int)];
  let capacity = 1 lsl capacity_log2 in
  { keys = Array.create ~len:capacity empty_key
  ; timestamps = Array.create ~len:capacity 0
  ; shares = Array.create ~len:capacity 0
  ; mask = capacity - 1
  ; capacity_log2
  ; load_limit = capacity / 8 * 7
  ; length = 0
  ; max_occupancy = 0
  }
;;

let capacity t = Array.length t.keys
let length t = t.length
let max_occupancy t = t.max_occupancy

(* Order references arrive very nearly sequentially, so the identity hash would
   lay them down in one unbroken run and make every probe chain as long as the
   live set. Multiplying by an odd constant and folding the high bits down
   scatters them. The constant is below 2^62 because OCaml ints are 63 bits and
   a larger literal does not exist; oddness is what makes the multiply a
   bijection, which is what matters here. *)
let[@inline] hash_key key mask =
  let h = key * 0x2545_F491_4F6C_DD1D in
  let h = h lxor (h lsr 29) in
  h land mask
;;

(* Every loop below is a top-level [let rec] taking its state as arguments
   rather than an inner one closing over it. That is not a style preference. An
   inner [let rec] that captures [t], [keys] and [mask] is a closure, and a
   closure is a heap allocation on every call -- which is precisely the 64-byte
   per-call block [@zero_alloc] caught in [Reader.consume], invisible to a test
   that only checks allocation does not grow with message count. These run once
   per message, so the same mistake here would cost the same way. *)
let rec probe_for keys key mask i =
  let k = Array.unsafe_get keys i in
  if k = key then i else if k = empty_key then -1 else probe_for keys key mask ((i + 1) land mask)
;;

let[@zero_alloc] slot t ~key =
  let mask = t.mask in
  probe_for t.keys key mask (hash_key key mask)
;;

let[@zero_alloc] timestamp_at t index = Array.unsafe_get t.timestamps index
let[@zero_alloc] shares_at t index = Array.unsafe_get t.shares index
let[@zero_alloc] set_shares_at t index shares = Array.unsafe_set t.shares index shares

let rec probe_free t keys key mask timestamp shares i =
  let k = Array.unsafe_get keys i in
  if k = empty_key
  then (
    Array.unsafe_set keys i key;
    Array.unsafe_set t.timestamps i timestamp;
    Array.unsafe_set t.shares i shares;
    t.length <- t.length + 1;
    if t.length > t.max_occupancy then t.max_occupancy <- t.length)
  else if k = key
  then raise_s [%message "Order_table.insert: duplicate key" (key : int)]
  else probe_free t keys key mask timestamp shares ((i + 1) land mask)
;;

let[@zero_alloc] insert t ~key ~timestamp ~shares =
  if key < 0 then raise_s [%message "Order_table.insert: negative key" (key : int)];
  (* Refuse before capacity rather than at it. The table cannot grow -- growing
     means allocating, and allocating is what the zero-alloc contract forbids --
     so the alternatives are an exception or an unbounded probe. Stopping at
     seven eighths matters because a linear-probing table does not degrade
     gracefully: at 99% occupancy the average probe runs to tens of slots and a
     run that was going to fail anyway crawls for minutes first. The message
     carries what is needed to fix it, because this surfaces a hundred seconds
     into a replay of a 12.95 GB file. *)
  if t.length >= t.load_limit
  then
    raise_s
      [%message
        "Order_table.insert: live-order table is full -- rerun with a larger \
         capacity_log2"
          ~capacity:(Array.length t.keys : int)
          ~live:(t.length : int)
          ~current_capacity_log2:(t.capacity_log2 : int)
          ~try_capacity_log2:(t.capacity_log2 + 2 : int)];
  let mask = t.mask in
  probe_free t t.keys key mask timestamp shares (hash_key key mask)
;;

(* Backward-shift deletion. Simply blanking the slot is the classic linear
   probing bug: it breaks every probe chain that ran through the hole, so keys
   inserted after a collision become unfindable while [length] still counts them.
   [test_order_table.ml] measures that directly -- with the naive version it
   reports ~170 lookup disagreements per seed and 54 of 300 sequential keys lost.

   The rule is Knuth's: walk forward from the hole, and move an entry back into
   it only when the entry's home slot does not lie cyclically in (hole, here].
   An entry whose home does lie in that interval is already at or after its home
   relative to the hole, so moving it back would put it before its home and make
   it unfindable in turn. *)
let rec shift_into t keys mask i j =
  let j = (j + 1) land mask in
  let kj = Array.unsafe_get keys j in
  if kj = empty_key
  then ()
  else (
    let home = hash_key kj mask in
    let cyclically_in_range =
      if i <= j then i < home && home <= j else i < home || home <= j
    in
    if cyclically_in_range
    then shift_into t keys mask i j
    else (
      Array.unsafe_set keys i kj;
      Array.unsafe_set t.timestamps i (Array.unsafe_get t.timestamps j);
      Array.unsafe_set t.shares i (Array.unsafe_get t.shares j);
      Array.unsafe_set keys j empty_key;
      shift_into t keys mask j j))
;;

let[@zero_alloc] delete_at t index =
  let keys = t.keys in
  Array.unsafe_set keys index empty_key;
  shift_into t keys t.mask index index;
  t.length <- t.length - 1
;;

let iter t ~f =
  Array.iteri t.keys ~f:(fun i key ->
    if key <> empty_key then f ~key ~timestamp:t.timestamps.(i) ~shares:t.shares.(i))
;;
