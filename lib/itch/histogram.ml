open! Core

type t =
  { counts : int array
  ; significant_bits : int
  ; sub_buckets : int (* 1 lsl significant_bits *)
  ; mutable count : int
  (* The running total is carried in two limbs, base 2 ** 30, because one int
     is not enough and this was found the hard way. A full trading day
     terminates 223 million orders whose lifetimes sum to about 6.2e18
     nanoseconds, and OCaml's int tops out at 4.6e18: the first full-day run
     reported a mean lifetime of -13.6 seconds. Nothing raised. A float
     accumulator would not have overflowed but would have stopped being exact,
     and exactness is what lets an independent implementation check this at all.
     [total_low] is always in [0, 2 ** 30). *)
  ; mutable total_high : int
  ; mutable total_low : int
  ; mutable min_value : int
  ; mutable max_value : int
  }

(* Values below [2 * sub_buckets] get one bucket each, so the low end is exact
   rather than merely precise. Above that, each octave is split into
   [sub_buckets] even slices. *)
let max_significant_bits = 16

let create ~significant_bits =
  if significant_bits < 1 || significant_bits > max_significant_bits
  then
    raise_s
      [%message
        "Histogram.create: significant_bits out of range" (significant_bits : int)];
  let sub_buckets = 1 lsl significant_bits in
  (* Cover the whole non-negative int range: 62 octaves is more than a 63-bit
     int can reach, so no value can ever fall off the end and be silently
     dropped into the last bucket. *)
  let buckets = ((62 - significant_bits) * sub_buckets) + (2 * sub_buckets) in
  { counts = Array.create ~len:buckets 0
  ; significant_bits
  ; sub_buckets
  ; count = 0
  ; total_high = 0
  ; total_low = 0
  ; min_value = Int.max_value
  ; max_value = Int.min_value
  }
;;

(* floor(log2 v) for v >= 1, in a handful of steps rather than one shift at a
   time: this runs once per terminated order, of which a trading day has
   hundreds of millions, and a bit-at-a-time loop would cost sixty iterations
   apiece. Top-level and tail recursive, so no closure is allocated. *)
let rec floor_log2 v acc =
  if v >= 0x1_0000_0000
  then floor_log2 (v lsr 32) (acc + 32)
  else if v >= 0x1_0000
  then floor_log2 (v lsr 16) (acc + 16)
  else if v >= 0x100
  then floor_log2 (v lsr 8) (acc + 8)
  else if v >= 0x10
  then floor_log2 (v lsr 4) (acc + 4)
  else if v >= 0x4
  then floor_log2 (v lsr 2) (acc + 2)
  else if v >= 0x2
  then acc + 1
  else acc
;;

let[@inline] bucket_index t value =
  let m = t.sub_buckets in
  if value < 2 * m
  then value
  else (
    let shift = floor_log2 value 0 - t.significant_bits in
    let sub = (value lsr shift) land (m - 1) in
    (shift * m) + m + sub)
;;

let[@zero_alloc] record t value =
  if value < 0 then raise_s [%message "Histogram.record: negative value" (value : int)];
  let index = bucket_index t value in
  Array.unsafe_set t.counts index (Array.unsafe_get t.counts index + 1);
  t.count <- t.count + 1;
  let sum = t.total_low + (value land 0x3FFF_FFFF) in
  t.total_high <- t.total_high + (value lsr 30) + (sum lsr 30);
  t.total_low <- sum land 0x3FFF_FFFF;
  if value < t.min_value then t.min_value <- value;
  if value > t.max_value then t.max_value <- value
;;

let count t = t.count
let total_high t = t.total_high
let total_low t = t.total_low

let mean t =
  if t.count = 0
  then 0.
  else
    ((Float.of_int t.total_high *. 1073741824.) +. Float.of_int t.total_low)
    /. Float.of_int t.count
;;
let min_value t = if t.count = 0 then 0 else t.min_value
let max_value t = if t.count = 0 then 0 else t.max_value

let bucket_low t index =
  let m = t.sub_buckets in
  if index < 2 * m
  then index
  else (
    let shift = (index - m) / m in
    let sub = (index - m) land (m - 1) in
    (m + sub) lsl shift)
;;

let bucket_high t index =
  let m = t.sub_buckets in
  if index < 2 * m then index else bucket_low t index + (1 lsl ((index - m) / m)) - 1
;;

let bucket_at_percentile t p =
  if t.count = 0
  then -1
  else (
    let p = Float.max 0. (Float.min 100. p) in
    (* Round up, and never ask for fewer than one: the 0th percentile is the
       first recorded value, not a vacuous answer. *)
    let target =
      Int.max 1 (Float.iround_up_exn (p /. 100. *. Float.of_int t.count))
    in
    let rec scan index cumulative =
      if index >= Array.length t.counts
      then Array.length t.counts - 1
      else (
        let cumulative = cumulative + t.counts.(index) in
        if cumulative >= target then index else scan (index + 1) cumulative)
    in
    scan 0 0)
;;

let count_at_or_below t value =
  let limit = bucket_index t value in
  let rec scan index cumulative =
    if index > limit then cumulative else scan (index + 1) (cumulative + t.counts.(index))
  in
  scan 0 0
;;

let iter_buckets t ~f =
  Array.iteri t.counts ~f:(fun index count ->
    if count > 0 then f ~low:(bucket_low t index) ~high:(bucket_high t index) ~count)
;;
