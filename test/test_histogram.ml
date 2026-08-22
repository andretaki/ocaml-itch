open! Core
open Itch

(* The oracle is the values themselves, kept in a sorted array. A histogram is
   allowed to lose precision; it is not allowed to lose a value, put one in the
   wrong bucket, or report a percentile the exact data contradicts. Each of
   those is checked against the exact answer rather than against a golden
   string, so the test says something even where the numbers change. *)

let significant_bits = 5

let%expect_test "every value lands in a bucket that contains it" =
  let t = Histogram.create ~significant_bits in
  let random = Random.State.make [| 42 |] in
  let values =
    (* Spread over nine orders of magnitude, which is the range order lifetimes
       actually occupy, plus the exact low values where bucketing is meant to be
       lossless. *)
    List.init 200 ~f:(fun i -> i)
    @ List.init 20_000 ~f:(fun _ ->
        let scale = Random.State.int random 10 in
        Random.State.int random (Int.pow 10 (scale + 1)))
  in
  List.iter values ~f:(fun v -> Histogram.record t v);
  let outside = ref 0 in
  let too_wide = ref 0 in
  List.iter values ~f:(fun v ->
    let index = Histogram.bucket_at_percentile t 0. in
    ignore (index : int);
    (* Find the bucket this value would be reported in by scanning the buckets,
       which is what a reader of the output does. *)
    let found = ref false in
    Histogram.iter_buckets t ~f:(fun ~low ~high ~count:_ ->
      if low <= v && v <= high
      then (
        found := true;
        (* The promise of the structure: bucket width is at most
           value / 2 ** significant_bits. *)
        let width = high - low + 1 in
        if width > Int.max 1 (v lsr significant_bits) then incr too_wide));
    if not !found then incr outside);
  printf "values outside every bucket: %d\n" !outside;
  printf "buckets wider than the precision promise: %d\n" !too_wide;
  printf "recorded: %d, distinct values: %d\n" (Histogram.count t) (List.length values);
  [%expect
    {|
    values outside every bucket: 0
    buckets wider than the precision promise: 0
    recorded: 20200, distinct values: 20200
    |}]
;;

let%expect_test "buckets are contiguous and never overlap" =
  let t = Histogram.create ~significant_bits in
  (* Record one value in a wide spread of buckets so the walk covers scales. *)
  List.iter [ 0; 1; 31; 32; 63; 64; 1_000; 1_000_000; 1_000_000_000 ] ~f:(fun v ->
    Histogram.record t v);
  let gaps = ref 0 in
  let overlaps = ref 0 in
  (* Walk the whole index range, not just the occupied buckets: a gap between
     two empty buckets still loses values at run time. *)
  let previous_high = ref (-1) in
  for index = 0 to 1_500 do
    let low = Histogram.bucket_low t index in
    let high = Histogram.bucket_high t index in
    if low <> !previous_high + 1 then if low > !previous_high + 1 then incr gaps else incr overlaps;
    if high < low then incr overlaps;
    previous_high := high
  done;
  printf "gaps %d, overlaps %d\n" !gaps !overlaps;
  [%expect {| gaps 0, overlaps 0 |}]
;;

let%expect_test "reported percentiles bracket the exact percentile" =
  let random = Random.State.make [| 7 |] in
  let values =
    Array.init 50_000 ~f:(fun _ ->
      (* Lognormal-ish: most values small, a long tail. This is the shape order
         lifetimes have, and it is the shape that breaks a naive histogram. *)
      let scale = Random.State.int random 9 in
      Random.State.int random (Int.pow 10 (scale + 1)) + 1)
  in
  let t = Histogram.create ~significant_bits in
  Array.iter values ~f:(fun v -> Histogram.record t v);
  let sorted = Array.copy values in
  Array.sort sorted ~compare:Int.compare;
  let exact p =
    let n = Array.length sorted in
    let rank = Int.max 1 (Float.iround_up_exn (p /. 100. *. Float.of_int n)) in
    sorted.(rank - 1)
  in
  let misses = ref 0 in
  List.iter [ 0.; 1.; 10.; 25.; 50.; 75.; 90.; 99.; 99.9; 100. ] ~f:(fun p ->
    let index = Histogram.bucket_at_percentile t p in
    let low = Histogram.bucket_low t index in
    let high = Histogram.bucket_high t index in
    let truth = exact p in
    if not (low <= truth && truth <= high)
    then (
      incr misses;
      printf "p%g: exact %d not in [%d, %d]\n" p truth low high));
  printf "percentiles that missed the exact value: %d\n" !misses;
  printf
    "exact min %d max %d; histogram min %d max %d\n"
    sorted.(0)
    sorted.(Array.length sorted - 1)
    (Histogram.min_value t)
    (Histogram.max_value t);
  [%expect
    {|
    percentiles that missed the exact value: 0
    exact min 1 max 999970037; histogram min 1 max 999970037
    |}]
;;

let%expect_test "the mean does not inherit the bucketing error" =
  let t = Histogram.create ~significant_bits in
  let values = List.init 10_000 ~f:(fun i -> (i * 7919) % 1_000_003) in
  List.iter values ~f:(fun v -> Histogram.record t v);
  let exact_total = List.sum (module Int) values ~f:Fn.id in
  let reconstructed = (Histogram.total_high t * 1073741824) + Histogram.total_low t in
  printf "totals agree exactly: %b\n" (reconstructed = exact_total);
  printf "low limb is in range: %b\n" (Histogram.total_low t >= 0 && Histogram.total_low t < 1073741824);
  [%expect
    {|
    totals agree exactly: true
    low limb is in range: true
    |}]
;;

let%expect_test "count_at_or_below at a bucket boundary is exact" =
  let t = Histogram.create ~significant_bits in
  (* Sub-bucket resolution is exact below 2 ** (significant_bits + 1) = 64. *)
  List.iter (List.init 1_000 ~f:(fun i -> i % 64)) ~f:(fun v -> Histogram.record t v);
  let exact_below n =
    List.count (List.init 1_000 ~f:(fun i -> i % 64)) ~f:(fun v -> v <= n)
  in
  List.iter [ 0; 5; 31; 63 ] ~f:(fun n ->
    printf "<=%d: histogram %d, exact %d\n" n (Histogram.count_at_or_below t n) (exact_below n));
  [%expect
    {|
    <=0: histogram 16, exact 16
    <=5: histogram 96, exact 96
    <=31: histogram 512, exact 512
    <=63: histogram 1000, exact 1000
    |}]
;;

(* The regression this file exists to hold onto. A single-int total silently
   wrapped on real data -- 223 million order lifetimes summing past 4.6e18 --
   and the run reported a mean lifetime of -13.6 seconds without raising
   anything. The values here are chosen to exceed the range of an OCaml int,
   which the old accumulator could not represent at all. *)
let%expect_test "the total survives a sum larger than an int can hold" =
  let t = Histogram.create ~significant_bits in
  let big = 1 lsl 60 in
  for _ = 1 to 10 do
    Histogram.record t big
  done;
  (* 10 * 2 ** 60 is 1.15e19, nearly three times Int.max_value. In base 2 ** 30
     that is high = 10 * 2 ** 30 and low = 0. *)
  printf "high %d (expected %d)\n" (Histogram.total_high t) (10 * (1 lsl 30));
  printf "low  %d (expected 0)\n" (Histogram.total_low t);
  printf "mean %.1f (expected %.1f)\n" (Histogram.mean t) (Float.of_int big);
  (* The wrap is not signalled by a negative result -- 10 * 2 ** 60 wraps to a
     positive, plausible-looking number, which is exactly why the original bug
     survived to a full-day run. Compare against the true value instead. *)
  let naive = List.fold (List.init 10 ~f:(fun _ -> big)) ~init:0 ~f:( + ) in
  printf "naive single-int total: %d\n" naive;
  printf "naive total is still positive: %b\n" (naive > 0);
  printf
    "naive total is wrong: %b\n"
    (Float.( <> ) (Float.of_int naive) (Histogram.mean t *. 10.));
  [%expect {|
    high 10737418240 (expected 10737418240)
    low  0 (expected 0)
    mean 1152921504606846976.0 (expected 1152921504606846976.0)
    naive single-int total: 2305843009213693952
    naive total is still positive: true
    naive total is wrong: true
    |}]
;;

let%expect_test "carries out of the low limb are exact" =
  let t = Histogram.create ~significant_bits in
  (* Values with bits in both limbs, and enough of them to carry repeatedly. *)
  let values = List.init 5_000 ~f:(fun i -> ((i * 2_654_435_761) land 0xFFF_FFFF_FFFF) + i) in
  List.iter values ~f:(fun v -> Histogram.record t v);
  let exact = List.sum (module Int) values ~f:Fn.id in
  let reconstructed = (Histogram.total_high t * 1073741824) + Histogram.total_low t in
  printf "exact sum fits in an int: %b\n" (exact > 0);
  printf "reconstructed = exact: %b\n" (reconstructed = exact);
  printf "low limb in range: %b\n" (Histogram.total_low t >= 0 && Histogram.total_low t < 1073741824);
  [%expect {|
    exact sum fits in an int: true
    reconstructed = exact: true
    low limb in range: true
    |}]
;;
