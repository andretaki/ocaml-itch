open! Core
open Itch

(* The oracle is [Core.Hashtbl]: it is a different data structure with the same
   contract, so any sequence of operations must leave the two agreeing on every
   key. Doing it as a randomised sequence rather than a handful of cases is the
   point -- the failure mode this table has to avoid only appears when a key is
   deleted from the middle of a probe chain and a later key in that chain then
   has to still be findable. Hand-picked examples miss it; a few thousand random
   operations against a deliberately tiny table do not. *)

let deleted_from_probe_chain = ref 0

let run_sequence ~capacity_log2 ~key_space ~operations ~seed =
  let random = Random.State.make [| seed |] in
  let table = Order_table.create ~capacity_log2 in
  let oracle = Int.Table.create () in
  let disagreements = ref 0 in
  let check key =
    let mine =
      match Order_table.slot table ~key with
      | -1 -> None
      | index ->
        Some (Order_table.timestamp_at table index, Order_table.shares_at table index)
    in
    let theirs = Hashtbl.find oracle key in
    if not ([%equal: (int * int) option] mine theirs) then incr disagreements
  in
  for step = 1 to operations do
    let key = Random.State.int random key_space in
    let present = Hashtbl.mem oracle key in
    (* Insert when absent, and otherwise either update or delete, so the table
       spends its life near the load factor a real run would hold it at. *)
    (match present, Random.State.int random 3 with
     | false, _ ->
       (* Leave headroom: a full table raises by design, and that is not what
          this test is probing. *)
       if Order_table.length table < Order_table.capacity table * 2 / 3
       then (
         let timestamp = step * 1_000 in
         let shares = 100 + Random.State.int random 900 in
         Order_table.insert table ~key ~timestamp ~shares;
         Hashtbl.set oracle ~key ~data:(timestamp, shares))
     | true, 0 ->
       let index = Order_table.slot table ~key in
       (* Was this key sitting in the middle of a probe chain? If the following
          slot is occupied, deleting without repairing the chain orphans it.
          Counting these proves the sequence actually exercised the hard case
          rather than only ever deleting chain tails. *)
       if index >= 0
       then (
         let next = (index + 1) land (Order_table.capacity table - 1) in
         (* "Occupied" is derived from the oracle rather than read out of the
            table, because the table exposes no slot-occupancy accessor and
            should not: adding one purely for a test would be a hole in the
            abstraction. Any live key that probes to [next] proves the chain
            continues past the entry about to be removed. *)
         if Hashtbl.existsi oracle ~f:(fun ~key:other ~data:_ ->
              other <> key && Order_table.slot table ~key:other = next)
         then incr deleted_from_probe_chain;
         Order_table.delete_at table index;
         Hashtbl.remove oracle key)
     | true, _ ->
       let index = Order_table.slot table ~key in
       if index >= 0
       then (
         let shares = Order_table.shares_at table index - 1 in
         Order_table.set_shares_at table index shares;
         let timestamp, _ = Hashtbl.find_exn oracle key in
         Hashtbl.set oracle ~key ~data:(timestamp, shares)));
    check key
  done;
  (* A final sweep over the whole key space, not just the keys touched: a broken
     probe chain hides a key that nothing has looked up since. *)
  for key = 0 to key_space - 1 do
    check key
  done;
  !disagreements, Hashtbl.length oracle, Order_table.length table
;;

let%expect_test "matches Core.Hashtbl over random insert/update/delete sequences" =
  List.iter [ 1; 2; 3; 4; 5 ] ~f:(fun seed ->
    let disagreements, oracle_size, table_size =
      run_sequence ~capacity_log2:5 ~key_space:200 ~operations:4_000 ~seed
    in
    printf
      "seed %d: disagreements %d, oracle %d, table %d\n"
      seed
      disagreements
      oracle_size
      table_size);
  [%expect
    {|
    seed 1: disagreements 0, oracle 21, table 21
    seed 2: disagreements 0, oracle 21, table 21
    seed 3: disagreements 0, oracle 21, table 21
    seed 4: disagreements 0, oracle 21, table 21
    seed 5: disagreements 0, oracle 21, table 21
    |}]
;;

let%expect_test "the sequences really do delete from the middle of probe chains" =
  (* If this ever prints 0 the test above has stopped proving anything. *)
  printf "mid-chain deletions exercised: %b\n" (!deleted_from_probe_chain > 0);
  [%expect {| mid-chain deletions exercised: true |}]
;;

let%expect_test "sequential keys, which is what order references actually look like" =
  let table = Order_table.create ~capacity_log2:10 in
  for key = 1 to 600 do
    Order_table.insert table ~key ~timestamp:(key * 7) ~shares:key
  done;
  (* Delete every other one, then confirm the survivors are all still findable.
     Sequential keys are the adversarial case for a table that hashes weakly. *)
  for key = 1 to 600 do
    if key % 2 = 0
    then (
      let index = Order_table.slot table ~key in
      Order_table.delete_at table index)
  done;
  let missing = ref 0 in
  let wrong = ref 0 in
  for key = 1 to 600 do
    let index = Order_table.slot table ~key in
    if key % 2 = 1
    then
      if index = -1
      then incr missing
      else if Order_table.timestamp_at table index <> key * 7
      then incr wrong
      else ()
    else if index <> -1
    then incr wrong
  done;
  printf "missing %d, wrong %d, live %d\n" !missing !wrong (Order_table.length table);
  [%expect {| missing 0, wrong 0, live 300 |}]
;;

let%expect_test "a full table raises rather than spinning" =
  let table = Order_table.create ~capacity_log2:2 in
  let result =
    Or_error.try_with (fun () ->
      for key = 1 to 100 do
        Order_table.insert table ~key ~timestamp:0 ~shares:0
      done)
  in
  printf "%s\n" (match result with Ok () -> "no error" | Error _ -> "raised");
  [%expect {| raised |}]
;;
