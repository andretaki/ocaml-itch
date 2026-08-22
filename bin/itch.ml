open! Core
open Itch

(** Wall clock time is not monotonic. Under WSL 2 it can step backwards, and a
    backwards step is not a small error in a "fastest of N" loop -- it produces a
    negative elapsed time, which then wins every comparison and is reported as
    the result. That is not hypothetical: a benchmark round on this machine
    printed [fastest of 7 -1.8768 s (-16.92 M msg/s)] before this was fixed.

    [Clock.gettime Monotonic] cannot go backwards. It is an [Or_error] because
    not every platform provides it, so fall back to the wall clock where it is
    missing rather than refusing to run. *)
let now_seconds =
  match Core_unix.Clock.gettime with
  | Ok gettime -> fun () -> Int63.to_float (gettime Core_unix.Clock.Monotonic) /. 1e9
  | Error _ -> Core_unix.gettimeofday
;;

(** mmap rather than read: the parser decodes in place out of the mapped pages,
    so nothing copies the file into the heap on the way in. *)
let with_mapped_file path ~f =
  let fd = Core_unix.openfile path ~mode:[ O_RDONLY ] in
  Exn.protect
    ~f:(fun () ->
      let size = (Core_unix.fstat fd).st_size |> Int64.to_int_exn in
      let buf = Bigstring_unix.map_file ~shared:false fd size in
      f buf)
    ~finally:(fun () -> Core_unix.close fd)
;;

let stats_command =
  Command.basic
    ~summary:"Count messages by type in an ITCH 5.0 file"
    (let%map_open.Command path = anon ("FILE" %: Filename_unix.arg_type) in
     fun () ->
       with_mapped_file path ~f:(fun buf ->
         let counts = Array.create ~len:256 0 in
         let total = ref 0 in
         (* Field level aggregates, so that cross-checking against an
            independent implementation compares decoded values and not just
            message counts. *)
         let added_shares = ref 0 in
         let executed_shares = ref 0 in
         let cancelled_shares = ref 0 in
         let max_order_ref = ref 0 in
         let add_notional = ref 0 in
         let note_order_ref order_ref =
           let order_ref = Types.Order_ref.to_int order_ref in
           if order_ref > !max_order_ref then max_order_ref := order_ref
         in
         let first_timestamp = ref None in
         let last_timestamp = ref None in
         let start = now_seconds () in
         let consumed =
           Reader.iter buf ~pos:0 ~len:(Bigstring.length buf) ~f:(fun message ->
             incr total;
             let index = Char.to_int (Message.message_type message) in
             counts.(index) <- counts.(index) + 1;
             (match message with
              | Add_order m ->
                added_shares := !added_shares + Types.Shares.to_int m.shares;
                add_notional
                := !add_notional
                   + (Types.Shares.to_int m.shares * Types.Price.to_raw_4 m.price);
                note_order_ref m.order_ref
              | Order_executed m ->
                executed_shares
                := !executed_shares + Types.Shares.to_int m.executed_shares;
                note_order_ref m.order_ref
              | Order_executed_with_price m ->
                executed_shares
                := !executed_shares + Types.Shares.to_int m.executed_shares;
                note_order_ref m.order_ref
              | Order_cancel m ->
                cancelled_shares
                := !cancelled_shares + Types.Shares.to_int m.cancelled_shares;
                note_order_ref m.order_ref
              | Order_delete m -> note_order_ref m.order_ref
              | Order_replace m ->
                note_order_ref m.original_order_ref;
                note_order_ref m.new_order_ref
              | System_event _ | Stock_directory _ | Unparsed _ -> ());
             match Message.timestamp message with
             | None -> ()
             | Some timestamp ->
               if Option.is_none !first_timestamp then first_timestamp := Some timestamp;
               last_timestamp := Some timestamp)
         in
         let elapsed = now_seconds () -. start in
         let size = Bigstring.length buf in
         printf "file            %s\n" path;
         printf "size            %d bytes\n" size;
         printf
           "consumed        %d bytes (%d unconsumed tail)\n"
           consumed
           (size - consumed);
         printf "messages        %d\n" !total;
         printf
           "elapsed         %.3f s  (%.2f M msg/s, %.1f MB/s)\n"
           elapsed
           (Float.of_int !total /. elapsed /. 1e6)
           (Float.of_int consumed /. elapsed /. 1e6);
         Option.iter !first_timestamp ~f:(fun t ->
           printf "first timestamp %s\n" (Types.Timestamp.to_string_hum t));
         Option.iter !last_timestamp ~f:(fun t ->
           printf "last timestamp  %s\n" (Types.Timestamp.to_string_hum t));
         printf "added shares    %d\n" !added_shares;
         printf "executed shares %d\n" !executed_shares;
         printf "cancelled share %d\n" !cancelled_shares;
         printf "add notional    %d (raw price(4) * shares)\n" !add_notional;
         printf "max order ref   %d\n" !max_order_ref;
         printf "\n%-6s %12s  %s\n" "type" "count" "decoded";
         Array.iteri counts ~f:(fun index count ->
           if count > 0
           then (
             let c = Char.of_int_exn index in
             let decoded =
               match c with
               | 'S' | 'R' | 'A' | 'F' | 'E' | 'C' | 'X' | 'D' | 'U' -> "yes"
               | _ -> "no"
             in
             printf "%-6s %12d  %s\n" (Char.to_string c) count decoded))))
;;

let dump_command =
  Command.basic
    ~summary:"Print the first N messages of an ITCH 5.0 file as s-expressions"
    (let%map_open.Command path = anon ("FILE" %: Filename_unix.arg_type)
     and count = flag "-n" (optional_with_default 10 int) ~doc:"N how many messages" in
     fun () ->
       with_mapped_file path ~f:(fun buf ->
         let printed = ref 0 in
         try
           Reader.iter buf ~pos:0 ~len:(Bigstring.length buf) ~f:(fun message ->
             if !printed >= count then raise Exit;
             incr printed;
             print_s [%sexp (message : Message.t)])
           |> (ignore : int -> unit)
         with
         | Exit -> ()))
;;

let book_command =
  Command.basic
    ~summary:"Reconstruct the limit order book by replaying an ITCH 5.0 file"
    (let%map_open.Command path = anon ("FILE" %: Filename_unix.arg_type)
     and symbol = flag "-symbol" (optional string) ~doc:"SYM show depth for a symbol"
     and levels =
       flag "-levels" (optional_with_default 5 int) ~doc:"N price levels per side"
     and tops =
       flag
         "-tops"
         no_arg
         ~doc:
           " print locate,bid_price,bid_shares,ask_price,ask_shares for every book, for \
            cross-checking against an independent implementation"
     in
     fun () ->
       with_mapped_file path ~f:(fun buf ->
         let books = Order_book.create () in
         (* Locate codes are assigned per day by the Stock Directory spin, so the
            symbol to locate mapping has to be learned from the file itself. *)
         let locate_of_symbol = String.Table.create () in
         let start = now_seconds () in
         let consumed =
           Reader.iter buf ~pos:0 ~len:(Bigstring.length buf) ~f:(fun message ->
             (match message with
              | Stock_directory m ->
                Hashtbl.set
                  locate_of_symbol
                  ~key:(Types.Stock.to_string m.stock)
                  ~data:m.stock_locate
              | _ -> ());
             Order_book.apply books message)
         in
         let elapsed = now_seconds () -. start in
         printf "replayed        %d bytes in %.3f s\n" consumed elapsed;
         printf "book messages   %d applied\n" (Order_book.applied books);
         printf
           "orphans         %d (modify for an order not seen)\n"
           (Order_book.orphans books);
         printf "live orders     %d\n" (Order_book.orders_live books);
         (* The exit status matters and used to not. This printed "invariants
            FAILED" and returned 0, so no script could gate on it -- the check
            was a thing a human had to read. *)
         let invariants_ok =
           match Order_book.check_invariants books with
           | Ok () ->
             printf "invariants      ok\n";
             true
           | Error error ->
             printf !"invariants      FAILED: %{Error#hum}\n" error;
             false
         in
         if tops
         then (
           (* A sentinel rather than a fixed header line count: the invariants
              error above is a sexp and wraps onto as many lines as it needs, so
              "tail -n +6" silently swept three lines of it into the CSV the
              first time this was diffed against another implementation. *)
           printf "-- tops --\n";
           List.iter (Order_book.locates books) ~f:(fun locate ->
             match Order_book.book_for books locate with
             | None -> ()
             | Some book ->
               let field = function
                 | None -> "-,-"
                 | Some (price, shares) ->
                   sprintf "%d,%d" (Types.Price.to_raw_4 price) shares
               in
               printf
                 "%d,%s,%s\n"
                 (Types.Locate.to_int locate)
                 (field (Order_book.Book.best_bid book))
                 (field (Order_book.Book.best_ask book))));
         (match symbol with
         | None -> ()
         | Some symbol ->
           (match Hashtbl.find locate_of_symbol symbol with
            | None -> printf "\nno stock directory entry for %s\n" symbol
            | Some locate ->
              (match Order_book.book_for books locate with
               | None -> printf "\nno book for %s\n" symbol
               | Some book ->
                 printf "\n%s (locate %d)\n" symbol (Types.Locate.to_int locate);
                 let show side label =
                   printf "  %s\n" label;
                   match Order_book.Book.depth book side ~levels with
                   | [] -> printf "    (empty)\n"
                   | levels ->
                     List.iter levels ~f:(fun (price, shares) ->
                       printf "    %12s  %8d\n" (Types.Price.to_string price) shares)
                 in
                 show Sell "asks (low to high)";
                 show Buy "bids (high to low)")));
         if not invariants_ok then Stdlib.exit 1))
;;

module Checksum_reader = Reader.Make (Checksum)

let checksum_command =
  Command.basic
    ~summary:
      "Fold a file through the zero-allocation path and print the reference aggregate"
    (let%map_open.Command path = anon ("FILE" %: Filename_unix.arg_type)
     and compare_paths =
       flag "-compare-paths" no_arg ~doc:" also run the allocating Message.t path"
     and repeats =
       flag "-repeats" (optional_with_default 1 int) ~doc:"N passes, for timing"
     in
     fun () ->
       with_mapped_file path ~f:(fun buf ->
         let len = Bigstring.length buf in
         let state = Checksum.create () in
         (* One untimed pass so the comparison is warm-cache throughput rather
            than a measurement of how fast this disk is. *)
         ignore (Checksum_reader.consume (Checksum.create ()) buf ~pos:0 ~len : int);
         let best = ref Float.infinity in
         let worst = ref 0. in
         (* A sample that is not strictly positive means the clock misbehaved,
            not that the parse was instant. Discarding it and saying so is the
            only honest option: silently keeping it would make it the reported
            result, since it beats every real sample. *)
         let discarded = ref 0 in
         for _ = 1 to repeats do
           let state = Checksum.create () in
           let start = now_seconds () in
           ignore (Checksum_reader.consume state buf ~pos:0 ~len : int);
           let elapsed = now_seconds () -. start in
           if Float.( <= ) elapsed 0.
           then incr discarded
           else (
             if Float.( < ) elapsed !best then best := elapsed;
             if Float.( > ) elapsed !worst then worst := elapsed)
         done;
         let consumed = Checksum_reader.consume state buf ~pos:0 ~len in
         printf "%s\n" (Checksum.to_string state);
         printf "consumed        %d bytes\n" consumed;
         if !discarded > 0
         then printf "discarded       %d non-positive timing samples\n" !discarded;
         if Float.is_inf !best
         then printf "no usable timing samples\n"
         else (
           printf
             "fastest of %d    %.4f s  (%.2f M msg/s, %.1f MB/s)\n"
             (repeats - !discarded)
             !best
             (Float.of_int state.messages /. !best /. 1e6)
             (Float.of_int consumed /. !best /. 1e6);
           printf
             "spread          %.4f s slowest, %+.1f%% over fastest\n"
             !worst
             ((!worst -. !best) /. !best *. 100.));
         if compare_paths
         then (
           let slow = Checksum.create () in
           let start = now_seconds () in
           let slow_consumed =
             Reader.iter buf ~pos:0 ~len ~f:(Checksum.of_message slow)
           in
           let elapsed = now_seconds () -. start in
           printf
             "\nMessage.t path  %.4f s  (%.2f M msg/s)\n"
             elapsed
             (Float.of_int slow.messages /. elapsed /. 1e6);
           printf
             "paths agree     %b\n"
             (String.equal (Checksum.to_string state) (Checksum.to_string slow)
              && consumed = slow_consumed))))
;;


module Analysis_reader = Reader.Make (Analysis)

(* Nanoseconds are unreadable at every scale that matters here, and rounding
   them to one unit is worse: the interesting lifetimes span microseconds to
   hours in the same table. Render each value in the largest unit that leaves a
   digit before the point. *)
let format_ns ns =
  let f = Float.of_int ns in
  if ns < 1_000
  then sprintf "%d ns" ns
  else if ns < 1_000_000
  then sprintf "%.2f us" (f /. 1e3)
  else if ns < 1_000_000_000
  then sprintf "%.2f ms" (f /. 1e6)
  else if ns < 60_000_000_000
  then sprintf "%.2f s" (f /. 1e9)
  else sprintf "%.1f min" (f /. 60e9)
;;

let format_ns_range low high =
  if low = high then format_ns low else sprintf "%s - %s" (format_ns low) (format_ns high)
;;

let percentiles = [ 1.; 10.; 25.; 50.; 75.; 90.; 99.; 99.9 ]

let print_lifetime_table label histogram =
  let count = Histogram.count histogram in
  if count = 0
  then printf "\n%s: none\n" label
  else (
    printf "\n%s (%d orders)\n" label count;
    printf "  %-8s %s\n" "mean" (format_ns (Float.iround_nearest_exn (Histogram.mean histogram)));
    printf "  %-8s %s\n" "min" (format_ns (Histogram.min_value histogram));
    List.iter percentiles ~f:(fun p ->
      let index = Histogram.bucket_at_percentile histogram p in
      printf
        "  p%-7g %s\n"
        p
        (format_ns_range
           (Histogram.bucket_low histogram index)
           (Histogram.bucket_high histogram index)));
    printf "  %-8s %s\n" "max" (format_ns (Histogram.max_value histogram)))
;;

(* The headline number, and the one most often quoted without evidence. Bucket
   boundaries are powers of two internally, so a threshold like "one
   millisecond" is asked for as "at or below 999,999 ns" -- one below the
   boundary -- and the count is then exact rather than rounded up into the
   bucket that straddles it. *)
let print_survival histogram =
  let count = Histogram.count histogram in
  if count > 0
  then (
    printf "\nshare of orders that lived less than\n";
    List.iter
      [ "1 us", 1_000; "10 us", 10_000; "100 us", 100_000; "1 ms", 1_000_000
      ; "10 ms", 10_000_000; "100 ms", 100_000_000; "1 s", 1_000_000_000
      ; "10 s", 10_000_000_000; "1 min", 60_000_000_000 ]
      ~f:(fun (label, threshold) ->
        let below = Histogram.count_at_or_below histogram (threshold - 1) in
        printf
          "  %-6s %12d  %5.2f%%\n"
          label
          below
          (Float.of_int below /. Float.of_int count *. 100.)))
;;

let analyze_command =
  Command.basic
    ~summary:"Order lifetime distribution and per-millisecond message rates"
    (let%map_open.Command path = anon ("FILE" %: Filename_unix.arg_type)
     and capacity_log2 =
       flag
         "-capacity-log2"
         (optional int)
         ~doc:"N live-order table is 2**N slots (default 25, about 34M)"
     and ms_csv =
       flag
         "-ms-csv"
         (optional string)
         ~doc:"PATH write ms,count for every millisecond of the session"
     and aggregate =
       flag
         "-aggregate"
         no_arg
         ~doc:
           " print one line of exact integers, for comparison against an \
            independent implementation"
     in
     fun () ->
       with_mapped_file path ~f:(fun buf ->
         let state = Analysis.create ?capacity_log2 () in
         let len = Bigstring.length buf in
         let start = now_seconds () in
         let consumed = Analysis_reader.consume state buf ~pos:0 ~len in
         let elapsed = now_seconds () -. start in
         if aggregate
         then (
           (* Every field here is an exact integer, deliberately: percentiles
              are bucketed and two implementations could disagree on one
              without either being wrong, so the cross-check compares only
              quantities that have a single right answer. The sum, min and max
              of the lifetimes pin the distribution's first moment and both
              ends without inheriting the histogram's resolution. *)
           let peak_ms = ref (-1) in
           let peak_count = ref (-1) in
           Analysis.iter_milliseconds state ~f:(fun ~ms ~count ->
             if count > !peak_count
             then (
               peak_count := count;
               peak_ms := ms));
           let all = Analysis.all_lifetimes state in
           printf
             "messages=%d decoded=%d timestamped=%d orphans=%d resting=%d executed=%d \
              cancelled=%d deleted=%d replaced=%d lifetime_sum_hi=%d lifetime_sum_lo=%d \
              lifetime_min=%d \
              lifetime_max=%d peak_ms=%d peak_ms_count=%d\n"
             (Analysis.messages state)
             (Analysis.decoded_messages state)
             (Analysis.timestamped_messages state)
             (Analysis.orphan_modifies state)
             (Analysis.live_orders state)
             (Analysis.terminated state Executed)
             (Analysis.terminated state Cancelled)
             (Analysis.terminated state Deleted)
             (Analysis.terminated state Replaced)
             (Histogram.total_high all)
             (Histogram.total_low all)
             (Histogram.min_value all)
             (Histogram.max_value all)
             !peak_ms
             !peak_count);
         (* The aggregate is one line and the report is a page; printing the
            aggregate first means a single pass over 12.95 GB yields both, and
            [head -1] still picks the machine-readable line out for the
            cross-check. A second pass costs fourteen minutes. *)
         (
         printf "\nfile            %s\n" path;
         printf "size            %d bytes\n" len;
         printf "consumed        %d bytes (%d unconsumed tail)\n" consumed (len - consumed);
         printf "messages        %d\n" (Analysis.messages state);
         printf "decoded         %d\n" (Analysis.decoded_messages state);
         printf
           "elapsed         %.3f s  (%.2f M msg/s, %.1f MB/s)\n"
           elapsed
           (Float.of_int (Analysis.messages state) /. elapsed /. 1e6)
           (Float.of_int consumed /. elapsed /. 1e6);
         let first = Analysis.first_timestamp state in
         let last = Analysis.last_timestamp state in
         if first >= 0
         then (
           printf
             "session         %s to %s\n"
             (Types.Timestamp.to_string_hum (Types.Timestamp.of_ns_since_midnight first))
             (Types.Timestamp.to_string_hum (Types.Timestamp.of_ns_since_midnight last)));
         printf
           "live table      %d of %d slots used at peak (%.1f%%)\n"
           (Analysis.max_live_orders state)
           (Analysis.table_capacity state)
           (Float.of_int (Analysis.max_live_orders state)
            /. Float.of_int (Analysis.table_capacity state)
            *. 100.);
         printf "orphan modifies %d (modify for an order never seen)\n" (Analysis.orphan_modifies state);
         printf "still resting   %d at end of file\n" (Analysis.live_orders state);
         let all = Analysis.all_lifetimes state in
         printf "\n%-10s %14s  %s\n" "cause" "orders" "share of terminations";
         let terminated_total = Histogram.count all in
         List.iter Analysis.Cause.all ~f:(fun cause ->
           let n = Analysis.terminated state cause in
           printf
             "%-10s %14d  %5.2f%%\n"
             (Analysis.Cause.to_string cause)
             n
             (if terminated_total = 0
              then 0.
              else Float.of_int n /. Float.of_int terminated_total *. 100.));
         print_lifetime_table "lifetime, all terminated orders" all;
         List.iter Analysis.Cause.all ~f:(fun cause ->
           print_lifetime_table
             (sprintf "lifetime, %s" (Analysis.Cause.to_string cause))
             (Analysis.lifetimes state cause));
         print_survival all;
         (* Message rate. Every millisecond of the session is counted, including
            the empty ones: leaving them out would divide by the busy
            milliseconds only and quietly inflate the average, which is the
            denominator the burst ratio is measured against. *)
         let rates = Histogram.create ~significant_bits:5 in
         let busiest_ms = ref (-1) in
         let busiest_count = ref (-1) in
         let empty_ms = ref 0 in
         Analysis.iter_milliseconds state ~f:(fun ~ms ~count ->
           Histogram.record rates count;
           if count = 0 then incr empty_ms;
           if count > !busiest_count
           then (
             busiest_count := count;
             busiest_ms := ms));
         let span = Histogram.count rates in
         if span > 0
         then (
           let mean = Histogram.mean rates in
           printf "\nmessage rate, per millisecond of the session\n";
           printf "  milliseconds in session   %d\n" span;
           printf "  empty milliseconds        %d (%.2f%%)\n" !empty_ms
             (Float.of_int !empty_ms /. Float.of_int span *. 100.);
           printf "  timestamped messages      %d\n" (Analysis.timestamped_messages state);
           printf "  mean                      %.1f msg/ms\n" mean;
           List.iter [ 50.; 90.; 99.; 99.9; 99.99 ] ~f:(fun p ->
             let index = Histogram.bucket_at_percentile rates p in
             printf
               "  p%-6g                   %d - %d msg/ms\n"
               p
               (Histogram.bucket_low rates index)
               (Histogram.bucket_high rates index));
           printf "  peak                      %d msg/ms\n" !busiest_count;
           printf
             "  peak at                   %s\n"
             (Types.Timestamp.to_string_hum
                (Types.Timestamp.of_ns_since_midnight (!busiest_ms * 1_000_000)));
           printf "  peak / mean               %.1fx\n" (Float.of_int !busiest_count /. mean));
         match ms_csv with
         | None -> ()
         | Some csv_path ->
           Out_channel.with_file csv_path ~f:(fun out ->
             Out_channel.output_string out "ms,count\n";
             Analysis.iter_milliseconds state ~f:(fun ~ms ~count ->
               Out_channel.output_string out (sprintf "%d,%d\n" ms count)));
           printf "\nwrote %s\n" csv_path)))
;;

let () =
  Command_unix.run
    (Command.group
       ~summary:"Nasdaq TotalView-ITCH 5.0 tools"
       [ "stats", stats_command
       ; "dump", dump_command
       ; "book", book_command
       ; "checksum", checksum_command
       ; "analyze", analyze_command
       ])
;;
