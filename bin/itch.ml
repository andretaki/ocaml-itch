open! Core
open Itch

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
         let start = Core_unix.gettimeofday () in
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
         let elapsed = Core_unix.gettimeofday () -. start in
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
         let start = Core_unix.gettimeofday () in
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
         let elapsed = Core_unix.gettimeofday () -. start in
         printf "replayed        %d bytes in %.3f s\n" consumed elapsed;
         printf "book messages   %d applied\n" (Order_book.applied books);
         printf
           "orphans         %d (modify for an order not seen)\n"
           (Order_book.orphans books);
         printf "live orders     %d\n" (Order_book.orders_live books);
         (match Order_book.check_invariants books with
          | Ok () -> printf "invariants      ok\n"
          | Error error -> printf !"invariants      FAILED: %{Error#hum}\n" error);
         if tops
         then
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
                 (field (Order_book.Book.best_ask book)));
         match symbol with
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
                 show Buy "bids (high to low)"))))
;;

let () =
  Command_unix.run
    (Command.group
       ~summary:"Nasdaq TotalView-ITCH 5.0 tools"
       [ "stats", stats_command; "dump", dump_command; "book", book_command ])
;;
