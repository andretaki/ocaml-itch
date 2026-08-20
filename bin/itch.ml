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
         let first_timestamp = ref None in
         let last_timestamp = ref None in
         let start = Core_unix.gettimeofday () in
         let consumed =
           Reader.iter buf ~pos:0 ~len:(Bigstring.length buf) ~f:(fun message ->
             incr total;
             let index = Char.to_int (Message.message_type message) in
             counts.(index) <- counts.(index) + 1;
             let timestamp =
               match message with
               | System_event m -> Some m.timestamp
               | Stock_directory m -> Some m.timestamp
               | Add_order m -> Some m.timestamp
               | Unparsed _ -> None
             in
             match timestamp with
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
         printf "\n%-6s %12s  %s\n" "type" "count" "decoded";
         Array.iteri counts ~f:(fun index count ->
           if count > 0
           then (
             let c = Char.of_int_exn index in
             let decoded =
               match c with
               | 'S' | 'R' | 'A' | 'F' -> "yes"
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

let () =
  Command_unix.run
    (Command.group
       ~summary:"Nasdaq TotalView-ITCH 5.0 tools"
       [ "stats", stats_command; "dump", dump_command ])
;;
