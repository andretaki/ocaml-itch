open! Core

(** A live-order table: order reference -> (arrival timestamp, resting shares).

    This exists because {!Order_book} cannot answer the question Level 4 asks.
    The book tracks shares at a price level; it deliberately forgets when an
    order arrived, because a book does not need to know. Order lifetime does.

    It is a hand-written open-addressing table rather than a [Core.Hashtbl] for
    one reason: every operation here runs inside a {!Handler.S} callback, and
    those callbacks carry [@@zero_alloc]. A [Hashtbl] allocates on insert, so
    under OxCaml a handler built on one does not compile. Storage is three
    preallocated [int array]s -- OCaml ints are immediates, so reading and
    writing them allocates nothing and needs no write barrier.

    Capacity is fixed at creation. Growing would mean allocating a larger array
    mid-stream, which is exactly what the zero-alloc contract forbids, so
    {!insert} raises rather than silently degrading into a full-table spin. Size
    the table with {!create} and check {!max_occupancy} afterwards. *)

(* The [@@zero_alloc] annotations below are the point of this module, and they
   belong in the interface rather than only on the definitions: the checker
   reasons per compilation unit, so a caller in another module can only rely on
   a property the signature states. This is the same lesson the handler
   signature already records -- the contract goes on the signature, not on the
   application. *)

type t

(** [create ~capacity_log2] allocates a table of [2 ** capacity_log2] slots,
    using three int arrays of that length -- 24 bytes per slot on a 64-bit
    machine. {!insert} refuses past seven eighths of that, so the usable
    capacity is [7 * 2 ** capacity_log2 / 8]. Raises if [capacity_log2] is not
    in [1, 40]. *)
val create : capacity_log2:int -> t

val capacity : t -> int
val length : t -> int

(** The high-water mark of {!length}, which is the number this table is really
    sized against. Peak live orders is not knowable ahead of a run. *)
val max_occupancy : t -> int

(** [slot t ~key] is the index of the slot holding [key], or [-1].

    Returning an index rather than the payload is what keeps the caller
    allocation-free: an [option] or a tuple would be a heap block per message.
    The index also lets a caller that finds an order then update or delete it
    without probing a second time. *)
val slot : t -> key:int -> int
[@@zero_alloc]

val timestamp_at : t -> int -> int [@@zero_alloc]
val shares_at : t -> int -> int [@@zero_alloc]
val set_shares_at : t -> int -> int -> unit [@@zero_alloc]

(** Raises if [key] is negative, if [key] is already present, or if the table has
    reached seven eighths of its capacity. It stops short of full because a
    linear-probing table degrades badly near capacity, and the message names the
    capacity to rerun with. *)
val insert : t -> key:int -> timestamp:int -> shares:int -> unit
[@@zero_alloc]

(** Removes the entry at [index], which must have come from {!slot}.

    Any index previously returned by {!slot} may be invalidated: deletion from a
    linear-probing table has to shift later entries backwards to keep probe
    chains unbroken, so entries move. Call {!slot} again rather than reusing an
    index across a delete. *)
val delete_at : t -> int -> unit
[@@zero_alloc]

(** Folds over live entries, for end-of-run accounting of orders that never
    died. Allocates, so it is not for the message path. *)
val iter : t -> f:(key:int -> timestamp:int -> shares:int -> unit) -> unit
