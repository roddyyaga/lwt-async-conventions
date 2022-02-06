(** [Monad_sequence.S] is a generic interface specifying functions that deal with a
    container and a monad.  It is specialized to the [Lwt] monad and used with
     [Lwt.List]. The [Monad_sequence.how] type specifies the parallelism of
    container iterators. *)

open! Base

type how =
  [ `Parallel
  | `Sequential
  ]
[@@deriving sexp_of]

module type S = sig
  type 'a monad
  type 'a t

  val fold : 'a t -> init:'b -> f:('b -> 'a -> 'b monad) -> 'b monad
  val find : 'a t -> f:('a -> bool monad) -> 'a option monad
  val exists : 'a t -> f:('a -> bool monad) -> bool monad
  val for_all : 'a t -> f:('a -> bool monad) -> bool monad
  val all : 'a monad t -> 'a t monad
  val all_unit : unit monad t -> unit monad

  (** {2 Deferred iterators}

      In the following, the default [how] is [`Sequential] *)

  val iter : ?how:how -> 'a t -> f:('a -> unit monad) -> unit monad
  val iteri : ?how:how -> 'a t -> f:(int -> 'a -> unit monad) -> unit monad
  val map : ?how:how -> 'a t -> f:('a -> 'b monad) -> 'b t monad
  val mapi : ?how:how -> 'a t -> f:(int -> 'a -> 'b monad) -> 'b t monad
  val filter : ?how:how -> 'a t -> f:('a -> bool monad) -> 'a t monad
  val filter_map : ?how:how -> 'a t -> f:('a -> 'b option monad) -> 'b t monad
end
