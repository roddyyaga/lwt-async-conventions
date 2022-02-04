open Base

type +'a t = 'a Lwt.t

module Resolver : sig
  type -'a t = 'a Lwt.u

  val fill : 'a t -> 'a -> unit
  val fill_with_exn : 'a t -> exn -> unit
  val read : [ `Lwt_waiter_of_wakener_is_deprecated ]

  (** [equal t t'] is physical equality of [t] and [t']. *)
  val equal : 'a t -> 'a t -> bool
end

val wait : unit -> 'a t * 'a Resolver.t

(** [create f] calls [f i], where [i] is an empty ivar.  [create] returns a deferred that
    becomes determined when [f] fills [i]. *)
val create : ('a Resolver.t -> unit) -> 'a t

(** [upon t f] will run [f v] at some point after [t] becomes determined with value
[v]. *)
val upon : 'a t -> ('a -> unit) -> unit

(** [peek t] returns [Some v] iff [t] is determined with value [v].

It raises iff [t] is rejected (with the rejecting exception). *)
val peek_exn : 'a t -> 'a option

(** [value_exn t] returns [v] if [t] is determined with value [v], and raises
otherwise. *)
val value_exn : 'a t -> 'a

val is_determined : 'a t -> [ `Determined | `Rejected ] option

include Monad.S with type 'a t := 'a t

module Infix : sig
  include Monad.Infix with type 'a t := 'a t

  val ( >>> ) : 'a t -> ('a -> unit) -> unit
end

(** [unit] is a deferred that is always determined with value [()] *)
val unit : unit t

(** [never ()] returns a deferred that never becomes determined. *)
val never : unit -> _ t

(** [both t1 t2] becomes determined after both [t1] and [t2] become determined. *)
val both : 'a t -> 'b t -> ('a * 'b) t

(** [all ts] returns a deferred that becomes determined when every [t] in [t]s is
    determined.  The output is in the same order as the input. *)
val all : 'a t list -> 'a list t

(** Like [all], but ignores results of the component deferreds. *)
val all_unit : unit t list -> unit t

(** [any ts] returns a deferred that is determined when any of the underlying deferreds is
    determined. *)
val any : 'a t list -> 'a t

(** [any_unit] is like [any], but ignores results of the component deferreds. *)
val any_unit : unit t list -> unit t

(** [don't_wait_for t] ignores [t].  It is like [Fn.ignore], but is more constrained
    because it requires a [unit Deferred.t].

    Rather than [ignore (t : _ t)], do [don't_wait_for (Deferred.ignore_m t)].

    We chose to give [don't_wait_for] type [unit t] rather than [_ t] to catch errors
    where a value is accidentally ignored. *)
val don't_wait_for : unit t -> unit

(** [for_ start ~to_:stop ~do_:f] is the deferred analog of:

{[
for i = start to stop do
f i;
done ]} *)
val for_ : int -> to_:int -> do_:(int -> unit t) -> unit t

(** [repeat_until_finished initial_state f] repeatedly runs [f] until [f] returns
[`Finished].  The first call to [f] happens immediately when [repeat_until_finished]
is called. *)
val repeat_until_finished
  :  'state
  -> ('state -> [ `Repeat of 'state | `Finished of 'result ] t)
  -> 'result t

(** [forever initial_state f] repeatedly runs [f], supplying the state returned to the
next call to [f]. *)
val forever : 'state -> ('state -> 'state t) -> unit

(** Useful for lifting values from the [Deferred.t] monad to the [Result.t Deferred.t]
monad. *)
val ok : 'a t -> ('a, _) Result.t t
