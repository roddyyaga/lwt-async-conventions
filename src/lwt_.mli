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

(** [peek t] returns:
    - [Some (Ok v)] if [t] is determined with value [v]
    - [Some (Error _)] if [t] is rejected 
    - [None] if [t] is sleeping *)
val peek : 'a t -> 'a Or_error.t option

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

module Mutex : sig
  include module type of Lwt_mutex

  val with_lock : t -> f:(unit -> 'a Lwt.t) -> 'a Lwt.t
end

module List : Monad_sequence.S with type 'a monad := 'a t and type 'a t := 'a list

module Result : sig
  include Monad.S2 with type ('a, 'b) t = ('a, 'b) Result.t Lwt.t (** @open *)

  val fail : 'err -> (_, 'err) t
  val failf : ('a, unit, string, (_, string) t) format4 -> 'a
  val map_error : ('ok, 'error1) t -> f:('error1 -> 'error2) -> ('ok, 'error2) t

  (** [combine] waits on both inputs and combines their results using [Result.combine]. *)
  val combine
    :  ('ok1, 'err) t
    -> ('ok2, 'err) t
    -> ok:('ok1 -> 'ok2 -> 'ok3)
    -> err:('err -> 'err -> 'err)
    -> ('ok3, 'err) t
end

module Or_error : sig
  type 'a t = 'a Or_error.t Lwt.t

  (** The applicative operations match the behavior of the applicative operations in
    [Or_error].  This means that [all] and [all_unit] are equivalent to [combine_errors]
    and [combine_errors_unit] respectively. *)
  include Applicative.S with type 'a t := 'a t

  (** [return x = Deferred.return (Ok x)] **)
  include Monad.S with type 'a t := 'a t

  (** [fail error = Deferred.return (Error error)] **)
  val fail : Error.t -> _ t

  (** These functions are direct analogs of the corresponding [Core.Or_error] functions. *)
  val ok_exn : 'a t -> 'a Lwt.t

  val of_exn : exn -> _ t
  val of_exn_result : ('a, exn) Caml.Result.t Lwt.t -> 'a t
  val error : string -> 'a -> ('a -> Sexp.t) -> _ t
  val error_s : Sexp.t -> _ t
  val error_string : string -> _ t
  val errorf : ('a, unit, string, _ t) format4 -> 'a
  val tag : 'a t -> tag:string -> 'a t
  val tag_s : 'a t -> tag:Sexp.t -> 'a t
  val tag_arg : 'a t -> string -> 'b -> ('b -> Sexp.t) -> 'a t
  val unimplemented : string -> _ t
  val combine_errors : 'a t list -> 'a list t
  val combine_errors_unit : unit t list -> unit t
  val filter_ok_at_least_one : 'a t list -> 'a list t

  (** [find_map_ok l ~f] returns the first value in [l] for which [f] returns [Ok],
    otherwise it returns the same error as [combine_errors (Lwt.List.map l ~f)]. *)
  val find_map_ok : 'a list -> f:('a -> 'b t) -> 'b t

  (** [ok_unit = return ()] *)
  val ok_unit : unit t

  (** [try_with f] catches exceptions thrown by [f] and returns them in the Result.t as an
    Error.t.  [try_with_join] is like [try_with], except that [f] can throw exceptions or
    return an [Error] directly, without ending up with a nested error; it is equivalent to
    [try_with f >>| Result.join]. *)
  val try_with : ?here:Lexing.position -> ?name:string -> (unit -> 'a Lwt.t) -> 'a t

  val try_with_join : ?here:Lexing.position -> ?name:string -> (unit -> 'a t) -> 'a t
end
