open Base
include Lwt

module T = struct
  type nonrec +'a t = 'a Lwt.t

  let return = Lwt.return
  let bind t ~f = Lwt.bind t f
  let map = `Custom (fun t ~f -> Lwt.map f t)
end

include T
include Monad.Make (T)
open Let_syntax

let upon = Lwt.on_success

(* We shadow [all] on-purpose here, since the default definition introduces a chain of
binds as long as the list. *)
let all = `Make_sure_to_define_all_elsewhere
let _ = all
let all = Lwt.all

module Infix = struct
  include Monad_infix

  let ( >>> ) = upon
end

open Infix

module Resolver = struct
  type -'a t = 'a Lwt.u

  let fill = Lwt.wakeup_later
  let fill_with_exn = Lwt.wakeup_later_exn
  let read = `Lwt_waiter_of_wakener_is_deprecated
  let equal = Caml.( = )
end

let create f =
  let t, resolver = wait () in
  f resolver;
  t
;;

let peek_exn t =
  match Lwt.state t with
  | Return a -> Some a
  | Fail exn -> raise exn
  | Sleep -> None
;;

let value_exn t =
  match Lwt.state t with
  | Return a -> a
  | Fail exn -> raise exn
  | Sleep -> failwith "Called Lwt_async_conventions.value_exn on a sleeping promise"
;;

let is_determined t =
  match Lwt.state t with
  | Return _ -> Some `Determined
  | Fail _ -> Some `Rejected
  | Sleep -> None
;;

let unit = Lwt.return_unit

let never () =
  let t, (_ : _ Resolver.t) = wait () in
  t
;;

let any = Lwt.choose
let any_unit = Lwt.choose
let don't_wait_for t = Lwt.async (fun () -> t)

let for_ start ~to_ ~do_ =
  let rec loop i =
    if i > to_
    then return ()
    else (
      let%bind () = do_ i in
      loop (i + 1))
  in
  loop start
;;

let repeat_until_finished state f =
  create (fun finished ->
      let rec loop state =
        f state
        >>> function
        | `Repeat state -> loop state
        | `Finished result -> Resolver.fill finished result
      in
      loop state)
;;

type never_returns = Nothing.t

let never_returns : never_returns -> 'a = function
  | _ -> .
;;

let forever state f =
  repeat_until_finished state (fun state ->
      let%map state = f state in
      `Repeat state)
  >>> never_returns
;;

let ok x = x >>| fun x -> Ok x
