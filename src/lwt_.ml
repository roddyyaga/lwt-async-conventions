open Base

module Lwt_ = struct
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

  let peek t =
    match Lwt.state t with
    | Return a -> Some (Ok a)
    | Fail exn -> Some (Or_error.of_exn ~backtrace:`Get exn)
    | Sleep -> None
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
end

include Lwt_

module Mutex = struct
  include Lwt_mutex

  let with_lock t ~f = with_lock t f
end

module List_ = struct
  let fold t ~init ~f = Lwt_list.fold_left_s f init t

  let find t ~f =
    Lwt.catch
      (fun () -> Lwt_list.find_s f t >>| Option.return)
      (function[@warning "-3"]
        | Not_found -> return None
        | exn -> Lwt.fail exn)
  ;;

  let exists t ~f = Lwt_list.exists_s f t
  let for_all t ~f = Lwt_list.for_all_s f t
  let all = all
  let all_unit = all_unit

  let iter ?(how = `Sequential) t ~f =
    match how with
    | `Sequential -> Lwt_list.iter_s f t
    | `Parallel -> Lwt_list.iter_p f t
  ;;

  let iteri ?(how = `Sequential) t ~f =
    match how with
    | `Sequential -> Lwt_list.iteri_s f t
    | `Parallel -> Lwt_list.iteri_p f t
  ;;

  let map ?(how = `Sequential) t ~f =
    match how with
    | `Sequential -> Lwt_list.map_s f t
    | `Parallel -> Lwt_list.map_p f t
  ;;

  let mapi ?(how = `Sequential) t ~f =
    match how with
    | `Sequential -> Lwt_list.mapi_s f t
    | `Parallel -> Lwt_list.mapi_p f t
  ;;

  let filter ?(how = `Sequential) t ~f =
    match how with
    | `Sequential -> Lwt_list.filter_s f t
    | `Parallel -> Lwt_list.filter_p f t
  ;;

  let filter_map ?(how = `Sequential) t ~f =
    match how with
    | `Sequential -> Lwt_list.filter_map_s f t
    | `Parallel -> Lwt_list.filter_map_p f t
  ;;
end

module Result_ = struct
  module T = struct
    type ('a, 'error) t = ('a, 'error) Result.t Lwt.t
  end

  include T

  let combine t1 t2 ~ok ~err =
    let%map.Lwt_ t1 = t1
    and t2 = t2 in
    Result.combine t1 t2 ~ok ~err
  ;;

  include Monad.Make2 (struct
    include T

    let return a = return (Ok a)

    let bind t ~f =
      bind t ~f:(function
          | Ok a -> f a
          | Error _ as error -> Lwt_.return error)
    ;;

    let map t ~f = map t ~f:(fun r -> Result.map r ~f)
    let map = `Custom map
  end)

  let fail x = Lwt_.return (Error x)
  let failf format = Printf.ksprintf fail format
  let map_error t ~f = Lwt_.map t ~f:(fun r -> Result.map_error r ~f)
end

module Or_error = struct
  include (Result_ : Monad.S2 with type ('a, 'b) t := ('a, 'b) Result_.t)

  type 'a t = 'a Or_error.t Lwt.t

  include Applicative.Make (struct
    type nonrec 'a t = 'a t

    let return = return

    let apply f x =
      Result_.combine
        f
        x
        ~ok:(fun f x -> f x)
        ~err:(fun e1 e2 -> Error.of_list [ e1; e2 ])
    ;;

    let map = `Custom map
  end)

  module Let_syntax = struct
    let return = return

    include Monad_infix

    module Let_syntax = struct
      let return = return
      let map = map
      let bind = bind
      let both = both

      (* from Applicative.Make *)
      module Open_on_rhs = struct end
    end
  end

  open Let_syntax

  let fail error = Lwt_.return (Result.fail error)
  let ok_exn t = Lwt_.map t ~f:Or_error.ok_exn
  let of_exn exn = Lwt_.return (Or_error.of_exn exn)
  let of_exn_result t = Lwt_.map t ~f:Or_error.of_exn_result
  let error msg v sexp_of = Lwt_.return (Or_error.error msg v sexp_of)
  let error_s sexp = Lwt_.return (Or_error.error_s sexp)
  let error_string msg = Lwt_.return (Or_error.error_string msg)
  let errorf format = Printf.ksprintf error_string format
  let tag t ~tag = Lwt_.map t ~f:(Or_error.tag ~tag)
  let tag_s t ~tag = Lwt_.map t ~f:(Or_error.tag_s ~tag)

  let tag_arg t message a sexp_of_a =
    Lwt_.map t ~f:(fun t -> Or_error.tag_arg t message a sexp_of_a)
  ;;

  let unimplemented msg = Lwt_.return (Or_error.unimplemented msg)
  let combine_errors l = Lwt_.map (Lwt_.all l) ~f:Or_error.combine_errors
  let combine_errors_unit l = Lwt_.map (Lwt_.all l) ~f:Or_error.combine_errors_unit
  let filter_ok_at_least_one l = Lwt_.map (Lwt_.all l) ~f:Or_error.filter_ok_at_least_one

  let find_map_ok l ~f =
    Lwt_.repeat_until_finished (l, []) (fun (l, errors) ->
        match l with
        | [] ->
          let errors = Error.of_list (List.rev errors) in
          Lwt_.return (`Finished (Error errors))
        | hd :: tl ->
          Lwt_.map (f hd) ~f:(function
              | Error current_error -> `Repeat (tl, current_error :: errors)
              | Ok result -> `Finished (Ok result)))
  ;;

  let ok_unit = return ()

  let try_with ?here ?name (f : unit -> 'a Lwt.t) =
    Lwt.catch
      (fun () -> f () |> Lwt_.map ~f:Or_error.return)
      (fun exn ->
        let error = Or_error.of_exn ~backtrace:`Get exn in
        let error =
          match here with
          | Some lexing_position ->
            Or_error.tag_s error ~tag:(Source_code_position.sexp_of_t lexing_position)
          | None -> error
        in
        let error =
          match name with
          | Some name -> Or_error.tag error ~tag:name
          | None -> error
        in
        Lwt_.return error)
  ;;

  let try_with_join ?here ?name f = Lwt_.map (try_with ?here ?name f) ~f:Or_error.join
end

module List = List_
module Result = Result_
