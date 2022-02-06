module Lwt = Lwt_

(* Deliberately don't include everything in the top level of [async_kernel.ml]
   based on my personal preference. *)
let don't_wait_for = Lwt.don't_wait_for
let ( >>> ) = Lwt.Infix.( >>> )

include Lwt.Let_syntax
