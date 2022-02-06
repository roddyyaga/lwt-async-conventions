A wrapper [Lwt](https://ocsigen.org/lwt/latest/manual/manual) with the conventions of Jane Street libraries/[Async](https://opensource.janestreet.com/async/).

- Labelled arguments (`Lwt.map t ~f`)
- `Lwt_list`/`Lwt_result` as `Lwt.List` and `Lwt.Result` with signatures like the corresponding Async modules
- `Let_syntax` modules for `ppx_let`
- Some functions from `Async` like `repeat_until_finished`

The implementations are all either Lwt code or copied from Async code in cases where the semantics definitely make sense.
