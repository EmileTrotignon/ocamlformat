type t =
  { ocamlformat_conf: Ocamlformat.Conf.t
  ; enable_outside_detected_project: bool
  ; inplace: bool
  ; check: bool
  ; inputs: Ocamlformat.Conf.file list
  ; kind: Ocamlformat.Syntax.t option
  ; numeric: bool
  ; output: string option
  ; name: string option
  ; print_config: bool
  ; root: string option }

val build_config :
     enable_outside_detected_project:bool
  -> root:Ocamlformat_stdlib.Fpath.t option
  -> file:string
  -> is_stdin:bool
  -> (Ocamlformat.Conf.t, string) Result.t

val action :
     unit
  -> ( Ocamlformat.Conf.action Cmdliner.Cmd.eval_ok
     , Cmdliner.Cmd.eval_error )
     Result.t
