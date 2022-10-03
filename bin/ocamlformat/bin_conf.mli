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
