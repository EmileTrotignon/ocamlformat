(**************************************************************************)
(*                                                                        *)
(*                              OCamlFormat                               *)
(*                                                                        *)
(*            Copyright (c) Facebook, Inc. and its affiliates.            *)
(*                                                                        *)
(*      This source code is licensed under the MIT license found in       *)
(*      the LICENSE file in the root directory of this source tree.       *)
(*                                                                        *)
(**************************************************************************)

val build_config :
     enable_outside_detected_project:bool
  -> root:Fpath.t option
  -> file:string
  -> is_stdin:bool
  -> (Ocamlformat.Conf.t, string) Result.t

val action :
     unit
  -> ( Ocamlformat.Conf.action Cmdliner.Cmd.eval_ok
     , Cmdliner.Cmd.eval_error )
     Result.t
