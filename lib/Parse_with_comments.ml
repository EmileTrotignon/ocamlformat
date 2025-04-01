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

open Migrate_ast

type 'a with_comments =
  {ast: 'a; comments: Cmt.t list; prefix: string; source: Source.t}

module W = struct
  type t = int

  let in_lexer = [1; 2; 3; 14; 29]

  let disable x = -abs x

  let enable x = abs x

  let to_string x =
    String.concat ~sep:"" (List.map ~f:(Format.sprintf "%+d") x)
end

exception Warning50 of (Location.t * Warnings.t) list

let tokens lexbuf =
  let rec loop acc =
    match Lexer.token_with_comments lexbuf with
    (* The location in lexbuf are invalid for comments *)
    | COMMENT (_, loc) as tok -> loop ((tok, loc) :: acc)
    | DOCSTRING ds as tok -> loop ((tok, Docstrings.docstring_loc ds) :: acc)
    | tok -> (
        let loc = Location.of_lexbuf lexbuf in
        let acc = (tok, loc) :: acc in
        match tok with EOF -> List.rev acc | _ -> loop acc )
  in
  loop []

let fresh_lexbuf source =
  let lexbuf = Lexing.from_string source in
  Location.init_info lexbuf !Location.input_name ;
  let hash_bang =
    Lexer.skip_hash_bang lexbuf ;
    let len = lexbuf.lex_last_pos in
    String.sub source ~pos:0 ~len
  in
  (lexbuf, hash_bang)

let split_hash_bang source =
  let lexbuf = Lexing.from_string source in
  Location.init_info lexbuf !Location.input_name ;
  Lexer.skip_hash_bang lexbuf ;
  let len = lexbuf.lex_last_pos in
  let hash_bang = String.sub source ~pos:0 ~len in
  let rest = String.sub source ~pos:len ~len:(String.length source - len) in
  (rest, hash_bang)

let parse ?(disable_w50 = false) ?(disable_deprecated = false) parse fragment
    (conf : Conf.t) ~input_name ~source =
  let warnings =
    if conf.opr_opts.quiet.v then List.map ~f:W.disable W.in_lexer else []
  in
  let warnings = if disable_w50 then warnings else W.enable 50 :: warnings in
  ignore @@ Warnings.parse_options false (W.to_string warnings) ;
  let w50 = ref [] in
  let t =
    let source, hash_bang = split_hash_bang source in
    Warning.with_warning_filter
      ~filter_warning:(fun loc warn ->
        if
          Warning.is_unexpected_docstring warn
          && conf.opr_opts.comment_check.v
        then (
          w50 := (loc, warn) :: !w50 ;
          false )
        else not conf.opr_opts.quiet.v )
      ~filter_alert:(fun _loc alert ->
        if Warning.is_deprecated_alert alert && disable_deprecated then false
        else not conf.opr_opts.quiet.v )
      ~f:(fun () ->
        let ocaml_version = conf.opr_opts.ocaml_version.v in
        let ast = parse fragment ~ocaml_version ~input_name source in
        Warnings.check_fatal () ;
        let comments =
          let mk_cmt = function
            | `Comment txt, loc -> Cmt.create_comment txt loc
            | `Docstring txt, loc -> Cmt.create_docstring txt loc
          in
          List.map ~f:mk_cmt (Lexer.comments ())
        in
        let tokens =
          let lexbuf, _ = fresh_lexbuf source in
          tokens lexbuf
        in
        let source = Source.create ~text:source ~tokens in
        {ast; comments; prefix= hash_bang; source} )
  in
  match List.rev !w50 with [] -> t | w50 -> raise (Warning50 w50)

let beginend_mapper ~source =
  let rec expr ~skip_remove_beginend ~skip_add_beginend
      (m : Ast_mapper.mapper) (e : Parsetree.expression) =
      let lnum_start = e.pexp_loc.loc_start.pos_lnum and lnum_end = e.pexp_loc.loc_end.pos_lnum in
      let lnum_delta = lnum_end - lnum_start in
    let is_multiline =
      lnum_delta <> 0
    in
    let is_parenze = Source.is_parens source e.pexp_loc in
    let is_two_line_func =
      let loc_inside_parens = Source.loc_inside_parens source e.pexp_loc in
      let inner_lnum_delta= match loc_inside_parens with Some inner_loc ->  Some (inner_loc.loc_end.pos_lnum - inner_loc.loc_start.pos_lnum) | None -> None in
      match (e.pexp_desc, inner_lnum_delta) with
      | Pexp_function _, _
        when lnum_delta = 1
        ->
          true
      | Pexp_function _, Some 1
        when lnum_delta = 2 ->
          true
      | _ , Some 0-> true
      | _ -> false
    in
    let is_always_parenze =
      match e.pexp_desc with
      | Pexp_tuple _ | Pexp_coerce _ | Pexp_pack _ -> true
      | _ -> false
    in
    let add_beginend =
      is_multiline && is_parenze && (not is_always_parenze)
      && (not is_two_line_func) && not skip_add_beginend
    in
    let remove_beginend =
      (not skip_remove_beginend)
      && (not is_multiline)
      && List.is_empty e.pexp_attributes
    in
    let e =
      match e.pexp_desc with
      | Pexp_beginend e' when remove_beginend -> e'
      (* | Pexp_beginend ({pexp_desc= Pexp_beginend _; _} as e') -> e' *)
      | _ -> e
    in
    let sub_expr =
      let sub_skip_remove_beginend =
        match e.pexp_desc with
        (* we never want to remove a begin end node that has an extension. *)
        | Pexp_extension
            ( ext
            , PStr
                [ ( { pstr_desc=
                        Pstr_eval
                          (({pexp_desc= Pexp_beginend _; _} as e_beginend), _)
                    ; pstr_loc= _ } as _pld ) ] )
          when Source.extension_using_sugar ~name:ext
                 ~payload:e_beginend.pexp_loc ->
            true
        | _ -> false
      in
      let sub_skip_add_beginend =
        match e.pexp_desc with
        (* Prevent adding a begin end node twice in row. *)
        | _ when add_beginend -> true
        (* Here, we already removed the begin-end node if we had to, so this
           case can only be triggered if [remove_beginend] is false. *)
        | Pexp_beginend _ -> true
        (* Adding a begin-end node inside an extension node will change which expression the  *)
        (* | Pexp_extension (_, PStr [{pstr_desc= Pstr_eval _; _}]) -> true *)
        | _ -> false
      in
      { m with
        expr=
          expr ~skip_remove_beginend:sub_skip_remove_beginend
            ~skip_add_beginend:sub_skip_add_beginend }
    in
    let e = Ast_mapper.default_mapper.expr sub_expr e in
    if add_beginend then
      {e with pexp_desc= Pexp_beginend e; pexp_attributes= []}
    else e
  in
  { Ast_mapper.default_mapper with
    expr= expr ~skip_remove_beginend:false ~skip_add_beginend:false }

let parse_ast (conf : Conf.t) ~is_first_iter fg ~ocaml_version ~input_name s =
  let tokens =
    let lexbuf, _ = fresh_lexbuf s in
    tokens lexbuf
  in
  let source = Source.create ~text:s ~tokens in
  let preserve_beginend =
    match conf.fmt_opts.exp_grouping.v with
    | `Preserve  -> true
    | `Auto when is_first_iter -> false
    | `Auto -> true
    | `Parens  -> false
  in
  Extended_ast.Parse.ast fg ~ocaml_version ~preserve_beginend ~input_name s
  |>
  if Poly.(conf.fmt_opts.exp_grouping.v = `Auto) && not is_first_iter then
    Extended_ast.map fg (beginend_mapper ~source)
  else Fn.id

(** [is_repl_block x] returns whether [x] is a list of REPL phrases and
    outputs of the form:

    {v
    # let this is = some phrase;;
    this is some output
    v} *)
let is_repl_block x =
  String.length x >= 2 && Char.equal x.[0] '#' && Char.is_whitespace x.[1]

let parse_toplevel ?disable_w50 ?disable_deprecated ~is_first_iter (conf : Conf.t)
    ~input_name ~source =
  if is_repl_block source && conf.fmt_opts.parse_toplevel_phrases.v then
    Either.Second
      (parse ?disable_w50 ?disable_deprecated (parse_ast ~is_first_iter conf)
         Extended_ast.Repl_file conf ~input_name ~source )
  else
    First
      (parse ?disable_w50 ?disable_deprecated (parse_ast  ~is_first_iter conf)
         Extended_ast.Use_file conf ~input_name ~source )
