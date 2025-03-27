open Ast_mapper_ctx

let rec mapper m e =
  let parenze = Source.string_atAst.parenze_exp (Ast.sub_exp ~ctx e) in
  let is_multiline =
    e.pexp_loc.loc_start.pos_lnum <> e.pexp_loc.loc_start.pos_lnum
  in
  let ctx = Ast.Exp e in
  let e =
    match e.pexp_desc with
    | Pexp_beginend e when not is_multiline -> e
    | _ -> e
  in
  let m = {m with Ast_mapper.expr= (fun m -> mapper m ctx)} in
  let e = Ast_mapper.E.map m e in
  if parenze && is_multiline then {e with pexp_desc= Pexp_beginend e} else e

let mapper = Ast_mapper.{default_mapper with expr= (fun m -> mapper m Top)}
