include Cmdliner

(** existential package of a Term and a setter for a ref to receive the
    parsed value *)
type arg = Arg : 'a Term.t * ('a -> unit) -> arg

let args : arg list ref = ref []

let mk ~default arg =
  let var = ref default in
  let set x = var := x in
  args := Arg (arg, set) :: !args ;
  (fun () -> !var)

let parse info validate term =
  Cmd.eval_value (Cmd.v info (Term.(ret (const validate $ term))))
