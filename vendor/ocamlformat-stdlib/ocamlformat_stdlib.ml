module Char = Char_ext
module Either = Either_ext
module Hashtbl = Hashtbl_ext
module Option = Option_ext
module Out_channel = Out_channel_ext 
module Fpath = Fpath_ext
module List = List_ext
module String = String_ext
module Warning = Warning
module Format = Stdlib.Format
module Filename = Stdlib.Filename

module Poly = Poly

let ( >> ) f g x = g (f x)

let impossible msg = failwith msg

let check f x =
  assert (
    ignore (f x) ;
    true ) ;
  x


let (=) = Int.equal

let compare = Int.compare
let ( % ) x y =
if y <= 0
then raise (
  Invalid_argument (Printf.sprintf 
    "%s %% %s in ocamolformat_stdlib.ml: modulus should be positive"
    (Int.to_string x)
    (Int.to_string y))
    );
let rval = x mod y in
if rval < 0 then rval + y else rval


let compare_lexicographic cmps x y =
  let rec loop = function
    | cmp :: cmps ->
      let res = cmp x y in
      if res = 0 then loop cmps else res
    | [] -> 0
  in
  loop cmps