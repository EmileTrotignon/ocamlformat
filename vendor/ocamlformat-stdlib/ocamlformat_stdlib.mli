module Char = Char_ext
module Either = Either_ext
module Hashtbl = Hashtbl_ext
module Option = Option_ext 
module Fpath = Fpath_ext
module List = List_ext
module String = String_ext
module Warning = Warning
module Format = Stdlib.Format
module Filename = Stdlib.Filename

module Poly = Poly

val ( >> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
(** Composition of functions: [(f >> g) x] is exactly equivalent to
    [g (f (x))]. Left associative. *)

val impossible : string -> _
(** Indicate why the call is expected to be impossible. *)

val check : ('a -> _) -> 'a -> 'a
(** Asserting identity: [check f x] asserts that [f x] does not raise and
    returns [x]. *)

val (=) : int -> int -> bool

val compare : int -> int -> int

val (%) : int -> int -> int

val compare_lexicographic : ('a -> 'a -> int) list -> 'a -> 'a -> int