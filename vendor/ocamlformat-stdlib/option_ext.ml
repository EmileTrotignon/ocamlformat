include Option

let value_map ~default ~f =
  function
  | None -> default
  | Some v -> f v

let map ~f o = map f o

let for_all ~f = function 
| Some v -> f v | None -> true

let exists ~f = function 
| Some v -> f v | None -> false

module Monad_infix = struct
  
  let (>>=) o f = Option.bind o f

  let (>>|) o f = map ~f o
end

module O = struct 
let (let+) o f = map ~f o
let (let*) o f = bind o f
end