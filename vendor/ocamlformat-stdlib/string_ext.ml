include StdLabels.String

let (<>) s s' = not ( equal s s')

let sub_pos ~lo ~hi s =
  sub ~pos:lo ~len:(hi - lo) s

let is_empty str = equal str ""

let starts_with_whitespace s = (not (is_empty s)) && Char_ext.is_whitespace s.[0]

let ends_with_whitespace s =
  (not (is_empty s)) && Char_ext.is_whitespace s.[length s - 1]

let split_on_chars ~on str =
  let[@tail_mod_cons] rec loop lo hi =
    if hi = length str then [(sub_pos str ~lo ~hi)] else
      if List.exists  ((=) str.[hi]) on then 
 ((sub_pos str ~lo ~hi) :: loop hi hi) 
  else 
    loop lo (hi + 1)
  in loop  0 0



let is_empty s = equal s ""

let lstrip ?(drop=Char_ext.is_whitespace) s = 
  let len = length s in
  let rec loop i =
    if i = len then i else
    if drop s.[i] then 
      loop (i + 1)
  else i
in  
let lo = loop 0 in
sub_pos s ~lo ~hi:len

let rstrip ?(drop=Char_ext.is_whitespace) s = 
  let rec loop i =
    if i = 0 then 0 else
    if drop s.[i] then 
      loop (i - 1)
  else i + 1
in  
let len = loop (length s - 1) in
sub s ~pos:0 ~len

let strip ?(drop = Char_ext.is_whitespace) s = 
  s |> rstrip ~drop |> lstrip ~drop
let split_lines s = 
  s |> split_on_char ~sep:'\n'  |> List.map (rstrip ~drop:((=) '\r')) |> List.filter (Fun.negate is_empty)


let filter ~f str = 
  let buf = Buffer.create 16 in

  iter ~f:(fun c -> 
    if f c then Buffer.add_char buf c else () 
      ) str ; Buffer.contents buf
    

let chop_prefix ~prefix str = 
  if starts_with ~prefix str then
    Some (sub_pos ~lo:(String.length prefix) ~hi:(length str) str)
  else 
    None

  let chop_prefix_exn ~prefix str =
    match 
chop_prefix ~prefix str 
  with 
  | Some r -> r | None -> raise (Invalid_argument "chop_prefix_exn")


  
    let chop_suffix ~suffix str = 
      if ends_with ~suffix str 
        then
        Some (sub ~pos:(0) ~len:(length str - length suffix ) str)
      else 
        None



        let chop_suffix_exn ~suffix str =
          match 
      chop_suffix ~suffix str 
        with 
        | Some r -> r | None -> raise (Invalid_argument "chop_suffix_exn")
      

        let lfindi ?(pos = 0) t ~f =
          let n = length t in
          let rec loop i = if i = n then None else if f i t.[i] then Some i else loop (i + 1) in
          loop pos [@nontail]


          let wrap_sub_n t n ~name ~pos ~len ~on_error =
            if n < 0
            then invalid_arg (name ^ " expecting nonnegative argument")
            else (
              try sub t ~pos ~len with
              | _ -> on_error)
          
          let drop_prefix t n =
            wrap_sub_n ~name:"drop_prefix" t n ~pos:n ~len:(length t - n) ~on_error:""
          
          let drop_suffix t n =
            wrap_sub_n ~name:"drop_suffix" t n ~pos:0 ~len:(length t - n) ~on_error:""

let (=) s s' = ( equal s s')
