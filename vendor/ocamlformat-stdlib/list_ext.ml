include StdLabels.List

let partition_map l ~f =
  let fst, snd =
    fold_left l
      ~f:(fun (fst, snd) x ->
        match f x with
        | Either.Left x' -> (x' :: fst, snd)
        | Either.Right x' -> (fst, x' :: snd))
      ~init:([], [])
  in
  (rev fst, rev snd)

let max_elt ~compare li = 

  let rec loop candidate li =
match li with
| [] -> candidate
| elt :: li ->

  let candidate = if compare elt candidate > 0 then elt else candidate in
  loop candidate li
  in
 match li with 
 | [] -> None
 | elt :: li -> Some ( loop elt li)

let rec drop_while ~f = 
  function [] -> []
  | elt :: li ->
    if f elt then drop_while ~f li else elt :: li

let is_empty = function [] ->true | _ -> false

let rec last = function [] -> None | [elt] -> Some elt | _ :: li -> last li

let last_exn li = match last li with None -> raise (Invalid_argument "last_exn []") | Some elt -> elt

let split_n t_orig n =
  if n <= 0
  then [], t_orig
  else (
    let rec loop n t accum =
      if n = 0
      then rev accum, t
      else (
        match t with
        | [] -> t_orig, [] (* in this case, t_orig = rev accum *)
        | hd :: tl -> loop (n - 1) tl (hd :: accum))
    in
    loop n t_orig [])

    let remove_consecutive_duplicates ?(which_to_keep = `Last) list ~equal =
      let rec loop to_keep accum = function
        | [] -> to_keep :: accum
        | hd :: tl ->
          if equal hd to_keep
          then (
            let to_keep =
              match which_to_keep with
              | `First -> to_keep
              | `Last -> hd
            in
            loop to_keep accum tl)
          else loop hd (to_keep :: accum) tl
      in
      match list with
      | [] -> []
      | hd :: tl -> rev (loop hd [] tl)

      let groupi l ~break =
        let groups =
          foldi l ~init:[] ~f:(fun i acc x ->
            match acc with
            | [] -> [ [ x ] ]
            | current_group :: tl ->
              if break i (hd current_group) x
              then [ x ] :: current_group :: tl (* start new group *)
              else (x :: current_group) :: tl)
          (* extend current group *)
        in
        match groups with
        | [] -> []
        | l -> rev_map l ~f:rev
      ;;
      
      let group l ~break = groupi l ~break:(fun _ x y -> break x y) [@nontail]

let rec zip li li' =
  match li, li' with
  | [], [] -> Some []
  | [], _ | _,[] -> Some []
  | elt :: li , elt' :: li' ->
  let open Option.O in
  let+ li = zip li li' in
    (elt, elt') :: li