include Hashtbl


let find_or_add tbl key ~default =
  match find_opt tbl key with 
  | Some v -> v
  | None -> 
    let v = default () in
    add tbl key v ; v
