type t = int

let create =
  let id_ref = ref 0 in
  let create' () =
    let id = !id_ref in
    incr id_ref;
    id
  in
  create'

let compare = compare
