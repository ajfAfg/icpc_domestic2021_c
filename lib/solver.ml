module Vertex = struct
  type t = Id.t

  let compare = compare
  let hash = Hashtbl.hash
  let equal = ( = )
end

module IdSet = Set.Make (Id)
module IdMap = Map.Make (Id)
module G = Graph.Persistent.Graph.Concrete (Vertex)

let rec graph_labels_of_exp' graph labels id = function
  | Syntax.ILit i ->
      let graph = G.add_vertex graph id in
      let labels = IdMap.add id (Either.Left i) labels in
      (graph, labels)
  | Op (op, exps) ->
      let id_exp_list = List.map (fun exp -> (Id.create (), exp)) exps in
      let graph, labels =
        List.fold_left
          (fun (graph', labels') (id, exp) ->
            graph_labels_of_exp' graph' labels' id exp)
          (graph, labels) id_exp_list
      in
      let graph = G.add_vertex graph id in
      let graph =
        List.fold_left (fun graph' id' -> G.add_edge graph' id id') graph
        @@ List.map fst id_exp_list
      in
      let labels = IdMap.add id (Either.Right op) labels in
      (graph, labels)

let graph_labels_of_exp =
  graph_labels_of_exp' G.empty IdMap.empty @@ Id.create ()

let rec traverse memo parent_opt graph labels id =
  match parent_opt with
  | None -> traverse' memo parent_opt graph labels id
  | Some parent ->
      let key = (parent, id) in
      let v =
        match Hashtbl.find_opt memo key with
        | None -> traverse' memo parent_opt graph labels id
        | Some v -> v
      in
      Hashtbl.add memo key v;
      v

and traverse' memo parent_opt graph labels id =
  let children =
    match parent_opt with
    | None -> G.succ graph id
    | Some parent -> G.succ graph id |> List.filter (( <> ) parent)
  in
  match IdMap.find id labels with
  | Either.Left i -> (i, i)
  | Right Syntax.Plus ->
      children
      |> List.map (fun child -> traverse memo (Some id) graph labels child)
      |> List.fold_left
           (fun (max1, min1) (max2, min2) -> (max1 + max2, min1 + min2))
           (0, 0)
  | Right Minus ->
      let max_min_list =
        children
        |> List.map (fun child -> traverse memo (Some id) graph labels child)
      in
      let max =
        max_min_list
        |> List.map (fun (max, min) ->
               max_min_list |> List.map snd |> List.fold_left ( - ) max
               |> ( + ) min)
        |> List.fold_left max Int.min_int
      in
      let min =
        max_min_list
        |> List.map (fun (max, min) ->
               max_min_list |> List.map fst |> List.fold_left ( - ) min
               |> ( + ) max)
        |> List.fold_left min Int.max_int
      in
      (max, min)

let solve exp =
  let graph, labels = graph_labels_of_exp exp in
  let memo = Hashtbl.create @@ (2 * IdMap.cardinal labels) in
  G.fold_vertex
    (fun id max ->
      match IdMap.find id labels with
      | Either.Left _ -> max
      | Right _ -> Int.max max @@ fst @@ traverse memo None graph labels id)
    graph Int.min_int
