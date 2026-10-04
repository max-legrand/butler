open Types

let generate_graph raw_services =
  let services = Array.of_list raw_services in
  let lookup = Hashtbl.create 16 in
  let* () =
    Array.to_seqi services
    |> Seq.fold_left
         (fun acc (index, service) ->
            let* () = acc in
            if Hashtbl.mem lookup service.name
            then Error (Printf.sprintf "duplicate service: %s" service.name)
            else (
              Hashtbl.add lookup service.name index;
              Ok ()))
         (Ok ())
  in
  let* nodes =
    Array.fold_right
      (fun service acc ->
         let* acc = acc in
         let* deps =
           List.fold_right
             (fun dependency acc ->
                let* acc = acc in
                match Hashtbl.find_opt lookup dependency with
                | Some index -> Ok (index :: acc)
                | None ->
                  Error
                    (Printf.sprintf
                       "unknown dependency for service %s: %s"
                       service.name
                       dependency))
             (Option.value service.dependencies ~default:[])
             (Ok [])
         in
         Ok ({ service; deps; dependents = []; state = Pending } :: acc))
      services
      (Ok [])
  in
  let nodes = Array.of_list nodes in
  Array.iteri
    (fun index node ->
       List.iter
         (fun dep -> nodes.(dep).dependents <- nodes.(dep).dependents @ [ index ])
         node.deps)
    nodes;
  Ok { nodes; lookup }
;;

(* Kahn's algorithm. Ties are broken by configuration position. *)
let topological_sort graph =
  let in_degree = Array.map (fun node -> List.length node.deps) graph.nodes in
  let queue = Queue.create () in
  Array.iteri (fun index degree -> if degree = 0 then Queue.add index queue) in_degree;
  let order = ref [] in
  while not (Queue.is_empty queue) do
    let index = Queue.pop queue in
    order := index :: !order;
    List.iter
      (fun dependent ->
         in_degree.(dependent) <- in_degree.(dependent) - 1;
         if in_degree.(dependent) = 0 then Queue.add dependent queue)
      graph.nodes.(index).dependents
  done;
  if List.length !order = Array.length graph.nodes
  then Ok (List.rev !order)
  else (
    let blocked =
      Array.to_list graph.nodes
      |> List.mapi (fun index node -> index, node)
      |> List.filter_map (fun (index, node) ->
        if in_degree.(index) > 0 then Some node.service.name else None)
      |> List.sort compare
    in
    Error ("dependency cycle: " ^ String.concat ", " blocked))
;;

type partition =
  { prerequisites : int list
  ; services : int list
  }

(* Nodes nothing depends on are long-running services; the rest are prerequisites. *)
let partition_helper graph =
  let indexed = Array.to_list graph.nodes |> List.mapi (fun index node -> index, node) in
  let leaves, prerequisites =
    List.partition (fun (_, node) -> node.dependents = []) indexed
  in
  { prerequisites = List.map fst prerequisites; services = List.map fst leaves }
;;
