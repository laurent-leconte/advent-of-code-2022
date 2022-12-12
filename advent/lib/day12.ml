type node = int * int
(* an edge is a weight and a destination node *)
type edge = int * node

module Nodes = Set.Make(struct
                            type t = int*int
                            let compare = compare
                        end)

let graph_of_matrix dist (mat, m, n) =
    let within_bounds (i,j) = (i >= 0) && (i < m) && (j >= 0) && (j < n) in
    let neighbors (i,j) = [(i-1,j); (i,j-1); (i,j+1); (i+1,j)] |> List.filter within_bounds in
    let add_node i j g mij =
      let distance x y = dist mij mat.(x).(y) in
      let neighbors = neighbors (i,j) in
      let edges = List.map (fun (x,y) -> (distance x y, (x,y))) neighbors
      |> List.filter (fun (x, _) -> x = 1) in
      Hashtbl.add g (i,j) edges; g in
    let graph = Hashtbl.create (m*n) in 
    Utils.foldij add_node graph mat

let find_points (mat, m, n) =
  let entrance = ref (-1, -1) in
  let exit = ref (-1,-1) in
  for i = 0 to m-1 do
    for j = 0 to n-1 do
      if mat.(i).(j) = 'S'
        then (mat.(i).(j) <- 'a'; entrance := (i, j);)
    else if mat.(i).(j) = 'E'
      then (mat.(i).(j) <- 'z'; exit := (i, j);)
    done
  done;
  (!entrance, !exit)


let dijkstra start graph = 
  let compare_second (_, d1) (_, d2) = compare d1 d2 in
  let update t (n, d) = Hashtbl.add t n d; t in
  (* takes a Hashtbl of distances so far, a set of visited nodes, and a list of (node, distance) points to visit *)
  let rec loop distances visited = function
    | [] -> distances
    | (a,d)::tl ->
      (* if current node was already visited, skip it *)
      if Nodes.mem a visited then loop distances visited tl else
      (* mark current node as visited *)
      let new_visited = Nodes.add a visited in
      let not_visited (_, n) = Bool.not @@ Nodes.mem n visited in
      (* get all non-visited neighbors *)
      let eligible_edges = Hashtbl.find graph a |> List.filter not_visited in 
      (* for each neighbor, the new distance is the min between the previous distance and the new path *)
      let new_distance (w,n) =
        let tentative_dist = match Hashtbl.find_opt distances n with
          | None -> d + w
          | Some(previous) -> min previous (d + w) in
        (n, tentative_dist) in
      (* build a list of all enighbors with updated distances *)
      let nodes_and_distances = List.map new_distance eligible_edges in
      (* update the Hashtbl of distances with the new values *)
      let new_distances = List.fold_left update distances nodes_and_distances in
      (* add the neighbors to the list of nodes to visit and sort by distance *)
      let to_visit = List.sort compare_second (nodes_and_distances@tl) in
      loop new_distances new_visited to_visit in
  let initial_distances = Hashtbl.create 1 in
  let empty = Nodes.empty in
  loop initial_distances empty [(start,0)]


let part1 input =
  let mat = Utils.read_char_matrix input in
  let entrance, exit = find_points mat in
  let dist a b = if Char.code b - Char.code a <=1 then 1 else 2 in
  let distances = graph_of_matrix dist mat |> dijkstra entrance in
  Hashtbl.find distances exit

let part2 input =
  (* for part2, build the graph with the edges going in the other direction and start from the exit *)
  let (arr, _, _ as mat) = Utils.read_char_matrix input in
  let _, exit = find_points mat in
  let dist a b = if Char.code a - Char.code b <= 1 then 1 else 2 in (* inverted distance *)
  let distances = graph_of_matrix dist mat |> dijkstra exit in
  print_endline "Distances are ok";
  let find_lowest i j min_so_far mij =
    if mij = 'a' && Hashtbl.mem distances (i,j) then
      (let this_distance = Hashtbl.find distances (i, j) in
      if this_distance < min_so_far then this_distance else min_so_far)
    else min_so_far in
  Utils.foldij find_lowest Int.max_int arr

let both_parts input = (part1 input, part2 input)

(* 
let distances = "inputs/day15.dat" |> Utils.read_matrix |> tile 5 |> graph_of_matrix |> dijkstra (0,0)
let () = print_endline @@ string_of_int @@ Hashtbl.find distances (499,499) *)