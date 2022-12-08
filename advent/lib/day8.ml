let add_visible mat n vis_mat coord_iterator =
  (* Printf.printf "** Starting new direction\n"; *)
  let max_so_far = ref (-1) in
  for i = 0 to (n-1) do
    let x, y = coord_iterator n i in
    (* Printf.printf "%d, %d\n" x y; *)
    let current = mat.(x).(y) in
    if current > !max_so_far then
      ((* Printf.printf "Marking %d %d as visible (val = %d, max_so_far = %d)\n" x y current !max_so_far; *)
      max_so_far := current;
      vis_mat.(x).(y) <- 1;) else ()
  done

let from_left j _ i = (i, j)
let from_right j n i = (n - 1 - i, j)
let from_top j _ i = (j, i)
let from_bottom j n i = (j, n - 1 - i)

let count_visible (mat, n, _) =
  let vis_mat = Array.make_matrix n n 0 in
  for k = 0 to (n-1) do
    (* Printf.printf "**** Starting row/column %d\n" k; *)
    let all_directions = [from_left k; from_right k; from_top k; from_bottom k] in
    List.iter (add_visible mat n vis_mat) all_directions
  done;
  let sum _ _ acc v = acc + v in
  Utils.foldij sum 0 vis_mat

let list_of_column mat n j =
  let acc = ref [] in
  for i = (n-1) downto 0 do
    acc := mat.(i).(j)::(!acc)
  done;
  !acc

let list_of_row mat n i =
  let acc = ref [] in
  for j = (n-1) downto 0 do
    acc := mat.(i).(j)::(!acc)
  done;
  !acc

let str_of_int_list l = "[" ^ String.concat ";" (List.map string_of_int l) ^ "]"

let seen_from height =
  let rec aux acc = function
    | [] -> acc
    | a::tl -> if a >= height then acc + 1 else aux (acc + 1) tl in
  aux 0

let scenic_score mat n i j = 
  let current_col = list_of_column mat n j in
  let current_row = list_of_row mat n i in
  let left, right = Utils.take j current_row in
  let up, down = Utils.take i current_col in
  [List.rev left; List.tl right; List.rev up; List.tl down]
  |> List.map (seen_from mat.(i).(j))
  |> List.fold_left ( * ) 1
  

let find_best_tree (mat, n, _) =
  let f i j prev_best _ = max prev_best (scenic_score mat n i j) in
  Utils.foldij f 0 mat

let part1 input =
  Utils.read_matrix input
  |> count_visible

let part2 input =
  Utils.read_matrix input
  |> find_best_tree

let both_parts input = (string_of_int(part1 input), string_of_int(part2 input))