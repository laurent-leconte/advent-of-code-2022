type direction = string * int

module Points = Set.Make(struct
                            type t = int*int
                            let compare = compare
                            end)
let add (x1, y1) (x2, y2) = (x1 + x2), (y1 + y2)

let sub (x1, y1) (x2, y2) = (x1 - x2), (y1 - y2)

let sign x = compare x 0

(** Takes the vector between H and T (i.e. H-T) and outputs the movement for T *)
let move_tail = function
  (* same row / column *)
  | 0,1 | 1,0 | 0,-1 | -1,0 | 0,0 -> 0,0
  | 0,2 -> 0,1
  | 2,0 -> 1,0
  | 0,-2 -> 0,-1
  | -2,0 -> -1,0
  (* diagonals *)
  | 1,1 | 1,-1 | -1,1 | -1,-1 -> 0,0
  | x,y -> (* at this stage abs(x) + abas(y) > 3 *) sign x, sign y

  let move_head (x,y) = function
    | "U" -> x, y+1
    | "D" -> x, y-1
    | "R" -> x+1, y
    | "L" -> x-1, y
    | _   -> failwith "unknown direction"

let str_of_point (x,y) = (string_of_int x) ^ "," ^ (string_of_int y)
let str_of_rope r = "[" ^ (String.concat ";" (List.map str_of_point r)) ^ "]"

let one_step rope dir =
  (* Printf.printf "Before: %s\n" (str_of_rope rope); *)
  let rec move_rope leader acc = function
    | follower::tl -> let delta = sub leader follower in
                let new_follower = add follower (move_tail delta) in
                move_rope new_follower (leader::acc) tl
    | [] -> List.rev (leader::acc), leader in
  let new_rope, new_tail = move_rope (move_head (List.hd rope) dir) [] (List.tl rope) in
  (* Printf.printf "After: %s\n" (str_of_rope new_rope); *)
  new_rope, new_tail

let rec all_movements points rope = function
  | (dir,s)::tl -> let new_rope, new_tail = one_step rope dir in
                   let new_points = Points.add new_tail points in
                   if s = 1 
                    then all_movements new_points new_rope tl
                    else all_movements new_points new_rope ((dir, s-1)::tl)
  | [] -> points

let parse_line l =
  match Utils.split_by_string " " l with
    | [dir;step] -> (dir, int_of_string step)
    | _ -> failwith "Wrong line format"

let generic rope_size input =
  let rec make_rope acc = function
    | 0 -> acc
    | n -> make_rope ((0,0)::acc) (n-1) in
  let rope = make_rope [] rope_size in
  let points = Points.singleton (0,0) in
  Utils.read_lines input
  |> List.map parse_line
  |> all_movements points rope
  |> Points.cardinal

let both_parts input = (string_of_int (generic 2 input), string_of_int (generic 10 input))