type direction = string * int

module Points = Set.Make(struct
                            type t = int*int
                            let compare = compare
                            end)
let add (x1, y1) (x2, y2) = (x1 + x2), (y1 + y2)

let sub (x1, y1) (x2, y2) = (x1 - x2), (y1 - y2)

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
  | 2,1 | 1,2 -> 1,1
  | 2,-1 | 1,-2 -> 1,-1
  | -2,1 | -1,2 -> -1,1
  | -2,-1 | -1,-2 -> -1,-1
  | x,y -> failwith @@ Printf.sprintf "Incorrect delta %d,%d" x y

  let move_head (x,y) = function
    | "U" -> x, y+1
    | "D" -> x, y-1
    | "R" -> x+1, y
    | "L" -> x-1, y
    | _   -> failwith "unknown direction"

let str_of_point (x,y) = (string_of_int x) ^ "," ^ (string_of_int y)

let one_step head tail dir =
  Printf.printf "Before: head=%s; tail=%s; dir=%s\n" (str_of_point head) (str_of_point tail) dir;
  let new_head = move_head head dir in
  let delta = sub new_head tail in
  Printf.printf "After moving head: head=%s; delta=%s\n" (str_of_point new_head) (str_of_point delta);
  let new_tail = add tail (move_tail delta) in
  Printf.printf "After moving tail: head=%s; tail=%s\n" (str_of_point new_head) (str_of_point new_tail);
  new_head, new_tail

let rec all_movements points head tail = function
  | (dir,s)::tl -> let new_head, new_tail = one_step head tail dir in
                   let new_points = Points.add new_tail points in
                   if s = 1 
                    then all_movements new_points new_head new_tail tl
                    else all_movements new_points new_head new_tail ((dir, s-1)::tl)
  | [] -> points

let parse_line l =
  match Utils.split_by_string " " l with
    | [dir;step] -> (dir, int_of_string step)
    | _ -> failwith "Wrong line format"

let part1 input =
  let head = 0,0 in
  let tail = 0,0 in
  let points = Points.singleton tail in
  Utils.read_lines input
  |> List.map parse_line
  |> all_movements points head tail
  |> Points.cardinal

let both_parts input = (string_of_int (part1 input), "")