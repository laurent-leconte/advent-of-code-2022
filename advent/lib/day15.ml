module Point =
  struct
    type t = int*int
    let compare = compare
    let to_string (x,y) = Printf.sprintf "(%d,%d)" x y
    let neighbors (x,y) = [(x-1,y);(x+1,y);(x,y-1);(x,y+1)]

  end

module Points = Set.Make(Point)

let dist (x1, y1) (x2, y2) = abs (x1 - x2) + abs(y1 - y2)

(* Pn = 1 + 2*n*(n+1)*)
let intersection row (center, beacon) =
  let radius = dist center beacon in
  let xc,yc = center in
  let row_length = radius - abs(yc - row) in
  if row_length < 0
    then None
    else Some(xc - row_length, xc + row_length)

let str_of_segments l = "[" ^ (String.concat ";" (List.map Point.to_string l)) ^ "]"

let union segments (a, b) = 
  (* Printf.printf "Adding %d,%d to %s\n" a b (str_of_segments segments); *)
  let rec aux (a,b) = function
    | [] -> [(a,b)]
    | (c,d)::tl as l -> 
      if b < c - 1 then (a,b)::l
      else if b = c - 1 || b = c then aux (a, d) tl
      else if d < a then (c,d)::(aux (a,b)) tl
      else aux (min a c, max b d) tl in
  let rec merge = function
    | (a,b)::((c,d)::tl as rest) -> if b = c-1 then merge ((a,d)::tl) else (a,b)::(merge rest)
    | x -> x in
  merge @@ aux (a,b) segments

let parse_line s =
  let numbers = List.map int_of_string (Utils.split_by_string " " s) in
  match numbers with
  | [xs;ys;xb;yb] -> (xs, ys), (xb, yb)
  | _ -> failwith "wrong format"  
 
let rec size acc = function
  | [] -> acc
  | (a,b)::tl -> size (acc + b - a + 1) tl

let row_coverage row inputs =
  inputs
  |> List.map (intersection row)
  |> List.filter_map (fun x -> x)
  |> List.fold_left union []

let rec box start stop = function
  | [] -> []
  | (a,b)::tl -> 
    if a > stop then []
    else if b < start then box start stop tl
    (* a <= b, start <= b, a <= stop *)
    else (max a start, min b stop)::(box start stop tl)


let part1 input row = 
  let inputs  = Utils.read_lines input
  |> List.map parse_line in
  let count_all = size 0 (row_coverage row inputs) in
  let all_beacons = List.map snd inputs in
  let count_beacons = List.fold_right Points.add all_beacons Points.empty
  |> Points.filter (fun (_,y) -> y = row)
  |> Points.cardinal in
  string_of_int (count_all - count_beacons)

let part2 input start stop =
  let inputs  = Utils.read_lines input
  |> List.map parse_line in
  for row = start to stop+1 do
    (* Printf.printf "Starting row %d\n" row; *)
    let row_segments = box start stop (row_coverage row inputs) in
    if List.length row_segments > 1 then begin
      Printf.printf "Row %d: %s\n" row (str_of_segments row_segments);
      let col = snd (List.hd row_segments) + 1 in
      Printf.printf "Found hole at %d, %d (%d)\n" col row (col*stop + row);
    end
  done; "done"

let both_parts input = (part1 input 4000000, part2 input 0 4000000)