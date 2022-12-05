let inter (a, b) (c, d) = (max a c, min b d)

let contained range1 range2 =
  let intersection = inter range1 range2 in
  (intersection = range1) || (intersection = range2)

let overlap range1 range2 =
  let x, y = inter range1 range2 in
  x <= y

let apply_to_pair f = function
  | a::b::[] -> f a b
  | _ -> failwith "Wrong number of elements"

let range_of_str s =
  match Utils.split_by_string "-" s with
  | a::b::[] -> (int_of_string a, int_of_string b)
  | _ -> failwith "Incorrect range format"

let generic f input =
  Utils.read_lines input
  |> List.map @@ Utils.split_by_string ","
  |> List.map @@ List.map range_of_str
  |> List.map @@ apply_to_pair f
  |> List.map (fun x -> if x then 1 else 0)
  |> Utils.sum

let both_parts input = (generic contained input, generic overlap input)