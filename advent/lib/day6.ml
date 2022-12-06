let all_different a b c d =
  a <> b && a <> c && a <> d && b <> c && b <> d && c <> d

let rec find_offset i = function
  | a::(b::c::d::_ as remainder) -> if all_different a b c d then i + 4 else find_offset (i+1) remainder
  | _ -> failwith "Start packet not found"

  let part1 input = 
    Utils.read_lines input
    |> List.hd
    |> Utils.explode
    |> find_offset 0

  let both_parts input = (part1 input, 0)