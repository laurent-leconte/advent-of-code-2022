let unflatten =
  let rec aux acc current_group = function
    | [] -> List.rev (List.rev current_group)::acc
    | hd::tl -> match String.length hd with
      | 0 -> aux ((List.rev current_group)::acc) [] tl
      | _ -> aux acc (int_of_string(hd)::current_group) tl in
  aux [] []

let process input = 
  Utils.read_lines input 
  |> unflatten
  |> List.map Utils.sum
  |> List.sort compare
  |> List.rev

let part1 input =
  process input |> List.hd

let part2 input =
  process input 
  |> Utils.take 3 
  |> fst
  |> Utils.sum

let both_parts input = (part1 input, part2 input)
