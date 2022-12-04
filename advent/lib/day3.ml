module CharSet = Set.Make(Char)

let split_str s =
  let n = String.length s in
  (String.sub s 0 (n/2), String.sub s (n/2) (n/2))

let set_of_chars s =
  let set = CharSet.empty in
  List.fold_right CharSet.add (Utils.explode_to_chars s) set

let score_letter = function
  | 'a'..'z' as c -> Char.code(c) - Char.code('a') + 1
  | 'A'..'Z' as c -> Char.code(c) - Char.code('A') + 27
  | c -> failwith @@ "Invalide char " ^ (Char.escaped c)

let part1 input =
  Utils.read_lines input 
  |> List.map split_str
  |> List.map (Utils.map_tuple set_of_chars)
  |> List.map (fun (a, b) -> CharSet.inter a b)
  |> List.map CharSet.min_elt
  |> List.map score_letter
  |> Utils.sum

let part2 input =
  Utils.read_lines input
  |> List.map set_of_chars
  |> Utils.bundle 3
  |> List.map (Utils.fold (fun a b -> CharSet.inter a b))
  |> List.map CharSet.min_elt
  |> List.map score_letter
  |> Utils.sum

let both_parts input =
  (part1 input, part2 input)