module SS = Set.Make(String)

let all_different lst =
  let n = List.length lst in
  let set = List.fold_right SS.add lst SS.empty in
  SS.cardinal set = n 


let find_offset window_size lst =
  let window, tl = Utils.take window_size lst in
  let rec aux window offset = function
  | a::tl -> if all_different window 
             then 
               offset + window_size
             else 
               let new_window = (List.tl window)@[a] in
               aux new_window (offset + 1) tl
  | _ -> failwith "Start packet not found" in
  aux window 0 (tl@["."]) (* add a railguard char at the end of the input to cat*)

  let generic size input = 
    Utils.read_lines input
    |> List.hd
    |> Utils.explode
    |> find_offset size

  let both_parts input = (generic 4 input, generic 14 input)