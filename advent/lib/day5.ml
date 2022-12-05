let stacks1 = [|
          ["G";"B";"D";"C";"P";"R"];
                      ["G";"V";"H"];
      ["M";"P";"J";"D";"Q";"S";"N"];
  ["M";"N";"C";"D";"G";"L";"S";"P"];
  ["S";"L";"F";"P";"C";"N";"B";"J"];
  ["S";"T";"G";"V";"Z";"D";"B";"Q"];
      ["Q";"T";"F";"H";"M";"Z";"B"];
              ["F";"B";"D";"M";"C"];
                  ["G";"Q";"C";"F"]
|]

let stacks2 = Array.copy stacks1

(*
let stacks = [|
  ["N";"Z"];
  ["D";"C";"M"];
  ["P"]
|]
let instructions = [(1,2,1);(3,1,3);(2,2,1);(1,1,2)]
*)

let parse_instruction s =
  match Utils.split_by_string " " s with
  | _::q::_::f::_::t::_ -> (int_of_string q, int_of_string f, int_of_string t)
  | _ -> failwith @@ "Wrong format " ^ s

let move_part1 stacks (q, f, t) =
  let taken, left = Utils.take q stacks.(f - 1) in
  let to_stack = stacks.(t - 1) in
  stacks.(f - 1) <- left;
  stacks.(t - 1) <- (List.rev taken)@to_stack

let move_part2 stacks (q, f, t) =
    let taken, left = Utils.take q stacks.(f - 1) in
    let to_stack = stacks.(t - 1) in
    stacks.(f - 1) <- left;
    stacks.(t - 1) <- taken@to_stack

let top_crates stacks () =
  let concat acc lst = acc ^ (List.hd lst) in
  Array.fold_left concat "" stacks

(*
let test =
  List.iter move instructions;
  top_crates()
*)

let generic algo stacks input =
  Utils.read_lines input
  |> List.map parse_instruction
  |> List.iter (algo stacks)
  |> top_crates stacks

let both_parts input =
  (generic move_part1 stacks1 input, generic move_part2 stacks2 input) 