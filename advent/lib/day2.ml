type rps = Rock | Paper | Scissors
type outcome = Win |Loss | Draw

let score_rps = function
  | Rock -> 1
  | Paper -> 2
  | Scissors -> 3

let score_outcome = function
  | Win -> 6
  | Draw -> 3
  | Loss -> 0

let evaluate_hand = function
  | (Rock, Paper) | (Paper, Scissors) | (Scissors, Rock) -> Win
  | (Paper, Paper) | (Rock, Rock) | (Scissors, Scissors) -> Draw
  | (Paper, Rock) | (Rock, Scissors) | (Scissors, Paper) -> Loss

let strategy = function
  | (Rock, Win) | (Paper, Draw) | (Scissors, Loss) -> Paper
  | (Paper, Win) | (Scissors, Draw) | (Rock, Loss) -> Scissors 
  | (Scissors, Win) | (Rock, Draw) | (Paper, Loss) -> Rock 

let score_hand (opponent, you) = 
    (score_outcome @@ evaluate_hand (opponent, you)) + score_rps you

let rps_of_str = function
  | "A" | "X" ->  Rock
  | "B" | "Y" ->  Paper
  | "C" | "Z" ->  Scissors
  | x -> failwith @@ "Invalid letter " ^ x

let outcome_of_str = function
  | "X" ->  Loss
  | "Y" ->  Draw
  | "Z" ->  Win
  | x -> failwith @@ "Invalid outcome " ^ x

let tuple_of_list = function
  | a::b::_ -> (a, b)
  | _ -> failwith "not enough items in list"

let parse input =
    Utils.read_lines input
    |> List.map @@ Utils.split_by_string " "

let part1 processed =
  List.map (List.map rps_of_str) processed
  |> List.map tuple_of_list
  |>List.map score_hand
  |> Utils.sum

let part2 processed = 
  let aux = function
    | a::b::_ -> let opponent  = rps_of_str a in
                 let you = strategy (opponent, outcome_of_str b) in
                 score_hand(opponent, you)
    | _ -> failwith "whatever" in
  List.map aux processed
  |> Utils.sum

let both_parts input = 
  let processed = parse input in
  (part1 processed, part2 processed)

