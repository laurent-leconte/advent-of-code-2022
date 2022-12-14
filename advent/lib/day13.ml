type packet = Atom of int | List of packet list

let rec string_of_packet = function
  | Atom(a) -> string_of_int a
  | List(p_list) -> "[" ^ String.concat ","  (List.map string_of_packet p_list) ^ "]"

let print_parsed l =
  let str_of_tok = function
    | Str.Delim(d) -> "D_" ^ d
    | Str.Text(t) -> "T_" ^ t in
  Utils.print_list str_of_tok l

  (** takes a string, splits it and parses it as a packet *)
let parse s = 
  let rec parse_packet acc = function
    | [] -> acc, []
    | Str.Delim("[")::tl -> let packets, rest = parse_packet [] tl in 
                            parse_packet (List(packets)::acc) rest
    | Str.Delim(",")::tl -> parse_packet acc tl
    | Str.Text(n)::tl -> parse_packet (Atom(int_of_string n)::acc) tl
    | Str.Delim("]")::tl -> List.rev acc, tl
    | Str.Delim(d)::_ -> failwith @@ "Unexpected delim " ^ d in
  let tokens = Str.full_split (Str.regexp {|\]\|,\|\[|}) s in
  List.hd @@ fst (parse_packet [] tokens)

let rec compare_packets pa pb =
  match (pa, pb) with
    | Atom(a), Atom(b) -> compare a b
    | Atom(a), List(lb) -> compare_lists [Atom(a)] lb
    | List(la), Atom(b) -> compare_lists la [Atom(b)]
    | List(la), List(lb) -> compare_lists la lb
and compare_lists la lb =
  match (la, lb) with
    | [], [] -> 0
    | [], _ -> -1
    | _, [] -> 1
    | a::xs, b::ys -> let c = compare_packets a b in
                      if c = 0 then compare_lists xs ys else c

let apply_2 f = function | a::b::_ -> f a b | _ -> failwith "not enough arguments"
let count_results l = 
  Utils.print_int_list l;
  let f i x = if x = -1 then i+1 else 0 in
  List.mapi f l |> Utils.sum

let part1 input = 
  Utils.read_lines input 
  |> List.filter (fun x -> String.length x > 0)
  |> List.map parse  
  |> Utils.bundle 2
  |> List.map (apply_2 compare_packets)
  |> count_results
  |> string_of_int

let part2 input=
  let markers = List.map parse ["[[2]]";"[[6]]"] in
  let sorted = Utils.read_lines input 
  |> List.filter (fun x -> String.length x > 0)
  |> List.cons "[[2]]" |> List.cons "[[6]]"
  |> List.map parse  
  |> List.sort compare_packets in

  let index p = 
    let rec aux idx = function
      | [] -> failwith "not found"
      | x::xs -> if compare_packets p x = 0 then idx else aux (idx+1) xs in
    aux 1 sorted in
  List.map index markers
  |> apply_2 ( * ) |> string_of_int



let both_parts input = (part1 input, part2 input)

