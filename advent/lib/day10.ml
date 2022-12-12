type op = Noop | Addx of int

let parse_line l =
  match Utils.split_by_string " " l with
  | ["noop"] -> Noop
  | ["addx";a] -> Addx(int_of_string(a))
  | _ -> failwith @@ "Unable to parse " ^ l

let rec simulate history = function
  | [] -> List.rev history
  | Noop::tl -> let ts, x = List.hd history in simulate ((ts+1, x)::history) tl
  | Addx(a)::tl -> let ts, x = List.hd history in
                   let new_history = (ts+2,x+a)::(ts+1,x)::history in
                   simulate new_history tl

let rec filter_history acc timestamps = function
  | [] -> List.rev acc
  | (ts,x)::tl -> (match timestamps with
                  | [] -> List.rev acc
                  | next::_ when next > ts -> filter_history acc timestamps tl
                  | next::tl_ts when next = ts -> filter_history ((ts,x)::acc) tl_ts tl
                  | _ -> failwith "Missing timestamp")

let part1 input =
  Utils.read_lines input
  |> List.map parse_line
  |> simulate [(1,1)]
  |> filter_history [] [20;60;100;140;180;220]
  |> List.map (fun (x,y) -> x*y)
  |> Utils.sum

let is_visible j x = j >= x-1 && j <= x+1

let rec draw screen = function
  | [] -> screen
  | (ts,x)::tl -> if ts > 240 then screen
                  else begin 
                    let i, j = (ts - 1)/40, (ts - 1) mod 40 in (* 0 <= i < 6, 0 <= y < 40 *)
                    if is_visible j x then screen.(i).(j) <- "#" else ();
                    draw screen tl
                  end
                  
let part2 input = 
  print_endline "";
  Utils.read_lines input
  |> List.map parse_line
  |> simulate [(1,1)]
  |> draw (Array.make_matrix 6 40 " ")
  |> Utils.print_matrix (fun x -> x); ""

  let both_parts input = (string_of_int (part1 input), part2 input)