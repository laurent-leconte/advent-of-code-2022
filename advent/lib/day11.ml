type monkey = {
  mutable items: int list;
  op: int -> int;
  test: int -> int;
  mutable inspected: int;
}

let monkeys = [|
  {items = [72; 64; 51; 57; 93; 97; 68];
  op = (fun x -> x*19);
  test = (fun x -> if x mod 17 = 0 then 4 else 7);
  inspected = 0
  };
  {items = [62];
  op = (fun x -> x*11);
  test = (fun x -> if x mod 3 = 0 then 3 else 2);
  inspected = 0
  };
  {items = [57; 94; 69; 79; 72];
  op = (fun x -> x + 6);
  test = (fun x -> if x mod 19 = 0 then 0 else 4);
  inspected = 0
  };
  {items = [80; 64; 92; 93; 64; 56];
  op = (fun x -> x + 5);
  test = (fun x -> if x mod 7 = 0 then 2 else 0);
  inspected = 0
  };
  {items = [70; 88; 95; 99; 78; 72; 65; 94];
  op = (fun x -> x + 7);
  test = (fun x -> if x mod 2 = 0 then 7 else 5);
  inspected = 0
  };
  {items = [57; 95; 81; 61];
  op = (fun x -> x * x);
  test = (fun x -> if x mod 5 = 0 then 1 else 6);
  inspected = 0
  };
  {items = [79; 99];
  op = (fun x -> x + 2);
  test = (fun x -> if x mod 11 = 0 then 3 else 1);
  inspected = 0
  };
  {items = [68; 98; 62];
  op = (fun x -> x + 3);
  test = (fun x -> if x mod 13 = 0 then 5 else 6);
  inspected = 0
  };
|]

let all_tests = 2*3*5*7*11*13*17*19

let one_round m part1 =
  let take_turn monkey =
    let handle_item item =
      let new_item = if part1 
          then (monkey.op item / 3)
          else ((monkey.op item) mod all_tests) in
      let new_idx = monkey.test new_item in
      m.(new_idx).items <- m.(new_idx).items@[new_item];
      monkey.inspected <- monkey.inspected + 1; in
    List.iter handle_item monkey.items;
    monkey.items <- []; in
  Array.iter take_turn m 

let copy_list l =
  let rec aux acc = function [] -> List.rev acc | hd::tl -> aux (hd::acc) tl in
  aux [] l

let copy m = {m with items = copy_list m.items;}

let generic rounds part1 =
  let new_monkeys = Array.map copy monkeys in
  for _ = 1 to rounds do one_round new_monkeys part1; done;
  let aux acc monkey = monkey.inspected::acc in
  let all_inspected = Array.fold_left aux [] new_monkeys in
  all_inspected |> List.sort compare |> List.rev |> fun l -> (List.hd l)*(List.hd (List.tl l))

let both_parts = (generic 20 true, generic 10000 false)