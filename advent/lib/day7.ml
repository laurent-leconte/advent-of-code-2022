type command = 
  | Cd of string 
  | Ls

type node_type = Dir | Fil

type node = {
  name: string;
  size: int;
  node_t: node_type;
  path: string list;
}
type filesystem =
  | File of string * int
  | Folder of string * (string, filesystem) Hashtbl.t

type line = 
  | Command of command 
  | Output of node

let command_of_str = function
  | ["ls"] -> Ls
  | "cd"::[a] -> Cd(a)
  | _ -> failwith "wrong command format"

let node_of_str = function
  | ["dir";dir] -> {name = dir;
                    size = 0;
                    node_t = Dir;
                    path = []}
  | [size;file] -> {name = file;
                   size = int_of_string(size);
                   node_t = Fil;
                   path = []}
  | _ -> failwith "wrong output format"

let str_of_path path name = String.concat "/" @@ List.rev (name::path)
let print_fs =
  let rec aux path = function
    | File(name, size) -> Printf.printf "F %s (%d)\n" (str_of_path path name) size
    | Folder(name, contents) -> let print_content _ v = aux (name::path) v in
                              Printf .printf "D %s/\n" (str_of_path path name);
                              Hashtbl.iter print_content contents in
  aux []

let rec fs_size acc = function
  | File(_, size) -> size, (Fil, size)::acc
  | Folder(_, contents) -> 
    let accu _ v (acc_sum, acc_lst) =
      let s, l = fs_size acc_lst v in
      (s + acc_sum, l) in
    let folder_size, folder_acc = Hashtbl.fold accu contents (0, acc) in
    folder_size, (Dir, folder_size)::folder_acc

let str_of_node {name; size; node_t; path} =
  let full_path = String.concat "/" (path@[name]) in
  match node_t with
  | Fil -> Printf.sprintf "(F) %s (%d)" full_path size
  | Dir  -> "(D) " ^ full_path
                              
let add_node filesystem node =
  (* print_endline "Current fs: ";
  print_fs "" filesystem;
  Printf.printf "Adding %s (%s)\n" node.name (str_of_node node); *)
  let name = node.name in
  let to_add = match node.node_t with
  | Fil -> File(name, node.size)
  | Dir -> Folder(name, Hashtbl.create 10) in
  let rec walk_tree fs path = 
    (* Printf.printf "In walk_tree, path=%s and FS=\n" (String.concat "/" path);
    print_fs "" fs; *)
    match fs with
    | File(_, _) -> failwith "Error walking the FS tree"
    | Folder(dir, content) -> (match path with
      | [] -> Hashtbl.add content name to_add; Folder(dir, content) (* found final subfolder; add content *)
      | a::tl -> let updated_subfolder = walk_tree (Hashtbl.find content a) tl in
                 Hashtbl.replace content a updated_subfolder; Folder(dir, content)) in
  walk_tree filesystem node.path  
                 
let line_of_str s =
  let items = Utils.split_by_string " " s in
  match List.hd items with
  | "$" -> Command(command_of_str @@ List.tl items)
  | _ -> Output(node_of_str items)


let rec list_files path acc = function
  | a::tl as l -> (match a with
                  | Output(f) -> let updated_f = {f with path=List.rev path} in
                                 list_files path (updated_f::acc) tl
                  | _ -> (acc, l))
  | [] -> (acc, [])

let rec parse path files = function
  | Command(Cd(dir))::tl -> (match dir with
                            | ".." -> parse (List.tl path) files tl
                            | dir_name -> parse (dir_name::path) files tl)
  | Command(Ls)::tl -> let new_files, rest = list_files path [] tl in
                       parse path (new_files@files) rest
  | Output(_)::_ -> failwith "Parse error"
  | [] -> files

let filter_size filter (node, size) =
  match node with
    | Fil -> None
    | Dir -> if filter size then Some(size) else None

let build_file_sizes input =
  let root_fs = Folder("", Hashtbl.create 10) in
  let fs =  Utils.read_lines input
  |> List.map line_of_str
  |> List.tl
  |> parse [] []
  |> List.rev
  |> List.fold_left add_node root_fs in
  fs_size [] fs

let part1 (_, sizes) =
  let filter x = x <= 100000 in
  sizes |> List.filter_map (filter_size filter) |> Utils.sum

  let part2 (tot, sizes) =
    let free = 70000000 - tot in
    let min_to_find = 30000000 - free in
    let filter x = x >= min_to_find in
    sizes |> List.filter_map (filter_size filter) |> List.sort compare |> List.hd



let both_parts input = 
  let x = build_file_sizes input in 
  (part1 x, part2 x)