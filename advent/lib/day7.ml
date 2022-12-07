type filesystem =
  | File of string * int
  | Folder of string * (string, filesystem) Hashtbl.t

type line = 
  | Cd of string
  | List_dir of string
  | List_file of string * int
  | Ls

let line_of_str line =
  match Utils.split_by_string " " line with
  | ["$";"ls"] -> Ls
  | ["$";"cd";dir] -> Cd(dir)
  | ["dir";name] -> List_dir(name)
  | [size;name] -> List_file(name, int_of_string size)
  | _ -> failwith "wrong command format"

let str_of_line = function
  | List_dir(name) -> "dir " ^ name
  | List_file(name, _) -> "file " ^ name
  | _ -> "Not implemented"

let str_of_path path name = String.concat "/" @@ List.rev (name::path)
let print_fs =
  let rec aux path = function
    | File(name, size) -> Printf.printf "F %s (%d)\n" (str_of_path path name) size
    | Folder(name, contents) -> let print_content _ v = aux (name::path) v in
                              Printf .printf "D %s/\n" (str_of_path path name);
                              Hashtbl.iter print_content contents in
  aux []

let rec fs_size acc = function
  | File(_, size) -> size, acc
  | Folder(_, contents) -> 
    let accu _ v (acc_sum, acc_lst) =
      let s, l = fs_size acc_lst v in
      (s + acc_sum, l) in
    let folder_size, folder_acc = Hashtbl.fold accu contents (0, acc) in
    folder_size, folder_size::folder_acc
                              
let add_node filesystem path line =
  let name, to_add = match line with
  | List_file(name, size) -> name, File(name, size)
  | List_dir(name) -> name, Folder(name, Hashtbl.create 10) 
  | _ -> failwith "Wrong line type" in
  let rec walk_tree fs path = 
    match fs with
    | File(_, _) -> failwith "Error walking the FS tree"
    | Folder(dir, content) -> (match path with
      | [] -> Hashtbl.add content name to_add; Folder(dir, content) (* found final subfolder; add content *)
      | a::tl -> let updated_subfolder = walk_tree (Hashtbl.find content a) tl in
                 Hashtbl.replace content a updated_subfolder; Folder(dir, content)) in
  walk_tree filesystem path

let rec parse fs path = function
  | Cd(dir)::tl -> 
      (match dir with
        | ".." -> parse fs (List.tl path) tl
        | dir_name -> parse fs (dir_name::path) tl)
  | Ls::tl -> parse fs path tl
  | node::tl -> let new_fs = add_node fs (List.rev path) node in
                parse new_fs path tl
  | [] -> fs

let filter_size filter size =
  if filter size then Some(size) else None

let build_file_sizes input =
  let root_fs = Folder("", Hashtbl.create 10) in
  let fs =  Utils.read_lines input
  |> List.map line_of_str
  |> List.tl
  |> parse root_fs [] in
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