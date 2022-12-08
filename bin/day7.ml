let file =
  Arg.read_arg "inputs/day7.txt" |> Array.to_list |> List.map (String.split_on_char ' ')
;;

type content =
  | Dir of string
  | File of (int * string)
[@@deriving show]

type command =
  | Cd of string
  | Ls of content list
[@@deriving show]

type slist = string list [@@deriving show]

let parse_content = function
  | [ "dir"; dir ] -> Dir dir
  | [ size; file ] -> File (int_of_string size, file)
  | _ -> failwith "Unrecognized case"
;;

let rec parse_ls (lss : string list list) =
  match lss with
  | [] -> [], []
  | ("$" :: _) :: _ -> [], lss
  | l :: lss ->
    let content = parse_content l in
    let lines, rest = parse_ls lss in
    content :: lines, rest
;;

let parse_command = function
  | [] -> Cd "/", []
  | [ "$"; "cd"; p ] :: lss -> Cd p, lss
  | [ "$"; "ls" ] :: lss ->
    let c, lss = parse_ls lss in
    Ls c, lss
  | l :: _ -> failwith (String.concat " " l)
;;

let rec parse_lines lss =
  match parse_command lss with
  | v, [] -> v :: []
  | v, lss -> v :: parse_lines lss
;;

module SetFs = Set.Make (struct
  type t = content

  let compare = compare
end)

let path_add a b =
  match a with
  | "/" -> "/" ^ b
  | a -> a ^ "/" ^ b
;;

let rec take n = function
  | [] -> failwith "not enough elements"
  | x :: xs -> if n = 1 then [ x ] else x :: take (n - 1) xs
;;

let path_up p =
  let ps = String.split_on_char '/' p in
  let np = String.concat "/" (take (List.length ps - 1) ps) in
  np
;;

let handle_path a = function
  | "/" -> "/"
  | ".." -> path_up a
  | p -> path_add a p
;;

let rec handle_command fs p cs =
  match cs with
  | [] -> ()
  | c :: cs ->
    let next_path =
      match c with
      | Cd pth -> handle_path p pth
      | Ls cs ->
        List.iter (handle_content fs p) cs;
        p
    in
    handle_command fs next_path cs

and handle_content fs p c =
  let set =
    match Hashtbl.find_opt fs p with
    | Some e -> e
    | None -> SetFs.empty
  in
  Hashtbl.replace fs p (SetFs.add c set)
;;

let build_fs = handle_command

let rec calculate_dir_size fs ss path =
  let size =
    match Hashtbl.find_opt fs path with
    | None -> 0
    | Some cs -> SetFs.fold (fun c a -> calculate_content_size fs ss path c + a) cs 0
  in
  Hashtbl.replace ss path size;
  size

and calculate_content_size fs ss path = function
  | File (s, _) -> s
  | Dir d -> calculate_dir_size fs ss (handle_path path d)
;;

let commands = parse_lines file

let fs, fs_size =
  let fs = Hashtbl.create 1000 in
  build_fs fs "/" commands;
  let fs_size = Hashtbl.create 1000 in
  let _ = calculate_dir_size fs fs_size "/" in
  fs, fs_size
;;

let sum = Hashtbl.fold (fun _ v a -> (if v < 100_000 then v else 0) + a) fs_size 0
let () = Printf.printf "\n Sum: %d" sum

let rec dirs fs fs_size dir =
  let size = Hashtbl.find fs_size dir in
  let content = Hashtbl.find fs dir |> SetFs.to_seq |> List.of_seq in
  let sizes =
    List.filter_map
      (function
       | Dir d -> Some (dirs fs fs_size (handle_path dir d))
       | _ -> None)
      content
    |> List.concat
  in
  size :: sizes
;;

let total_size = 70_000_000
let unused_space = total_size - Hashtbl.find fs_size "/"
let dir_sizes = dirs fs fs_size "/"

let smallest =
  List.filter (fun i -> unused_space + i >= 30_000_000) dir_sizes
  |> List.fold_left min total_size
;;

let () = Printf.printf "\n Smallest: %d" smallest
