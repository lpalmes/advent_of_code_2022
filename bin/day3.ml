module SChar = Set.Make (Char)

let file = Arg.read_arg "inputs/day3.txt" |> Array.to_list

let calculate_priority c =
  match c with
  | 'a' .. 'z' -> Char.code c - 96
  | 'A' .. 'Z' -> Char.code c - 64 + 26
  | _ -> assert false
;;

let partition_line l =
  let length = String.length l in
  String.sub l 0 (length / 2), String.sub l (length / 2) (length / 2)
;;

let get_common_item l =
  let sets = List.map (fun i -> SChar.add_seq (String.to_seq i) SChar.empty) l in
  let inner_set = List.fold_left SChar.inter (List.hd sets) sets in
  match (SChar.to_seq inner_set) () with
  | Seq.Cons (c, _) -> c
  | _ -> assert false
;;

let lines = List.map partition_line file

let value =
  lines
  |> List.map (fun (a, b) -> get_common_item [ a; b ])
  |> List.map calculate_priority
  |> List.fold_left ( + ) 0
;;

let () = print_endline (string_of_int value)

(* part 2 *)
(* utility function to map groups of three items, 
   fails if last items are not in group of three *)
let rec map_in_3 f = function
  | [] -> []
  | h1 :: h2 :: h3 :: l ->
    let r = f [ h1; h2; h3 ] in
    r :: map_in_3 f l
  | _ -> assert false
;;

let badges =
  file
  |> map_in_3 get_common_item
  |> List.map calculate_priority
  |> List.fold_left ( + ) 0
;;

let () = print_endline (string_of_int badges)
