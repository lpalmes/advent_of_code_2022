module SChar = Set.Make (Char)

let file = Arg.read_arg "inputs/day3.txt"

let calculate_priority c =
  match c with
  | 'a' .. 'z' -> Char.code c - 96
  | 'A' .. 'Z' -> Char.code c - 64 + 26
  | _ -> assert false
;;

let parse_line l =
  let length = String.length l in
  String.sub l 0 (length / 2), String.sub l (length / 2) (length / 2)
;;

let get_common_item (a, b) =
  let set_a = SChar.add_seq (String.to_seq a) SChar.empty in
  let set_b = SChar.add_seq (String.to_seq b) SChar.empty in
  let inner_set = SChar.inter set_a set_b in
  match (SChar.to_seq inner_set) () with
  | Seq.Cons (c, _) -> c
  | _ -> assert false
;;

let value =
  Array.map parse_line file
  |> Array.map get_common_item
  |> Array.map calculate_priority
  |> Array.fold_left ( + ) 0
;;

let () = print_endline (string_of_int value)
