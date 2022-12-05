let file = Arg.read_arg "inputs/day4.txt" |> Array.to_list

let parse_line l =
  let (s1 :: s2 :: _) = String.split_on_char ',' l in
  let (s1_start :: s1_end :: _) = String.split_on_char '-' s1 in
  let (s2_start :: s2_end :: _) = String.split_on_char '-' s2 in
  let r1 = int_of_string s1_start, int_of_string s1_end in
  let r2 = int_of_string s2_start, int_of_string s2_end in
  r1, r2
;;

(*
a1 ---------- a2 
   b1 --- b2 
*)
let contains_range a b =
  let a1, a2 = a in
  let b1, b2 = b in
  a1 <= b1 && b2 <= a2
;;

let print_range ((a1, a2), (b1, b2)) = Printf.printf "\n%d-%d, %d-%d" a1 a2 b1 b2
let fully_contains (a, b) = contains_range a b || contains_range b a

let pairs =
  List.map parse_line file
  |> List.map fully_contains
  |> List.map (fun a -> if a == true then 1 else 0)
  |> List.fold_left ( + ) 0
;;

let () = Printf.printf "\n Contains: %d" pairs

(* part 2 *)

(*
a1 --- a2 
   b1 --- b2 
*)
let range_overlap a b =
  let a1, a2 = a in
  let b1, _ = b in
  a1 <= b1 && b1 <= a2
;;

let overlaps (a, b) = range_overlap a b || range_overlap b a

let overlaps_n =
  List.map parse_line file
  |> List.map overlaps
  |> List.map (fun a -> if a == true then 1 else 0)
  |> List.fold_left ( + ) 0
;;

let () = Printf.printf "\n Overlaps: %d" overlaps_n
