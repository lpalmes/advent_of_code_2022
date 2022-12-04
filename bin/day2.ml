let file = Arg.read_arg "inputs/day2.txt"

type move =
  | Rock
  | Paper
  | Scissors

type winner =
  | Left
  | Draw
  | Right

let shape_score = function
  | Rock -> 1
  | Paper -> 2
  | Scissors -> 3
;;

let parse_move = function
  | "A" | "X" -> Rock
  | "B" | "Y" -> Paper
  | "C" | "Z" -> Scissors
  | _ -> assert false (* ocaml like invariant *)
;;

let calculate_move a b =
  match b with
  | "Y" -> a
  | "X" ->
    (match a with
     | Rock -> Scissors
     | Paper -> Rock
     | Scissors -> Paper)
  | "Z" ->
    (match a with
     | Scissors -> Rock
     | Rock -> Paper
     | Paper -> Scissors)
  | _ -> assert false
;;

let round_winner a b =
  match a, b with
  | Rock, Scissors | Scissors, Paper | Paper, Rock -> Left
  | Scissors, Rock | Paper, Scissors | Rock, Paper -> Right
  | a, b when a = b -> Draw
  | _ -> assert false
;;

let round_score (a, b) =
  let value =
    match round_winner a b with
    | Left -> 0
    | Draw -> 3
    | Right -> 6
  in
  value + shape_score b
;;

let parse_line l =
  match String.split_on_char ' ' l with
  | [ left; right ] -> parse_move left, parse_move right
  | _ -> raise (Invalid_argument "Invalid input")
;;

let parse_line_2 l =
  match String.split_on_char ' ' l with
  | [ left; right ] ->
    let left_move = parse_move left in
    left_move, calculate_move left_move right
  | _ -> raise (Invalid_argument "Invalid input")
;;

let sum_array = Array.fold_left ( + ) 0
let sum = Array.map parse_line file |> Array.map round_score |> sum_array
let sum2 = Array.map parse_line_2 file |> Array.map round_score |> sum_array
let () = print_endline (string_of_int sum)
let () = print_endline (string_of_int sum2)
