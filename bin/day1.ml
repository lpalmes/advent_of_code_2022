let file = Arg.read_arg "inputs/day1.txt"
let list = ref []
let sum = ref 0

let loop v =
  match String.trim v with
  | "" -> list := !list @ [ !sum ]
  | v -> sum := !sum + int_of_string v
;;

let () = Array.iter loop file

let reverse_compare a b =
  match compare a b with
  | 0 -> 0
  | v -> v * -1
;;

let first_3 = function
  | h1 :: h2 :: h3 :: _ -> [ h1; h2; h3 ]
  | _ -> []
;;

let sum_values = List.fold_left ( + ) 0
let max = List.fold_left max 0 !list
let max_3 = List.sort reverse_compare !list |> first_3 |> sum_values
let () = print_endline (string_of_int max)
let () = print_endline (string_of_int max_3)
