let file = Arg.read_arg "inputs/day5.txt" |> Array.to_list

type crates = char option list [@@deriving show]
type chars = char list [@@deriving show]

let explode s =
  let rec exp i l = if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []
;;

(* utlitiy function to split list when condition is true *)
let rec split f l =
  match l with
  | [] -> [], []
  | h :: t ->
    if f h
    then [], t
    else (
      let l1, l2 = split f t in
      h :: l1, l2)
;;

let rec parse_crate = function
  | [] -> []
  | ' ' :: '1' .. '9' :: _ -> [] (* break recursion *)
  | '[' :: chr :: ']' :: l ->
    Printf.printf "\n%d %s" (List.length l) (show_chars l);
    (match l with
     | [] -> []
     | _ :: l -> Some chr :: parse_crate l)
  | ' ' :: ' ' :: ' ' :: l ->
    (match l with
     | [] -> []
     | _ :: l -> None :: parse_crate l)
  | l -> failwith (show_chars l)
;;

let parse_line l =
  let chars = explode l in
  parse_crate chars
;;

let drawing, moves = split (fun l -> String.length l == 0) file
let () = List.iter print_endline drawing

let () =
  List.map parse_line drawing |> List.iter (fun a -> a |> show_crates |> print_endline)
;;
