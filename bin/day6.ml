let file = Arg.read_arg "inputs/day6.txt"

exception Found

let explode s =
  let rec exp i l = if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []
;;

let line = explode file.(0)

let rec elem x xs =
  match xs with
  | [] -> false
  | y :: ys -> x = y || elem x ys
;;

let has_dup list =
  let hash = Hashtbl.create (List.length list) in
  try
    List.iter
      (function
       | x -> if Hashtbl.mem hash x then raise Found else Hashtbl.add hash x true)
      list;
    false
  with
  | Found -> true
;;

let unique s = not (has_dup s)

let rec take n = function
  | [] -> failwith "not enough elements"
  | x :: xs -> if n = 1 then [ x ] else x :: take (n - 1) xs
;;

let rec find_marker s n = if unique (take n s) then n else 1 + find_marker (List.tl s) n
let marker = find_marker line 4
let () = Printf.printf "\nMarker at %d\n" marker

(* part 2 *)
let message = find_marker line 14
let () = Printf.printf "\nMessage at %d\n" message
