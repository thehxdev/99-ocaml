(*
open Stdio
*)
open Base


let even (x: int) : bool = x % 2 = 0;;


let rec range (bottom: int) (top: int) : int list =
    match bottom >= top with
    | true -> []
    | false -> bottom :: range (bottom + 1) top;;


let rec filter (f: 'a -> bool) (l: 'a list) : 'a list =
    match l with
    | [] -> []
    | x :: rest -> 
            match f x with
            | true -> x :: filter f rest
            | false -> filter f rest
;;


let rec map (f: 'a -> 'b) (l: 'a list) : 'b list =
    match l with
    | [] -> []
    | x :: rest -> f x :: map f rest
;;


(*
let () = Stdio.print_endline "Hello, World!"
*)
