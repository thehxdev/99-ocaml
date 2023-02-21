open Base
open Stdio


let square x = x * x


let is_even x = x % 2 = 0


let rec range a b =
    if a = b then []
    else a :: range (a + 1) b


let average a b =
    (a +. b) /. 2.


let rec factorial = function
    | 0 | 1 -> 1
    | x -> x * factorial (x - 1)


let rec sumOfList (l: int list) : int =
    match l with
    | []  -> 0
    | [x] -> x
    | x :: rest -> x + sumOfList rest
    

let rec append (a: 'a list) (b: 'a list) : 'a list =
    match a with
    | [] -> b
    | x :: rest -> x :: append rest b


let rec appendNList (l: 'a list list) : 'a list =
    match l with
    | [] -> []
    | [x] -> x
    | x :: rest -> append x (appendNList rest)


let rec map (f: 'a -> 'b) (l: 'a list): 'b list =
    match l with
    | [] -> []
    | x :: rest -> f x :: map f rest


(* ======================================================= *)

(*
type 'a binaryTree =
    | Leaf
    | Node of 'a binaryTree * 'a * 'a binaryTree


let aTree = 
    Node (Node (Leaf, 1, Leaf), 2, Node (Node (Leaf, 3, Leaf), 4, Leaf))


let rec sumOfBinaryTree (t: 'a binaryTree) : int =
    match t with
    | Leaf -> 0
    | Node (l, c, r) -> sumOfBinaryTree l + c + sumOfBinaryTree r


let rec flipBinaryTree (t: 'a binaryTree) : 'a binaryTree =
    match t with
    | Leaf -> Leaf
    | Node (l, c, r) -> Node (flipBinaryTree l, c, flipBinaryTree r)
*)

(* ======================================================= *)


let rec last (l: 'a list) : 'a option =
    match l with
    | []  -> None
    | [x] -> Some x
    | _ :: rest -> last rest

let rec last_tow (l: 'a list) : ('a * 'a) option =
    match l with
    | []  -> None
    | [_] -> None
    | [x; y] -> Some (x, y)
    | _ :: rest -> last_tow rest


let rec listNth (l: 'a list) (x: int) : 'a option =
    match l with
    | [] -> raise (Failure "Given index not found!")
    | h :: rest -> if x = 0 then Some h else listNth rest (x - 1)


let rec lenOfList (l: 'a list) : int = 
    match l with
    | [] -> 0
    | _ :: rest -> 1 + lenOfList(rest)


let rec reverseList (l: 'a list) : 'a list =
    match l with
    | [] -> []
    | x :: rest -> reverseList rest @ [x]


let distance ((x1, y1): (float * float)) ((x2, y2): (float * float)) : float =
    Float.sqrt ( ((x1 -. x2) **. 2.) +. ((y1 -. y2) **. 2.))


let rec filter (func: 'a -> bool) (l: 'a list) : 'a list =
    match l with
    | [] -> []
    | x :: rest -> 
        if func x then x :: filter func rest
        else filter func rest


let rec sum (l: int list) : int =
    match l with
    | [] -> 0
    | x :: rest -> x + sum rest
