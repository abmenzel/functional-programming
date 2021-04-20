// Question 1
type Heap<'a when 'a: equality> =
| EmptyHP
| HP of 'a * Heap<'a> * Heap<'a>

// Question 1.1

// Declare a value ex3 representing the binary tree shown in example 3
let ex3 = HP(1, HP(2, HP(3, EmptyHP, EmptyHP), HP(5, EmptyHP, EmptyHP)), HP(4, EmptyHP, EmptyHP))

// Write the type of the value ex3.
// The type is Heap<int> which is monomorphic as it only takes a type of int, and not multiple types.

// Declare a value empty
let empty = EmptyHP

exception HeapError of string

let isEmpty H =
    match H with
    | EmptyHP -> true
    | HP(_) -> false

let rec size H =
    match H with
    | EmptyHP -> 0
    | HP(_,l,r) -> 1 + (size l) + (size r)

let find h =
    match h with
    | HP(i,_,_) -> i
    | EmptyHP -> raise (HeapError "can't find on Empty Heap")

let rec chkHeapProperty h =
    match h with
    | HP(_, EmptyHP, EmptyHP) -> true
    | HP(i,l,r) ->
        if ((find l <= i) && (find r) <= i) then false else (chkHeapProperty l && chkHeapProperty r)
    | EmptyHP -> true

// Question 1.3

// Declare map function

let rec map f h =
    match h with
    | HP(i,l,r) -> HP(f i,map f l, map f r)
    | EmptyHP -> EmptyHP

let exfalse3 = map (fun e -> e*(-1)) ex3
let test1 = chkHeapProperty ex3
let test2 = chkHeapProperty exfalse3
