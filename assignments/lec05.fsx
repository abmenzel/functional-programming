type 'a BinTree =
    Leaf
    | Node of 'a * 'a BinTree * 'a BinTree

let intBinTree =
    Node(43, Node(25, Node(56,Leaf, Leaf), Leaf),
        Node(562, Leaf, Node(78, Leaf, Leaf)))

// 5.1
let rec inOrder tree =
    match tree with
    | Leaf -> []
    | Node(n,treeL,treeR) -> inOrder treeL @ [n] @ inOrder treeR

//inOrder intBinTree

// 5.2
let rec mapInOrder f tree =
    match tree with
    | Leaf -> Leaf
    | Node (n, treeL, treeR) ->
        let treeL2 = mapInOrder f treeL
        Node(f n, treeL2, mapInOrder f treeR)

// mapInOrder and mapPostOrder will give different ordered results, as they map over the values in a different order.
// E.g. mapInOrder (fun x -> printfn "%i" x; x+1) intBinTree;; results in:
// mapInOrder: 57, 26, 44, 563, 79
// mapPostOrder: 57, 26, 79, 563, 44

// 5.3
let rec foldInOrder f x tree =
    match tree with
    | Leaf -> x
    | Node(treeL, node, treeR) ->
        foldInOrder f x node
        |> f treeL
        |> fun x -> foldInOrder f x treeR

let floatBinTree =
    Node(43.0,Node(25.0, Node(56.0,Leaf, Leaf), Leaf),
    Node(562.0, Leaf, Node(78.0, Leaf,Leaf)))

foldInOrder (fun n a -> a + n) 0.0 floatBinTree


//5.4 + 5.5
type aExp =                     (* Arithmetical expressions *)
    | N of int                  (* numbers *)
    | V of string               (* variables *)
    | Add of aExp * aExp        (* addition *)
    | Mul of aExp * aExp        (* multiplication *)
    | Sub of aExp * aExp        (* subtraction *)
    | Inc of string

type bExp =                     (* Boolean expressions *)
    | TT                        (* true *)
    | FF                        (* false *)
    | Eq of aExp * aExp         (* equality *)
    | Lt of aExp * aExp         (* less than *)
    | Neg of bExp               (* negation *)
    | Con of bExp * bExp        (* conjunction *)

type stm =                      // statements
    | Ass of string * aExp      // assignment
    | Skip
    | Seq of stm * stm          // sequential composition
    | ITE of bExp * stm * stm   // if-then-else
    | While of bExp * stm       // while
    | IT of bExp * stm          // if-then
    | RU of stm * bExp          //  repeat until

let rec A a s =
    match a with
    | N n -> n
    | V x -> Map.find x s
    | Add(a1, a2) -> A a1 s + A a2 s
    | Mul(a1, a2) -> A a1 s * A a2 s
    | Sub(a1, a2) -> A a1 s - A a2 s
    | Inc x -> Map.find x s + 1;;

let rec B b s =
    match b with
    | TT -> true
    | FF -> false
    | Eq(b1, b2) -> A b1 s = A b2 s
    | Lt(b1, b2) -> A b1 s < A b2 s
    | Neg(b1) -> not (B b1 s)
    | Con(b1, b2) -> B b1 s && B b2 s

let update x v s = Map.add x v s;;

let rec I stm s =
    match stm with
    | Ass(x,a) ->  update x (A a s) s
    | Skip -> s
    | Seq(stm1, stm2) -> I stm1 s |> I stm2 
    | ITE(b,stm1,stm2) -> if (B b s) then I stm1 s else I stm2 s
    | While(b, stm) -> if (B b s) then (I stm s) |> I (While(b, stm)) else s
    | IT(b,stm) -> if (B b s) then I stm s else s
    | RU(stm, b) -> if (B b s) then s else (I stm s) |> I (RU(stm, b))

// Example 0
let stmt0 = Ass("res", Add(N 10, N 30))
let state0 = Map.empty

I stmt0 state0

// Example 1
let stmt1 = Ass("99", Sub(N 100, N 1))
let state1 = Map.empty

I stmt1 state1 // should store 1

// Example 2
let stmt2 = IT((TT), Ass("logic", N 3))
let state2= Map.empty

I stmt2 state2 // should store 3

// Example 3
let stmt3 = IT((FF), Ass("logic", N 3))
let state3= Map.empty

I stmt3 state3 // should store nothing

// Example 4
let stmt4 = RU(Ass("num", Add(V "num",N 1)),Eq(V "num",N 3))
let state4 = Map.add "num" 0 Map.empty

I stmt4 state4 // should repeat until number stored is 3

// Example 5
let stmt5 = IT(Eq(V "test2", V "test1"), Ass("testResult", N 1))
let state5 = (Map.add "test2" 5 Map.empty) |> Map.add "test1" 5 

I stmt5 state5 // testResult is positive

// Exercise 5.6
// I would update x with (V x + 1) s, such that it looks for the stored variable adds 1 to it, and stores that as the new state
// Implementation is in A a s
