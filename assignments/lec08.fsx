(* Assignment 8.1, HR 9.8 *)
type BinTree<'a> = (* Page 133 *)
    Leaf
  | Node of BinTree<'a> * 'a * BinTree<'a>

let rec countA t n =
    match t with
    | Leaf -> n
    | Node(tl,node,tr) -> countA tl (n) + 1 + countA tr (n)

(* Example *)
let t = Node(Node(Leaf,3,Node(Leaf,3,Leaf)),1,Node(Leaf,4,Leaf))
countA t 0

(* Assignment 8.2, HR 9.9 *)
let rec countAC t n c =
    match t with
    | Leaf -> c n
    | Node(tl, node, tr) -> countAC tl n (fun vl -> countAC tr n (fun vr -> c (vl + vr + 1)))

(* Example *)
countAC t 0 id

(* Assignment 8.3, HR 9.10 *)
let rec bigListK n k =
    if n=0 then k []
    else bigListK (n-1) (fun res -> 1::k(res))

// bigListK 130000 id
// the anonymous function is not tail-recursive, which causes it to save the function on the stack, leading to stack overflow
bigListK 130000 id

(* Assignment 8.4, HR 9.11 *)
let rec leftTreeC n c =
    match n with
    | 0 -> c (Node(Leaf,0,Leaf))
    | _ -> leftTreeC (n-1) (fun res -> c (Node(res,n,Leaf))) 

let leftTree n =
    leftTreeC n id
(* Examples *)
leftTree 0
leftTree 1
leftTree 3
leftTree 360000

let rec rightTreeC n c =
    match n with
    | 0 -> c (Node(Leaf,0,Leaf))
    | _ -> rightTreeC (n-1) (fun res -> c (Node(Leaf,n,res)))   
let rightTree n =
    rightTreeC n id
    
(* Examples *)
rightTree 0
rightTree 1
rightTree 2
rightTree 360000

let rec count = function (* from page HR 214 *)
    Leaf -> 0
    | Node(tl,n,tr) -> count tl + count tr + 1

//count (rightTree 200000) leads to stack overflow
//countA (rightTree 150000) 0 leads to stack overflow

let rec countC t c = (* from page HR 215 *)
    match t with
    Leaf -> c 0
    | Node(tl,n,tr) -> countC tl (fun vl -> countC tr (fun vr -> c(vl+vr+1)))

//countC (rightTree 20000000) id works without stack overflow in Real: 00:00:08.381
//countAC (rightTree 20000000) 0 id works without stack overflow in Real: 00:00:08.380

let xs = List.init 10000 (fun i -> 10000)
//for i in xs do let _ = count (rightTree i) in () // 04.589 seconds
//for i in xs do let _ = countC (rightTree i) id in () // 08.374 seconds
//for i in xs do let _ = countAC (rightTree i) 0 id in () //08.503 seconds

(* Assignment 8.5, HR 11.1 *)
let oddNumbers = Seq.initInfinite (fun n -> (n + n) + 1) 

(* Assignment 8.6, HR 11.2 *)
let fac =
    let rec fact n =
        match n with
        | 0 -> 1
        | 1 -> 1
        | n -> n * fact (n-1)
    Seq.initInfinite (fun n -> fact (n))

(*Examples *)
Seq.take 0 fac
Seq.take 1 fac
Seq.take 2 fac
Seq.take 10 fac

