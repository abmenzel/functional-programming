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

// Example of function that makes ex3 heap property false
let exfalse3 = map (fun e -> e*(-1)) ex3

// Question 2

// Question 2.1

// Declare function genRandoms n
let random =
    let rnd = System.Random()
    fun () -> rnd.Next(1,10000)

let genRandoms n =
    [| for _ in 1 .. n -> random() |]

let genRandomsP n =
    Array.Parallel.init n (fun _ -> random())

genRandoms 100000000
genRandomsP 100000000
// Question 2.2

// Declare split ...
let split xs =
    match xs with 
    | [] -> ([], [])
    | _ -> List.splitAt ((List.length xs)/2) xs

let case1 = split [1;2;3;1]
// Assert that a simple list with an even number of items is distributed into two lists
let case2 = split [1;2;3]
// Assert that a list with an uneven number of items is split into two lists, with the second list getting the uneven index
let case3 = split ([]:list<int>)
// Assert that an empty list is split into two empty lists

// Declare indivisible xs
let indivisible xs =
    match xs with
    | [] -> true
    | [_] -> true
    | _ -> false

let ind_case1 = indivisible ([]:list<int>)
let ind_case2 = indivisible [1]
let ind_case3 = indivisible [1;3;4]

// Define merge
let merge (xs,ys) =
    let rec m (list1,list2) merged =
        match list1, list2 with
        | [],[] -> merged
        | x::list1,[] -> m (list1, list2) (x::merged) 
        | [],y::list2 -> m (list1, list2) (y::merged) 
        | x::list1, y::list2 when x >= y -> m (x::list1, list2) (y::merged) 
        | x::list1, y::list2 when x <= y -> m (list1, y::list2) (x::merged) 
    List.rev (m (xs,ys) [])

let merge_case1 = merge ([],[1])
// Assert that the function merges with an empty list
let merge_case2 = merge ([1;2;3], [1;5])
// Assert that the function places items from list2 in the correct order, that is 1 at index 1 and 5 at index 4
let merge_case3 = merge ([1;2;3], [1;2;3])
// Assert that the function correctly merges identical lists

// Question 2.3

let divideAndConquer split merge indivisible p =
    let rec dc p =
        if indivisible p
        then p 
        else match (split p) with (l1,l2) -> merge (dc l1, dc l2)
    dc p

divideAndConquer split merge indivisible [22;746;931;975;200]

// Question 3
let triNum =
    Seq.initInfinite (fun i -> i*(i+1)/2)

let triNumC =
    Seq.cache triNum

let rec filterOddIndex s =
    Seq.append (Seq.singleton (Seq.item 0 s)) (filterOddIndex (Seq.skip 2 s))

let myFilterOddIndex s =
    let rec inner seq acc =
        match seq with
        | seq -> inner (Seq.skip 2 seq) (Seq.append acc (Seq.item 0 seq))
        | _ -> acc
    inner s Seq.empty

myFilterOddIndex triNum

// Question 3.3
let rec zipSeq s1 s2 =
    seq {let e1 = Seq.item 0 s1
         let e2 = Seq.item 0 s2
         (e1,e2); yield! (zipSeq (Seq.skip 1 s1) (Seq.skip 1 s2)) }

let test =
    seq {1;2;3}

printfn "%A" (zipSeq triNum triNum)

// Question 4
exception FigError of string
type Point = P of double * double
type Fig =
    | Circle of Point * double
    | Line of Point * Point
    | Move of double * double * Fig
    | Combine of Fig list
    | Label of string * Fig
    | Ref of string

let figEx01 = Combine [Circle(P(1.0,1.0),2.0);Line(P(0.0,0.0),P(1.0,1.0))]

// Declare an F# value rectEx of type Fig that represents a rectangle with four points
let rectEx = Combine [Line(P(-1.0,1.0),P(1.0,1.0));Line(P(1.0,1.0),P(1.0,-1.0));Line(P(-1.0,-1.0),P(1.0,-1.0)); Line(P(-1.0,1.0),P(-1.0,-1.0))]

// Declare rect ...
let rect (x1,y1) (x2,y2) =
    Combine [Line(P(x1,y1),P(x2,y1)); Line(P(x1,y2),P(x2,y2)); Line(P(x1,y1),P(x1,y2)); Line(P(x2,y1),P(x2,y2))]

// Consider F# value figEx02
let figEx02 =
    Combine [Label("c",Circle(P(0.0,0.0),1.0));
             Move(1.0,1.0,Ref "c");
             Move(2.0,2.0,Ref "c")]

let figEx03 =
    Combine [Label("e",Line(P(1.0,1.0),P(2.0,2.0))); Label("d",Line(P(5.0,5.0),P(6.0,6.0)))]

let buildEnv fig =
    match fig with
    | Label(l,figure) -> Map [(l,figure)]
    | Combine(list) ->
        let rec findLabels ls acc =
            match ls with
            | Label(l,figure)::ls -> findLabels ls (Map.add l figure acc)
            | _::ls -> findLabels ls acc
            | _ -> acc
        findLabels list Map.empty
    | _ -> Map.empty

let envEx02 = buildEnv figEx02
let envEx03 = buildEnv figEx03

// Question 4.3

// Declare F# function substFigRefs env fig
let substFigRefs env fig =
    let rec substFigRefsRec fig =
        match fig with
        | Ref(ref) ->  Map.find ref env
        | Combine(list) -> Combine (List.map substFigRefsRec list)
        | Label(_,x) -> substFigRefsRec x
        | Move(x,y,z) -> Move(x,y,substFigRefsRec z)
        | _ -> fig
    substFigRefsRec fig 

let substEx02 = substFigRefs envEx02 figEx02

// Question 4.4

let rec reduceMove fig =
    match fig with
    | Move(newX,newY,Circle(P(x,y),r)) -> Circle(P(x+newX,y+newY),r)
    | Move(newX,newY,Line(P(x,y),P(x1,y1))) -> Line(P(x+newX,y+newY),P(x1+newX,y1+newY))
    | Combine(list) -> Combine (List.map reduceMove list)
    | _ -> fig

let reduceEx02 = reduceMove substEx02
    