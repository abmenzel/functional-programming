// Question 1.1

type mymap<'a,'b> = MyMap of list<'a*'b>

// Declare map values dice1 and dice2, representing the two maps
let dice1 = MyMap [(1,4);(2,2);(3,3);(4,2);(5,2);(6,2)]
let dice2 = MyMap [(1,4);(2,2);(3,3);(4,3);(5,5);(6,3)]

// Explain the type of the two values
// Both values are of type mymap which holds a key of type int mapped to a value of type int

// Declare an F# function emptyMap() of type ...
let emptyMap () = MyMap []

// Declare F# function size m of type ...
let size m =
    match m with 
    | MyMap m -> List.length m

// Question 1.2

// Declare an F# function isEmpty m
let isEmpty m =
    match m with
    | MyMap m when List.length m = 0 -> true
    | _ -> false

// Declare an F# function tryFind of type ...
let tryFind k m =
    match m with
    | MyMap m -> List.tryFind (fun (x,_) -> x = k) m

// Declare an F# function remove k m of type ...
let remove k m =
    match m with
    | MyMap m ->
        let result = List.tryFind (fun (x,_) -> x = k) m
        match result with
        | Some t -> MyMap (List.except [t] m) 
        | None -> MyMap (m)

// Declare an F# function add k v m of type ...
let add k v m =
    match m with
    | MyMap m ->
        let result = List.tryFind (fun (x,_) -> x = k) m
        match result with
        | Some t -> MyMap ((k,v)::(List.except [t] m))
        | None -> MyMap ((k,v)::m)

// Question 1.3

// Declare an F# function upd f k v m
let upd f k v m =
    match m with
    | MyMap m ->
        let result = List.tryFind (fun (x,y) -> x = k || y = v) m
        match result with
        | Some (x,y) -> MyMap ((x,f y v)::m)
        | None -> MyMap ((k,v)::m)

// Declare an F# function map f m of type ...
let map f m =
    match m with
    | MyMap m -> MyMap (List.map (fun (k,v) -> (k,(f k v))) m)

//let test = map (fun k v -> v + 2) dice1
        
let fold f s m =
    match m with
    | MyMap m -> List.fold (fun state (k,v) -> f state k v) s m

// Question 2.1

// Declare  an F# function even n of type ...
let even n =
    match n with
    | n when n % 2 = 0 -> true
    | _ -> false

// Declare an F# function collatz n of type ...
let collatz n =
    match n with
    | n when even n -> n / 2
    | _ -> 3 * n + 1

// Declare an F# function collatz' n of type ...
let collatz' n =
    match n with
    | n when n <= 0 -> failwith("collatz': n is zero or less")
    | _ -> collatz n

// Question 2.2

// Declare an F# function applyN of type ...
let applyN f n N =
    let start = N
    let rec buildListN n N l =
        match N with
        | N when N = start -> (n)::(buildListN (f n) (N-1) l)
        | N when N > 1 -> (f n)::(buildListN (f n) (N-1) l)
        | _ -> [f n]
    buildListN n N []

// Declare an F# function applyUntilOne f n of type ...
let applyUntilOne f n =
    let rec findI n i =
        match n with
        | n when n = 1 -> i
        | _ -> findI (f n) (i+1)
    findI n 0

// Question 2.3

let rec mySeq f x =
    seq { yield x
          yield! mySeq f (f x)}

// x = 4
// f x = collatz 4 = 2
// myseq collatz 2
// x = 2
// f x = collatz 2 = 1
// myseq collatz 1
// x = 1
// f x = collatz 1 = 

// An example sequence with x = 42 is: seq[4; 2; 1; 4; 2; 1; 4; 2; 1; ...]

// Declare g x
let g x = x*2

// Question 3
type name = string
type quantity = float
type date = int * int * int
type price = float
type transType = Buy | Sell
type transData = date * quantity * price * transType
type trans = name * transData

let ts : trans list =
    [("ISS", ((24,02,2014),100.0,218.99,Buy)); ("Lego",((16,03,2015),250.0,206.72,Buy));
    ("ISS", ((23,02,2016),825.0,280.23,Buy)); ("Lego",((08,03,2016),370.0,280.23,Buy));
    ("ISS", ((24,02,2017),906.0,379.46,Buy)); ("Lego",((09,11,2017), 80.0,360.81,Sell));
    ("ISS", ((09,11,2017),146.0,360.81,Sell)); ("Lego",((14,11,2017),140.0,376.55,Sell));
    ("Lego",((20,02,2018),800.0,402.99,Buy)); ("Lego",((02,05,2018),222.0,451.80,Sell));
    ("ISS", ((22,05,2018),400.0,493.60,Buy)); ("ISS", ((19,09,2018),550.0,564.00,Buy));
    ("Lego",((27,03,2019),325.0,625.00,Sell)); ("ISS", ((25,11,2019),200.0,680.50,Sell));
    ("Lego",((18,02,2020),300.0,720.00,Sell))]

let addTransToMap ((n, td):trans) m =
    match Map.tryFind n m with
    | None -> m.Add (n, [td])
    | Some transList -> m.Add (n, td::transList)

let shares = List.foldBack addTransToMap ts Map.empty

let accTrans (tq:float, avg:float) ((d,q,p,tType):transData) =
    match tType with
    | Buy ->
        ((tq + q), ((avg * tq) + (q * p)) / (tq + q))
    | Sell ->
        (tq - q, avg)

let quantityAndAvgPrice ts = List.fold accTrans (0.0,0.0) ts

quantityAndAvgPrice [((24,02,2014),100.0,218.99,Buy);((23,02,2016),825.0,280.23,Buy)]

let res = Map.map (fun name td -> (quantityAndAvgPrice td)) shares

// Question 4

// Question 4.1

// Consider the F# declaration
let rec dup = function
      [] -> []
    | x::xs -> x::x::dup xs
// Describe the list generated
// The dup function duplicates all elements of a list, that is given a list [1,2,3] the dup of that list would be [1,1,2,2,3,3]

// Tail recursive version
let rec dupA l acc =
    match l with
    | [] -> acc
    | x::xs -> dupA xs (acc @ [x] @ [x])

dupA [1;2;3] []

// Question 4.2

// Declare an F# function replicate2 i of type ...
let replicate2 i =
    seq (dupA [i] [])

let dupSeq =
    Seq.initInfinite (fun index ->
    let n = float index
    int(floor (n/2.0)))

// Question 4.3

// Declare an F# function dupSeq2
let dupSeq2 s =
    seq {for i in s do yield i; yield i}


