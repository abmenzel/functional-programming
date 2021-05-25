// Question 1

// Question 1.1

// Declare an F# function infSeq3
let infSeq3 =
    Seq.initInfinite (fun index -> index * 3)

// Declare an F# function finSeq3
let finSeq3 n = Seq.take n infSeq3

// Declare a function sumSeq3 n of type ...
let sumSeq3 n =
    Seq.fold (fun acc i -> acc + i) 0 (finSeq3 n)

// Question 1.2
let seqMap2 f s1 s2 =
    seq {
        for (x, y) in Seq.zip s1 s2 do
            yield f x y
    }

// seqMap2 takes a function with two parameters as it's first parameter.
// the sequence returned will be a sequence in which each item is the result of applying the given function on item x from s1 and s2.
// Let's say the function takes two integers and adds them together (+)
// In this case the first item in the output sequence would be the result of adding together the first item from both s1 and s2.

// seqMap2 swap will not work as the function "swap" is declared as only taking a single parameter (x,y)
// However, as the signature ('a -> 'b -> 'c) tells us, this function must take two parameters.

let swap (x, y) = (y, x)

let fix f x y =
    match f, x, y with
    | f, x, y -> f (x, y)

seqMap2 (fix swap) [ 1; 3; 3 ] [ 4; 5; 2 ]

// Question 2

type TrieNode<'a when 'a: equality> = TN of 'a * bool * TrieNode<'a> list

let trie01 =
    TN('a', false, [ TN('n', false, [ TN('d', true, []) ]) ])

let trie03 =
    TN(
        'a',
        false,
        [ TN('n', true, [ TN('d', true, []) ]);
          TN('d', false, [ TN('d', true, []) ]);
          TN('t', true, []) ]
    )

let trie04 =
    TN(
        'a',
        false,
        [ TN('n', true, [ TN('d', true, []) ])
          TN('d', false, [ TN('d', true, []) ])
          TN('t', true, [ TN('x', false, []) ]) ]
    )

// trie04 is of type TrieNode<char>, this is a monomorphic type as it has been set to hold types of 'char'

// ???
exception TrieError of string

// Question 2.2

let rec numLetters t =
    match t with
    | TN (_, _, []) -> 1
    | TN (_, _, t2) ->
        1
        + (List.fold (fun acc i -> acc + (numLetters i)) 0 t2)

numLetters trie04

let rec numWords t =
    match t with
    | TN (_, res, []) -> if res then 1 else 0
    | TN (_, res, t2) ->
        if res then
            1
            + (List.fold (fun acc i -> acc + (numWords i)) 0 t2)
        else
            0
            + (List.fold (fun acc i -> acc + (numWords i)) 0 t2)

numWords trie04

let rec exists ls t =
    match ls, t with
    | x::[], TN(l, res, t2) -> if(x = l && res) then true else false 
    | x::xs, TN(l, res, t2) when x = l -> List.fold (fun s i -> if s then s else exists xs i) false t2
    | x::xs, TN(l, res, t2) when x <> l -> false

exists ['a';'d'] trie04


let rec chkTrie t =
    match t with
    | TN(l, res, []) -> if res then true else false
    | TN(l, res, t2) -> List.fold (fun s t -> chkTrie t) false t2

chkTrie trie04

// Question 2.3
let rec map f t =
    match t with
    | TN(l, res, []) -> TN(f l, res, [])
    | TN(l, res, t2) -> TN(f l, res, (List.map (fun t -> map f t) t2))

map (string) trie03

// Question 3
let rec F m i n k =
    match k with
    | k when k <= 0 -> m
    | _ -> (F (m * (1.0 + (i/n))) i n (k-1))

let test = F 100.0 0.1 1.0 0
let test2 = F 100.0 0.1 1.0 10

// 3.2
let rec tabulate f start step stop =
    match start with
    | start when start < stop -> [(start, f start)] @ (tabulate f (start + step) step stop)
    | _ -> [(start, f start)]

tabulate (F 100.0 0.1 1.0) 0 2 4

let prettyPrint xs =
    printfn "   x  | f(x)"
    printfn "------+--------"
    List.iter (fun (t,x) -> printfn "    %i | %.2f" t x) xs

prettyPrint [(0,100.0); (2, 121.0)]

// Question 4
let dt(d,m,y) = System.DateTime(y, m, d)
exception Error of string
type Position =
| Stock of string
| Cash of float
type Action =
| Aquire of System.DateTime * Position
| Give of System.DateTime * Position
| Scale of int * Action
| All of Action list

let ex1 =
    Scale(100, All[Aquire (dt(1,2,2018), Stock "APPLE");
        Give (dt(1,2,2018), Cash 300.3)])

// Question 4.1
let sellApple = Scale(100, All[Aquire (dt(1,3,2018), Cash 400.4);
        Give (dt(1,3,2018), Stock "APPLE")])

let priceTable =
    Map [(("APPLE",dt(1,2,2018)),300.3);
         (("APPLE",dt(1,3,2018)),400.4);
         (("ISS",dt(1,2,2018)),150.0);
         (("ISS",dt(1,3,2018)),200.2);
         (("TIVOLI",dt(1,2,2018)),212.0);
         (("TIVOLI",dt(1,3,2018)),215.2)
        ]
let price (s,d) =
    match Map.tryFind (s,d) priceTable with
    | Some (price) -> price
    | None -> raise (Error "price for date is unknown")

price ("ISS", dt(1,3,2018))

// Question 4.2
let buyStock n s d =
    match price (s,d) with
    | price -> Scale(n, All[(Aquire (d, Cash price)); (Give (d, Stock s))])

let appleStock = buyStock 100 "APPLE" (dt(1,2,2018))
let unknownDate = buyStock 1000 "APPLE" (dt(2,5,2000))

let receiveCash c d =
    Aquire(d,Cash c)

let rc = receiveCash 100000.0 (dt(1,2,2018))

// Question 4.3
let actions =
    let d1 = dt(1,2,2018)
    let d2 = dt(1,3,2018)
    All [receiveCash 100000.0 d1;
         buyStock 100 "APPLE" d1;
         buyStock 200 "ISS" d1;
         buyStock 50 "TIVOLI" d2]

type stockEnv = Map<string, int>
let updStock s n m =
    match Map.tryFind s m with
    None -> Map.add s n m
    | Some n1 -> Map.add s (n+n1) m

type env = float * stockEnv
let emptyEnv = (0.0, Map.empty)

// ??? ENV
let updEnv scaling (cash, stockEnv) pos =
    match pos with
    | Cash amount -> env (cash + (float) scaling * amount, stockEnv)
    | Stock name -> env (0.0, updStock name scaling stockEnv)

updEnv 100 emptyEnv (Cash 100.0)
updEnv 100 emptyEnv (Stock "APPLE")

let execA action env =
    let rec exec scaling env = function
        | Aquire(d,p) -> updEnv scaling env p
        | Give(d,p) -> updEnv -scaling env p
        | Scale(i,a) -> exec i env a
        | All(al) -> List.fold (fun s a -> (exec scaling s a)) env al 
    exec 1 env action

