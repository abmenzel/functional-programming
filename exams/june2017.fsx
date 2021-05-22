// Question 1
type PrioritySet<'a when 'a: equality> = PrioritySet of List<'a>

let psEx = PrioritySet ["a";"b";"c"]

//Q1.1

let priSetEx = PrioritySet ["a"; "q"; "b"; "d"]

// priSetEx is of type PrioritySet

let empty = PrioritySet List.empty

// Q1.2
let isEmpty (PrioritySet set) = List.isEmpty set

let size (PrioritySet set) = List.length set

let contains e (PrioritySet ps) = List.contains e ps

let getPN e (PrioritySet ps) =
    match contains e (PrioritySet ps) with
    | true -> (List.findIndex (fun item -> item = e) ps) + 1
    | false -> failwith "item does not exist"

getPN "b" priSetEx

// Q1.3

let remove e (PrioritySet ps) =
    match contains e (PrioritySet ps) with
    | true -> PrioritySet (List.except [e] ps)
    | false -> PrioritySet ps

let add e (PrioritySet ps) =
    match contains e (PrioritySet ps) with
    | true -> PrioritySet ps
    | false -> PrioritySet (e::ps)

// Q1.4
let map f (PrioritySet ps) =
    PrioritySet (List.distinct (List.map (fun elm -> f elm) ps))

let cp (PrioritySet ps1) (PrioritySet ps2) =
    PrioritySet (List.allPairs ps1 ps2)

// Q2
let f curRow =
    let rec f' = function
    | [] -> []
    | [_] -> [1]
    | xs -> let (x1::x2::xs) = xs
            x1 + x2 :: f' (x2::xs)
    (1 :: f' curRow)

// Q2.1
// f creates a list starting from 1 followed by the results of adding together pairs from the input list.
// the list will always end with 1.


// Q2.2
let fMatch curRow =
    let rec fMatch' curRowi =
        match curRowi with
        | [] -> []
        | [_] -> [1]
        | x1::x2::xs -> x1 + x2 :: fMatch' (x2::xs)
    (1 :: fMatch' curRow)

// f has incomplete pattern matching, as the compiler can't tell whether it is able to deconstruct x1::x2::xs from xs at the time this is done.
// Manipulations could have been made to xs prior to the destructuring.
// This is avoided by using pattern matching.

// Q2.3
let fA curRow =
    let rec fA' curRow acc =
        match curRow with
        | [] -> acc
        | [_] -> acc @ [1]
        | x1::x2::xs -> fA' (x2::xs) (acc @ [x1+x2])
    (1 :: fA' curRow [])


// Q3.1
let mySeq s1 s2 =
    seq { for e1 in s1 do
        for e2 in s2 do
            yield! [e1;e2] }

// The sequence creates the cartesian product from two lists, e.g. all pairs.

Seq.toList (mySeq (seq {"A";"B"}) (seq {"D";"E";"F"}))

// Q3.2
let mySeq2 s1 s2 =
    seq { for e1 in s1 do
        for e2 in s2 do
        yield! [(e1,e2)]}

// Q3.3
let mySeq3 n =
    Seq.initInfinite (fun i ->  ((int) ((float) n**2.0)) - ( n + i))


// Question 4
type Env = Map<string, string list>

type DataSpec =
    | RangeInt of int * int
    | Pick of string
    | Label of string * DataSpec
    | ChoiceString of string list
    | StringSeq of string
    | Pair of DataSpec * DataSpec
    | Repeat of int * DataSpec


let reg =
    Repeat(3,Pair(StringSeq "a",
        Pair(ChoiceString["cheese";"herring";"soft drink"],
            RangeInt(1,100))))

let pur1 = [(3, "a2"); (1, "a1")]

let pur = Repeat(2,Pair(RangeInt(1,10),StringSeq "a"))

let rand = System.Random()
let next(i1,i2) = rand.Next(i1,i2)
let numGen =
    let n = ref 0
    fun () -> n := !n+1; !n

let rec genValue = function
    | RangeInt(i1, i2) -> next(i1,i2).ToString()
    | ChoiceString xs -> List.item (next(0, (List.length xs))) xs
    | Repeat (x,e) -> List.fold (fun acc e -> acc + ", " + (genValue e)) "" (Seq.toList (Seq.init x (fun _ -> e)))
    | Pair (e1,e2) -> genValue e1 + " " + genValue e2
    | StringSeq s -> s + numGen.ToString()

let reg2 = Repeat(3, Pair(Label("articleCode", StringSeq "a"),
                        Pair(ChoiceString["cheese";"herring";"soft drink"],
                            RangeInt(1,100))))

let pur2 = Repeat(2, Pair(RangeInt(1,10), Pick "articleCode"))

let addToEnv s v (dEnv:Env) : Env =
    match Map.tryFind s dEnv with
    | None -> Map.add s [v] dEnv
    | Some ev -> Map.add s (ev @ [v]) dEnv

let pickFromEnv s (dEnv:Env) =
    match Map.tryFind s dEnv with
    | None -> failwith "does not exist"
    | Some v ->
        let itemNumber = next(0,v.Length)
        List.item itemNumber v

