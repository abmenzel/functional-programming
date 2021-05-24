// Q1

type multimap<'a,'b when 'a: comparison> = MMap of Map<'a,list<'b>>

let ex = MMap (Map.ofList [("record",[50]);("ordering",[36;46;70])])


// Q1.1
let studReg = MMap (Map.ofList [("Grete", []); ("Hans", ["TOPS"; "HOPS"]); ("Peter", ["IFFY"]); ("Sine", ["HOPS"; "IFFY"; "BFNP"])])

let studReg2 = MMap (Map.ofList [("Grete", []); ("Hans", ["TOPS"; "HOPS"]); ("Peter", ["IFFY"]); ("Sine", ["IFFY"; "HOPS"; "BFNP"])]) 

let compare = studReg = studReg2
// As the lists are considered as sets, the order in which strings occur is irrelevant.
// However, changing the order means the data is not the same, resulting in a false comparison.

// Q1.2
let canonical (MMap map) = MMap (Map.map (fun _ v -> List.sort v) map)

let toOrderedList map = match canonical map with MMap m -> Map.toList m

let studRegOrdered = toOrderedList studReg

// Q1.3
let newMultiMap () = MMap Map.empty

let sizeMultimap (MMap map) =
    let subSetsSize = Map.fold (fun size k v -> size + (List.length v)) 0 map
    (map.Count, subSetsSize)

let sizeStudReg = sizeMultimap studReg

// Q1.4
let addMultimap k v (MMap m) =
    match Map.tryFind k m with
    | Some list -> MMap (Map.add k (List.distinct (v::list)) m)
    | None -> MMap (Map.add k [v] m)

sizeMultimap (addMultimap "Sine" "BFNP" studReg)
sizeMultimap (addMultimap "Grete" "TIPS" studReg)
sizeMultimap (addMultimap "Pia" "" studReg)

let removeMultimap k vOpt (MMap m) =
    match vOpt with
    | None -> MMap (Map.remove k m)
    | Some v -> 
        match Map.tryFind k m with
        | None -> MMap (m)
        | Some list -> MMap (Map.add k (List.filter (fun elm -> elm <> v) list) m) 

sizeMultimap (removeMultimap "Sine" None studReg)
sizeMultimap (removeMultimap "Sine" (Some "PLUR") studReg)
sizeMultimap (removeMultimap "Kenneth" (Some "BLOB") studReg)
sizeMultimap (removeMultimap "Peter" (Some "IFFY") studReg)


// Q1.5
let mapMultimap f (MMap m) =
    MMap (Map.map (fun key list -> List.distinct (List.map (fun elm -> f key elm) list)) m)

// Q1.6
let foldMultimap (f:'s->'k->'t->'s) s (MMap m:multimap<'k,'t>) =
    Map.fold (fun acc key list -> List.fold (fun acc' elm -> f acc' key elm) acc list) s m

// Q2
let rec f i j xs =
    if xs = [] then
        [i*j]
    else
        let (x::xs') = xs
        x*i :: f (i*j) (-1*j) xs'

f 10 1 [1 .. 9]

// 1 x*i = 1*10
// 1 i*j = 10*1
// 1 -1*j = -1*1
// 10::
// 2 x*i = 10*2
// 2 i*j = 10*-1
// 2 -1*j = -1*-1
// 10::20
// 3 x*i = -10*3
// 3 i*j = -10*1
// 3 -1*j = -1*1
// 10::20::-30
// 4 x*i = -10*4
// 4 i*j = -10*-1
// 4 -1*j = -1*-1

// f computes a list of numbers from the list xs, multiplying each value with i, and for every pair of two numbers, it inverts the numbers.

// The incomplete pattern match issue arises in the else statement.
// When destructuring the statement let (x::xs) = xs, the compiler can't tell if xs at this point is empty.
// Preceeding this statement, multiple other cons statements may have been used. Thus, the case of an empty list is unhandled.
// This is why it is better to handle this using pattern matching, through a match expression.

let rec fMatch i j xs =
    match xs with
    | [] -> [i*j]
    | x::xs -> x*i :: f (i*j) (-1*j) xs

// Q2.2
let fA i j xs =
    let rec fA' i j xs acc =
        match xs with
        | [] -> ((i*j)::acc)
        | x::xs -> fA' (i*j) (-1*j) xs ((x*i)::acc)
    List.rev (fA' i j xs [])

fA 10 1 [1 .. 9]

// 3.1
let myFinSeq n m = seq { for i in [n .. m] do
                            yield [n .. i] }

Seq.take 10 (myFinSeq 5 10)
// myFinSeq builds a sequence of m lists where each sequence gradually grows starting from n, until it has a range of all numbers from n to m.
// Given that n < m.

// 12 occurs 3 times

// 3.2

let myFinSeq2 n m = seq {for i in [n .. m] do
                            yield! [n .. i] }

// 3.3
let sum xs = List.fold (fun r x -> r+x) 0 xs
let seq4000 = myFinSeq 10 4000
let array4000 = Array.ofSeq seq4000

// The array holds 4000 lists
let sums = Array.map sum array4000

let sums = Array.Parallel.map sum array4000


// 4
type JSONlite =
    Object of list<string * Value>
and Value =
    String of string
    | Record of JSONlite
    | Label of string * Value
    | Ref of string


let address = Object [("Street", String "Hansedalen");
    ("HouseNo", String "27")]
let person1 = Object [("Name", String "Hans Pedersen");
    ("Address", Label ("Addr1", Record address))]
let person2 = Object [("Name", String "Pia Pedersen");
    ("Address", Ref "Addr1")]
let persons = Object [("Person1", Record person1);
    ("Person2", Record person2)]


// 4.1
let student = Object [("Name", String "Per Simonsen"); ("Field", String "BSWU")]


// 4.2
let nl = System.Environment.NewLine
let space n = String.replicate n " "
let ppQuote s = "\"" + s + "\""

let ppJSONlite json =
    let rec ppValue' indent = function
        | String s -> (ppQuote s)
        | 
    and ppJSONlite' indent = function
        | Object xs ->
            nl + "{"
            + nl + List.fold (fun state elm ->
                let (key, value) = elm
                state + " " + (ppQuote key) + " : { " + (ppValue' indent value) + " }" + nl) "" xs
            + nl + "}"
    ppJSONlite' 0 json

ppJSONlite persons
