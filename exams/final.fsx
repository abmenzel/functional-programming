// I hereby declare that I myself have created this exam hand–in in its entirety without help from anybody else.
// Alexander Menzel (alme@itu.dk)

// Q 1

type Rope =
  Leaf of string * int
| Node of Rope * int * Rope

let rope1 = Node(Node(Leaf("I_lik",5),5,Leaf("e_functional_pr",15)),
                 20,
                 Node(Leaf("ogr",3),3,Leaf("amming",6)))

// Q 1.1

let rope2 = Node(Leaf("_and", 4),
                4,
                Node(Leaf("_very_",6),
                    6,
                    Leaf("much_F#",7)))

// The type rope is monomorphic, as it can only store strings and integers. It does not define any generic types 'a.

let rope3 = Node(Node(Leaf("example_",8),
                    8,
                    Leaf("with_",5)),
                13,
                Leaf("5_nodes", 7))

// Q 1.2

// We use a recursive function to collect the size of all leaf nodes and return the sum.
let rec length (r:Rope) : int =
    match r with
    | Leaf (_,s) -> s
    | Node (nodeLeft, _, nodeRight) -> (length nodeLeft) + (length nodeRight)

length rope1

// We use a recursive function to collect the strings of all leaf nodes, and concatenate the returned substrings.
let rec flatten (r:Rope) : string =
    match r with
    | Leaf (word,_) -> word
    | Node (nodeLeft, _, nodeRight) -> (flatten nodeLeft) + (flatten nodeRight)

flatten rope1

// We count the depth of the left and right nodes and return the max of the two.
let maxDepth (r:Rope) : int =
    let rec maxDepth' (r:Rope) acc =
        match r with
        | Leaf _ -> (acc+1)
        | Node (nodeLeft, _, nodeRight) -> max (maxDepth' nodeLeft (acc+1)) (maxDepth' nodeRight (acc+1))
    maxDepth' r 0

maxDepth rope3

// We use the previously defined functions to check if the index is out of bounds, and to construct the string
let index i (r:Rope) : char =
    match (i >= (length r)) with
    | true -> failwith "index is out of bounds"
    | false -> (flatten r).[i]

index 5 rope1
index 29 rope1
index 50 rope3
index 5 rope3

// Q 1.3

// We use the previously defined length function to get the length of the left tree
let concat r1 r2 =
    Node(r1,(length r1),r2)

let rope4 = concat rope1 rope2


// !!!!
let prettyprint (r:Rope) = ""


// Q 2

type Bucket<'a> = { sizeBucket : int;
                    elems : List<'a> }

type UList<'a> = Bucket<'a> list

let ulist01 = [ { sizeBucket = 4; elems = ['A';'B';'C';'D'] };
                { sizeBucket = 2; elems = ['E';'F'] };
                { sizeBucket = 6; elems = ['G';'H'; 'I'; 'J'; 'K'; 'L'] };
                { sizeBucket = 6; elems = ['M'; 'N'; 'O'; 'P'; 'Q'; 'R'] };
                { sizeBucket = 4; elems = ['S'; 'T'; 'U'; 'V'] };
                { sizeBucket = 4; elems = ['W'; 'X'; 'Y'; 'Z'] } ]

// Q 2.1

// The type of ulist01 is Bucket<char> list, as fsharp's type inference system sees that it follows the rules defined for the Bucket type.
// Furthermore the lists are filled with chars, meaning that it is now no longer a list of generic types.
// So it is now a list of multiple buckets holding chars.

// We can create another value with the same elements as ulist01, and switch the order of the buckets.
let ulist02 = [ { sizeBucket = 4; elems = ['A';'B';'C';'D'] };
                { sizeBucket = 2; elems = ['E';'F'] };
                { sizeBucket = 6; elems = ['M'; 'N'; 'O'; 'P'; 'Q'; 'R'] };
                { sizeBucket = 6; elems = ['G';'H'; 'I'; 'J'; 'K'; 'L'] };
                { sizeBucket = 4; elems = ['S'; 'T'; 'U'; 'V'] };
                { sizeBucket = 4; elems = ['W'; 'X'; 'Y'; 'Z'] } ]

// the expression ulist02 = ulist01 returns false, as these values do not store the bucket records in the same order.
// However, for our data structure we don't care about the order of the buckets. Thus we consider these two store the same elements.

let emptyUL () : UList<'a> = []

// Q 2.2

// We get the size of the unrolled list by folding over the lsit, and using and accumulator to sum up the size of each bucket.
let sizeUL ul = List.fold (fun acc bucket -> acc + bucket.sizeBucket ) 0 ul

sizeUL ulist01
sizeUL (emptyUL())

// We check if the list is empty using pattern matching, matching on the empty list.
let isEmptyUL (ul:UList<'a>) =
    match ul with
    | [] -> true
    | _ -> false

isEmptyUL (emptyUL())
isEmptyUL ulist01

// We check each bucket with a tryFind, returning true if some matching value is found in some list.
let existsUL e (ul:Bucket<'a> list) =
    let res = List.tryFind (fun bucket ->
        match (List.tryFind (fun item -> e = item) bucket.elems) with
        | Some _ -> true 
        | None -> false) ul
    match res with
    | Some _ -> true
    | None -> false

existsUL 'A' (emptyUL())
existsUL 'A' ulist01

// First we concatenate all buckets into one list, then we check if the index is larger than the length of the combined list, if not, we return the item at the given index.
let itemUL (ul:Bucket<'a> list) i =
    let (totalSize, totalList) = List.fold (fun (size,lists) bucket -> (size + bucket.sizeBucket, lists @ bucket.elems)) (0,[]) ul
    match (i >= totalSize) with
    | true -> failwith "index is out of bounds"
    | false -> totalList.[i]

// First we map over the buckets and filter out the items on each list and update the size of the bucket.
// Then we filter out any bucket with size < 1
let filterUL p (ul:Bucket<'a> list) =
    List.filter (fun bucket -> bucket.sizeBucket > 0) (List.map (fun bucket ->
        let newElems = (List.filter p bucket.elems)
        {sizeBucket = (newElems.Length); elems = newElems}) ul)

filterUL (fun e -> e < 'I') ulist01

// Q 2.3
let ulist03Wrong = [ { sizeBucket = 2; elems = [1] };
                     { sizeBucket = 0; elems = [] };
                     { sizeBucket = 5; elems = [2;3;4;5;6] } ]

// We check all three rules and only return true if all rules are met.
// The assignment states that ulist01 should return true. However, it has buckets with sizeBucket larger than 4.
// Thereby, from my interpretation, it violates rule 1: maxSizeBucket = 4, and should return false, not true as stated in the assignment.

let chkUL (ul:Bucket<'a> list) =
    let sizeMatchElements = List.fold (fun state bucket -> if bucket.sizeBucket <> bucket.elems.Length then false else state) true ul
    let withinBounds = List.fold (fun state bucket -> if bucket.sizeBucket > 4 then false else state) true ul
    let notEmpty = List.fold (fun state bucket -> if List.isEmpty bucket.elems then false else state) true ul
    (sizeMatchElements && withinBounds && notEmpty)

chkUL ulist03Wrong
chkUL ulist01

// We map over each bucket in the unrolled list. For each bucket we map over the list of elements and apply the provided function f.
let map (f:'a->'b) (ul:Bucket<'a> list) =
    List.map (fun bucket -> {sizeBucket = bucket.sizeBucket; elems = (List.map (fun elem -> f elem) bucket.elems)}) ul

map (int) ulist01

// We fold over each bucket in the unrolled list. For each bucket we fold over the bucket's elements, and apply the provided function f.
let fold (f:'a->'b->'a) (a:'a) (ul:Bucket<'b> list) : 'a =
    List.fold (fun acc bucket -> List.fold (fun acc' elem -> f acc' elem) acc bucket.elems) a ul

fold (fun a c -> a+((string) c)) "" ulist01

// Q 3



// Get back to:
// q1.3
// q2.1
// q2.3 chkUL