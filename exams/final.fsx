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


// We use two helper functions nl (creates a new line) and (space n) creates indentation of n*4 characters.
// We then proceed to iterate over all nodes using a recursive function buildString. For each recursive call, we increase the indentation.
let nl = System.Environment.NewLine
let space n = String.replicate (n*4) " "

let prettyprint (r:Rope) =
    let rec buildString indent (r':Rope) =
        match r' with
        | Leaf (word, length) ->
            nl + (space indent) + "Leaf(" + word + "," + ((string) length) + ")"
        | Node (ropeLeft, size, ropeRight) ->
            nl + (space indent) + "Node(" +
            (buildString (indent + 1) ropeLeft) + "," +
            nl + (space (indent + 1)) + ((string) size) + "," +
            (buildString (indent + 1) ropeRight)
    printfn "%s" (buildString 0 r)

prettyprint rope1

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

// We can create another value with the same elements as ulist01, but with bucket two and bucket 1 combined.
let ulist02 = [ { sizeBucket = 6; elems = ['A';'B';'C';'D';'E';'F'] };
                { sizeBucket = 6; elems = ['G';'H'; 'I'; 'J'; 'K'; 'L'] };
                { sizeBucket = 6; elems = ['M'; 'N'; 'O'; 'P'; 'Q'; 'R'] };
                { sizeBucket = 4; elems = ['S'; 'T'; 'U'; 'V'] };
                { sizeBucket = 4; elems = ['W'; 'X'; 'Y'; 'Z'] } ]

// the expression ulist02 = ulist01 returns false, as these values do not store equal values of bucket records.
// However, for our data structure we don't care how elements are grouped together in the buckets, as long as the same elements are represented.
// Thus we consider ulist01 and ulist02 to store the same elements.

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

let ulist03Correct = [ { sizeBucket = 2; elems = [1;2] };
                       { sizeBucket = 4; elems = [2;3;4;5] } ] 

// We start off by assuming the unrolled list meets the rules, therefore we use true as our default state.
// We check all three rules and only return true if all rules are met.

// The assignment states that ulist01 should return true. However, it has buckets with sizeBucket larger than 4.
// Thereby, from my interpretation, it violates rule 1: maxSizeBucket = 4, and should return false, not true as stated in the assignment.
// I have create ulist03Correct to show an example of an unrolled list that meets all rules.
let chkUL (ul:Bucket<'a> list) =
    let maxSizeBucket = 4
    let sizeMatchElements = List.fold (fun state bucket -> if bucket.sizeBucket <> bucket.elems.Length then false else state) true ul
    let withinBounds = List.fold (fun state bucket -> if bucket.sizeBucket > maxSizeBucket then false else state) true ul
    let notEmpty = List.fold (fun state bucket -> if List.isEmpty bucket.elems then false else state) true ul
    (sizeMatchElements && withinBounds && notEmpty)

chkUL ulist03Wrong
chkUL ulist01
chkUL ulist03Correct

// We map over each bucket in the unrolled list. For each bucket we map over the list of elements and apply the provided function f.
let map (f:'a->'b) (ul:Bucket<'a> list) =
    List.map (fun bucket -> {sizeBucket = bucket.sizeBucket; elems = (List.map (fun elem -> f elem) bucket.elems)}) ul

map (int) ulist01

// We fold over each bucket in the unrolled list. For each bucket we fold over the bucket's elements, and apply the provided function f.
let fold (f:'a->'b->'a) (a:'a) (ul:Bucket<'b> list) : 'a =
    List.fold (fun acc bucket -> List.fold (fun acc' elem -> f acc' elem) acc bucket.elems) a ul

fold (fun a c -> a+((string) c)) "" ulist01


// Q 3

// Q 3.1

let rec G (m,n) =
    match n with
    | n when n <= 0 -> n + m
    | _ -> G (2*m,n-1) + m

G(10,10)

// This implementation of G is not tail-recursive, we can make it tail-recursive like below.
// Here we use an inner recursive function to accumulate the computed values.

let GA (m,n) =
    let rec G' (m,n) acc =
        match n with
        | n when n <= 0 -> acc + n + m
        | _ -> G' (2*m,n-1) (acc + m)
    G' (m,n) 0

GA(10,10)

// Q 3.2

// We use a nested loop in order to create 100 sequences from i to 100 each.
let mySeq = seq {for i in [1 .. 100] do yield! seq {for j in [1 .. 100] do yield (i,j)}}
Seq.take 4 mySeq

// We use a sequence expression in order to loop over elements from mySeq and apply GA to each of the tuples.
let gSeq = seq {for i in mySeq do GA i}
Seq.take 4 gSeq


// Q 4
type stack = int list
type inst =
  ADD    
| SUB
| PUSH of int
| LABEL of string
| IFNZGOTO of string
| EXIT

let insts01 =
  [PUSH 10;
   PUSH 12;
   ADD;
   EXIT]

let insts02 =
  [PUSH 10;
   LABEL "sub1";
   PUSH 1;
   SUB;
   IFNZGOTO "sub1";
   EXIT]

// Q 4.1
// We add support for add, push and exit using the defined template.
let execInsts insts =
  let rec exec insts s =
    match (insts,s) with
      | (SUB::is,v1::v2::s) -> exec is (v2-v1::s)
      | (ADD::is,v1::v2::s) -> exec is (v1+v2::s)
      | ((PUSH number)::is,s) -> exec is (number::s)
      | (LABEL lab::_,s) -> failwith "LABEL not implemented"
      | (IFNZGOTO lab::_,s) -> failwith "IFNZGOTO not implemented"
      | (EXIT::_,res::_) -> res 
      | _ -> failwith "Missing stack values for instruction"
  exec insts []
               
execInsts insts01
execInsts insts02

// Q 4.2

type resolvedInst =
    RADD
  | RSUB
  | RPUSH of int
  | RIFNZGOTO of int
  | REXIT
type prog = Map<int,resolvedInst>

type env = Map<string,int>

let lookup l m =
  match Map.tryFind l m with
    None -> failwith "Value not in map"
  | Some v -> v

// We build the environment by matching labels to their index, and returning the map built
let buildEnv insts =
  let rec build idx env = function
  | [] -> env
  | LABEL lab :: insts -> build idx (Map.add lab idx env) insts
  | _ :: insts -> build (idx+1) env insts
  build 0 Map.empty insts

buildEnv insts01
buildEnv insts02

// We create a resolved map of instructions by removing all labels and replacing references with their respective indexes.
// The result is a map with no labels.
let resolveInsts insts env =
  let rec resolve idx = function
    [] -> Map.empty
  | LABEL _ :: insts -> resolve idx insts
  | ADD :: insts -> Map.add idx RADD (resolve (idx+1) insts)
  | SUB :: insts -> Map.add idx RSUB (resolve (idx+1) insts)
  | (PUSH v):: insts -> Map.add idx (RPUSH v) (resolve (idx+1) insts)
  | IFNZGOTO lab :: insts -> Map.add idx (RIFNZGOTO (lookup lab env)) (resolve (idx+1) insts)
  | EXIT :: insts -> Map.add idx REXIT (resolve (idx+1) insts)
  resolve 0 insts

resolveInsts insts02 (buildEnv insts02)