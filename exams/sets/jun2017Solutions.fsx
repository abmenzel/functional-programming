// Solutions for the June 2017 exam set.
// Solutions are not complete with comments etc. to do a full mark grading.

// 1 Priority Set

exception PrioritySetErr of string

type PrioritySet<'a when 'a: equality> = PrioritySet of list<'a>

// Q1.1
let psEx = PrioritySet ["a";"b";"c"]
let priSetEx = PrioritySet ["a";"q";"b";"d"]
let empty = PrioritySet []

// Q1.2
let isEmpty s = s=empty 
let size (PrioritySet s) = List.length s
let contains e (PrioritySet ps) = List.contains e ps
contains "b" priSetEx

let ex1 = PrioritySet ["a"]
let ex2 = PrioritySet []
let ex3 = PrioritySet ["a";"b"]
let ex4 = PrioritySet ["a";"b";"c"]

let getPN e (PrioritySet ps) =
  let rec getPN' n = function
    | [] -> raise (PrioritySetErr "getPN: element e does not exists")
    | x::xs -> if x=e then n else getPN' (n+1) xs
  getPN' 1 ps
    
let _ = getPN "d" ex4
let _ = getPN "a" priSetEx

// Not Used
let rec insertNth e n xs =
  match (n,xs) with
  | (1, xs) -> e :: xs
  | (n, _) when n < 1 -> raise (PrioritySetErr "insertNth with n < 1.")
  | (n, e'::xs') -> e' :: insertNth e (n-1) xs'
  | (_, []) -> [e] // We just insert with lowest priority number.

let ofPSet (PrioritySet l) = l
Let _ = insertNth "A" 3 (ofPSet ex4)

// Q1.3
let removeL e = List.filter (fun x -> x<>e)
let remove e (PrioritySet ps') = PrioritySet (removeL e ps')
remove "b" psEx

// Not Used
let add e (PrioritySet ps' as ps) =
  if contains e ps then ps else PrioritySet (insertNth e (size ps + 1) ps')

// Add as lowest priority, i.e., e gets priority number (size ps) + 1.
let insertLast x xs = xs @ [x]
let add e (PrioritySet ps' as ps) =
  if contains e ps then ps else PrioritySet (insertLast e ps')

let add e (PrioritySet ps') =
  if contains e (PrioritySet ps') then ps else PrioritySet (insertLast e ps')

  
add "b" psEx
add "d" psEx

// Not Used
let rec removeNth n xs =
  match (n,xs) with
  | (1, e::xs') -> xs'
  | (n, _) when n<1 -> raise (PrioritySetErr "removeNth with n < 1.")    
  | (n, e::xs') -> e::removeNth (n-1) xs'
  | (_, []) -> raise (PrioritySetErr "removeNth: try remove element with priority number too big.")

// Not Used
let addPN e pn (PrioritySet ps' as ps) =
  if contains e ps then
    let pn' = getPN e ps
    if pn' <= pn then ps // Do nothing as e already exists with higher priority
                 // Remove e with old priority number and insert e with new priority number. Removing e first does not influence new priority number.    
                 else PrioritySet (insertNth e pn (removeL e ps')) 
  else PrioritySet (insertNth e pn ps')

// Q1.4
// In case the function fn maps the domain into the same value for
// some values, then one should apply distinct. So map' is a better
// solution. It it not specified what elements should then be
// removed. A good decision would be to keep the ones with lowest
// priority number. This happens to be what List.distinct does.
// From the library documentation:
//  Returns a list that contains no duplicate entries according to
//  generic hash and equality comparisons on the entries. If an element
//  occurs multiple times in the list then the later occurrences are
//  discarded.
// Notice: Considering dublicates is necessary to get full mark, but exactly
//         how List.distict works is not required.
let map fn (PrioritySet ps') = PrioritySet (List.map fn ps')  // Not full mark
let map fn (PrioritySet ps') = PrioritySet (List.distinct (List.map fn ps'))  
map (fun (c:string) -> c.ToUpper()) psEx

// Not Used
let union (PrioritySet ps1') (PrioritySet ps2') =
  let rec union' ps1' ps2' =
    match (ps1', ps2') with
    | (p1::ps1',p2::ps2') -> p1::p2::union' ps1' ps2'
    | (ps1',[]) -> ps1'
    | ([], ps2') -> ps2'
  PrioritySet (union' ps1' ps2')

// Folding over the ps1 first being first component in pairs build.
// For each element in ps1 we build pairs with elements from ps2 in
// order of priority.  If either ps1 or ps2 are an empty priority set
// an empty priority set is returned.
let cp (PrioritySet ps1') (PrioritySet ps2') =
  List.fold (fun acc e1 ->
             List.fold (fun acc e2 -> add (e1,e2) acc) acc ps2')
            (PrioritySet []) ps1'

let ex5 = PrioritySet ["A";"B";"C"]
let ex6 = PrioritySet ["h";"i"]
cp ex5 ex6
cp psEx (empty:PrioritySet<string>)
cp psEx empty = empty

// 2 Pascal Triangle

let f curRow =
  let rec f' = function
      []             -> [] 
    | [_]            -> [1]
    | xs             -> let (x1::x2::xs) = xs
                        x1 + x2 :: f' (x2::xs)
  (1 :: f' curRow)

// Q2.1
f [1]
f [1;1]
f [1;2;1]
f [1;3;3;1]

// For an empty list, f returns and one element list
// For a one element list, f returns the two element list [1;1].
// For a list [x0;...;xN], f returns a new list with N+1 elements computed as
// [1;x0+x1;x1+x2;...;xN-1+xN;1]
// This happens to be Pascals triangle, that is, each element is the
// sum of the two elements above. It is not necessary to know it is Pascals
// triangle to get full mark. Simply a description as the lines above.

// Q2.2
// The warning happens because the compiler does not know that xs in
// let (x1::x2::xs) = xs will always have atleast 2 elements.

let fMatch curRow =
  let rec fMatch' = function
      []             -> [] 
    | [_]            -> [1]
    | x1::x2::xs     -> x1+x2 :: fMatch' (x2::xs)
  (1 :: fMatch' curRow)

f [1] = fMatch [1]
f [1;1] = fMatch [1;1]
f [1;2;1] = fMatch [1;2;1]
f [1;3;3;1] = fMatch [1;3;3;1]

// Not Used
let rec pascal curRow = function
    0 -> []
  | n -> let nr = f curRow
         nr :: (pascal nr (n-1))
// Q2.3
let fA curRow =
  let rec fA' acc = function
      []             -> List.rev acc
    | [_]            -> List.rev (1::acc)
    | x1::x2::xs     -> fA' (x1+x2::acc) (x2::xs)
  (1 :: fA' [] curRow)

// As pascals triangle is the same forward and backwards you don't
// really need the reverse, but if you skip it a note is required as
// this is really a special case.

fA [1] 
fA [1;1]
fA [1;2;1]
fA [1;3;3;1] 
  
// 3 Sequences

// Q3.1
let mySeq s1 s2 =
  seq { for e1 in s1 do
          for e2 in s2 do
            yield! [e1;e2] }

// With Ns1 and Ns2 elements in s1 and s2, the sequence returns
// Ns1*Ns2 elments, assuming s1 and s2 is finite. Otherwise infinite
// number of elements. Asuming s1 = [s11;s12;...] and s2 = [s21;s22;...]
// the returned seuqence is [s11;s21 ; s11;s22 ; ... ; s12;s21 ; s12,s22; ...]

mySeq ['A';'B'] ['D';'E';'F']

// Q3.2
let mySeq2 s1 s2 =
  seq { for e1 in s1 do
          for e2 in s2 do
            yield (e1,e2) }

mySeq2 [1;2] ['A';'B';'C']

// Q3.3
let mySeq3 n = Seq.initInfinite (fun i -> n*n - n*i)
mySeq3 3
  
// 4 Data Generator

type DataSpec =
    RangeInt of int * int
  | ChoiceString of string list
  | StringSeq of string    
  | Pair of DataSpec * DataSpec
  | Repeat of int * DataSpec

let reg =
  Repeat(3,Pair(StringSeq "a",
                Pair(ChoiceString["cheese";"herring";"soft drink"],
                     RangeInt(1,100))))

// Q4.1
let pur = Repeat(2,Pair(RangeInt(1,10), StringSeq "a"))

// Q4.2
let rand = System.Random()
let next(i1,i2) = rand.Next(i1,i2) // Interval [i1,...,i2[ 
let numGen =
  let n = ref 0
  fun () -> n := !n+1; !n

let rec genValue = function
    RangeInt(i1,i2) -> next(i1,i2+1).ToString()
  | ChoiceString xs -> let idx = next(0,List.length xs)
                       List.item idx xs
  | StringSeq s -> s + numGen().ToString()
  | Pair(ds1,ds2) -> "("+(genValue ds1)+","+(genValue ds2)+")"
  | Repeat (n,ds) -> "["+(String.concat ";" (Seq.init n (fun _ -> genValue ds)))+"]"

genValue reg

// In case you want to implement Repeat as a recursive function call
let rec genValue = function
    RangeInt(i1,i2) -> next(i1,i2+1).ToString()
  | ChoiceString xs -> let idx = next(0,List.length xs)
                       List.item idx xs
  | StringSeq s -> s + numGen().ToString()
  | Pair(ds1,ds2) -> "("+(genValue ds1)+","+(genValue ds2)+")"
  | Repeat (n,ds) ->
      let rec repeat acc = function
         0 -> acc + "]"
       | 1 -> acc + genValue ds + "]"
       | n -> repeat (acc + genValue ds + ";") (n-1)
      repeat "[" n

genValue reg

// Q4.3
type DataSpec =
    RangeInt of int * int
  | ChoiceString of string list
  | StringSeq of string    
  | Pair of DataSpec * DataSpec
  | Repeat of int * DataSpec
  | Pick of string
  | Label of string * DataSpec

let reg2 = Repeat(3,Pair(Label("articleCode",StringSeq "a"),
                         Pair(ChoiceString["cheese";"herring";"soft drink"],
                              RangeInt(1,100))))
let pur2 = Repeat(2,Pair(RangeInt(1,10), Pick "articleCode"))

type Env = Map<string,string list>

let addToEnv s v (dEnv:Env) :Env =
  match Map.tryFind s dEnv with
   Some vs -> Map.add s (v::vs) dEnv
 | None -> Map.add s [v] dEnv

let env = Map.ofList[("x",["43"])]
let env = addToEnv "x" "42" env

let pickFromEnv s (dEnv:Env) =
  match Map.tryFind s dEnv with
    Some vs -> List.item (next(0,List.length vs)) vs
  | None -> failwith "pickFromEnv: label does not exists in environment."

pickFromEnv "x" env

// Q4.4

let rec genValue dEnv = function
    RangeInt(i1,i2) -> (next(i1,i2+1).ToString(),dEnv)
  | ChoiceString xs -> let idx = next(0,List.length xs)
                       (List.item idx xs,dEnv)
  | StringSeq s -> (s + numGen().ToString(),dEnv)
  | Pair(ds1,ds2) ->
    let (v1',dEnv1) = genValue dEnv ds1
    let (v2',dEnv2) = genValue dEnv1 ds2
    ("("+v1'+","+v2'+")", dEnv2)
  | Repeat (n,ds) ->
    let rec iter (dEnv,vs) = function
        0 -> (dEnv,List.rev vs)
      | n when n>0 -> let (v',dEnv') = genValue dEnv ds
                      iter (dEnv',v'::vs) (n-1)
      | _ -> failwith "Repeat with n < 0"
    let (dEnv',vs') = iter (dEnv,[]) n                      
    ("[" + (String.concat ";" vs') + "]",dEnv')
  | Pick s -> (pickFromEnv s dEnv,dEnv)
  | Label (s,ds) ->
    let (v',dEnv') = genValue dEnv ds
    (v', addToEnv s v' dEnv')

let (v,dEnv) = genValue Map.empty reg2
genValue dEnv pur2    

// ALL BELOW NOT USED

type SpecDataF =
    Fields of (string * DataSpec) list

let reg =
  Repeat(3,Pair(StringSeq "a",
                Pair(ChoiceString["cheese";"herring";"soft drink"],
                     RangeInt(1,100))))
let pur = Repeat(2,Pair(RangeInt(1,10),StringSeq "a"))

let reg2 =
  Fields [("reg",Repeat(3,Pair(StringSeq "a",
                               Pair(ChoiceString["cheese";"herring";"soft drink"],
                                    RangeInt(1,100)))))]
let pur2 = Fields [("pur", Repeat(2,Pair(RangeInt(1,10), StringSeq "a")))]




let genSpecDataF specDataF =
  match specDataF with    
    Fields xs -> List.fold (fun acc (s,v) ->
                            (acc + "\n" + s + ": " + (genValue v))) "" xs

let _ = genValue reg
let _ = genValue pur

  
type SpecDataF =
    Fields of (string * DataSpec) list

let basket =
  Fields [("reg",Repeat(3,Pair(Label("articleCode",StringSeq "a"),
                               Pair(ChoiceString["cheese";"herring";"soft drink"],
                                    RangeInt(1,100)))));
          ("pur",Repeat(2,Pair(Pick "articleCode", RangeInt(1,10))))]

let reg2 = Repeat(3,Pair(Label("articleCode",StringSeq "a"),
                         Pair(ChoiceString["cheese";"herring";"soft drink"],
                              RangeInt(1,100))))
let pur2 = Repeat(2,Pair(RangeInt(1,10), Pick "articleCode"))

let genSpecDataF dEnv = function
    Fields xs -> List.fold (fun (acc,dEnv) (s,v) ->
                            let (v',dEnv') = genValue dEnv v
                            (acc + "\n" + s + ": " + v', dEnv')) ("",dEnv) xs

let _ = genSpecDataF Map.empty basket

