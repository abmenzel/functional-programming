type item = {Id: int; Name: string; Price: float}

type pizzareg = item list

let item1 = {Id = 1; Name = "Vesuvio"; Price = 55.00}
let item2 = {Id = 2; Name = "Pepperoni"; Price = 50.00}
let item3 = {Id = 3; Name = "Italiana"; Price = 59.00}
let item4 = {Id = 4; Name = "Capriccosa"; Price = 62.00}

let reg:pizzareg = [item1;item2;item3;item4]

let regDup:pizzareg = (item1::reg)

let emptyReg () : pizzareg = []

let size (r:pizzareg) = List.length r


// Q1.2
let isEmpty (r:pizzareg) = List.isEmpty r

let pId i item = i = item.Id

let pName n item = n = item.Name

let tryFind p (r:pizzareg) = List.tryFind p r

// In the case that there are more than one item that matches the tryFind, the first match will be returned.

let isUniqueById (r:pizzareg) =
    let rec checkAll xs acc =
        match xs with
        | [] -> acc
        | item::xs -> match tryFind (pId item.Id) xs with Some _ -> false | None -> checkAll xs acc
    checkAll r true

isUniqueById regDup

type order = list<(int * int)>

let order1:order = [(2,3);(3,4)]
let order2:order = [(2,3); (1,2); (1,3); (2,4)]

let collectById (order:order) : order=
    List.fold (fun map (id,amount) ->
        match Map.tryFind id map with
        | Some v ->Map.add id (v+amount) map
        | None -> Map.add id amount map
    ) Map.empty order |> Map.toList

collectById order2

let makeOrderList (reg:pizzareg) (order:order) =
    let orders = collectById order
    let getName itemid = match tryFind (pId itemid) reg with Some v -> v.Name | None -> ""
    let getSum itemid amount =
        let price = match tryFind (pId itemid) reg with Some v -> v.Price | None -> 0.0
        price * (float amount)
    List.rev (List.map (fun (id, amount) -> (amount, (getName id), (getSum id amount))) orders)

makeOrderList reg order1


// Q2

let rec f i = function
    [] -> []
    | x::xs -> (x+i) :: g (i+1) xs
    | x1::x2::xs -> (x1+i) :: g (i+1) (x2::xs)
and g i = function
    [] -> [i]
    | x::xs -> (x-i) :: f (i+1) xs

f 10 [1 .. 10]

// Q2.1
// The compiler tells us that x1::x2::xs will never be matched.
// This happens because the pattern above x::xs will always be matched first, if it is possible to take two elements from the list, it is definetely possible to take 1 element from it.

let rec fFix i = function
    [] -> []
    | x::xs -> (x+i) :: g (i+1) xs
and gFix i = function
    [] -> [i]
    | x::xs -> (x-i) :: f (i+1) xs

fFix 10 [1 .. 10]

let rec sum l =
    match l with
    | [] | [_] -> []
    | x1::x2::xs -> (x1+x2)::(sum (x2::xs))

// The result is as expected, as f creates the list [11; -9; 15; -9; 19; -9; 23; -9; 27; -9], when adding these elements together in pairs using addition rules, it results in the list.

// Q2.2
// g and f cannot be considered tail-recursive as they will have to make all recursive calls before making their final computation.
// When using tail recursion computation would happen every step, this is easier on the stack.

let rec fA i acc = function
    [] -> List.rev (acc)
    | x::xs -> gA (i+1) ((x+i)::acc) xs
and gA i acc = function
    [] -> List.rev (i::acc)
    | x::xs -> fA (i+1) ((x-i)::acc) xs

fA 10 [] [1 .. 10]

// There does not exist a call in which we will have an unbounded number of recursive calls.
// The recursive calls are bound by the number of integers in the input list.

// Q3
type name = string
type FamilyTree = Family of name * name * Children list
and Children =
    | Single of name
    | Couple of FamilyTree

let fam1 = Family ("Hanne", "Peter", [])
let fam2 = Family ("Kurt", "Pia", [Single "Henrik"])

// both fam1 and fam2 are of type FamiltyTree, this can be seen to the right of the declarations denoted by // FamilyTree

let fam3 = Family ("Charlotte", "Oliver", [Couple (fam1); Couple (fam2)])

// Q3.2
let rec numPerFam ft =
    match ft with
    | Family (_,_,ch) -> 2 + (List.fold (fun acc ch -> acc + numPerChildren ch) 0 ch)
and numPerChildren ch =
    match ch with
    | Single _ -> 1
    | Couple f -> numPerFam f

numPerFam fam3
// 7 members in fam3

let rec toListFam ft =
    match ft with
    | Family (n1,n2,ch) -> n1::n2::(List.fold (fun acc ch -> acc @ (toListChildren ch)) [] ch)
and toListChildren ch : list<name> =
    match ch with
    | Single n -> [n]
    | Couple f -> toListFam f 

toListFam fam3
// fam3 is: ["Charlotte"; "Oliver"; "Hanne"; "Peter"; "Kurt"; "Pia"; "Henrik"]

// Q3.3
let singlePerson = "Henrik"
let child = Single "Henrik Junior"
let singleFam = Family (singlePerson, singlePerson, [child])
// In the case of singleFam we have created a family where the two parents refer to the same person "Henrik".

// Q4
let rec F n =
    match n with
    0 -> 1
    | n when n > 0 -> n-(M (F (n-1)))
and M n =
    match n with
    0 -> 0
    | n when n > 0 -> n - F (M (n-1))

let combineFM n = (F n, M n)

// Q4.2
let Fseq = Seq.initInfinite (fun i -> F i)

let FseqCache = Seq.cache Fseq

// Fseq and FseqCache have the same type as caching simply means that the value is computed once and the re-used if called again with the same parameters.

let combineFMSeq = Seq.initInfinite combineFM
Seq.take 4 combineFMSeq


