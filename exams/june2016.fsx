// Q1
type Multiset<'a when 'a: comparison> = MSet of Map<'a, int>

let ex =
    MSet(
        Map.ofList [ ("a", 1)
                     ("b", 2)
                     ("c", 1) ]
    )

let wrong =
    MSet(
        Map.ofList [ ("a", 0)
                     ("b", 2)
                     ("c", 1) ]
    )

// Q1.1
let diceSet =
    MSet(
        Map.ofList [ (1, 2)
                     (2, 1)
                     (3, 5)
                     (4, 0)
                     (5, 2)
                     (6, 2) ]
    )
// the type of the value diceSet is MSet

// It is not possible to represent the system functions as a multiset as float types, as the function itself is not of type float.

// Q1.2
let newMultiSet () = MSet(Map.empty)

let isEmpty (MSet ms) = ms.IsEmpty

// Q1.3
let add k (MSet ms) =
    match Map.tryFind k ms with
    | Some v -> MSet(Map.add k (v + 1) ms)
    | None -> MSet(Map.add k 1 ms)

let del k (MSet ms) = MSet(Map.remove k ms)

// Q1.4
let toList (MSet ms) =
    let msList = Map.toList ms

    let rec inner ls acc =
        match ls with
        | [] -> acc
        | (k, v) :: tl -> inner tl (acc @ (Seq.toList (Seq.init v (fun _ -> k))))

    inner msList []

toList ex

let fromList ls =
    let rec inner ls acc =
        match ls with
        | [] -> acc
        | k :: tl -> inner tl (add k acc)
    inner ls (MSet Map.empty)


// Q1.5
let map f ms = fromList (List.map (fun k -> f k) (toList ms))
// Might cause issues check Niels slides

map (fun (c:string) -> c.ToUpper()) ex

let fold f a ms = List.fold (fun s k -> f s k) a (toList ms)

fold (fun acc e -> acc+e) "" ex

// Q1.6
let union ms1 (MSet ms2) =
    let list = Map.toList ms2
    let rec inner ls acc =
        match ls with
        | [] -> acc
        | (k,v)::xs when v > 0 -> inner ((k,v-1)::xs) (add k acc)
        | (k,_)::xs -> inner xs (add k acc)
    inner list ms1

let minus ms1 ms2 = List.fold (fun state key -> del key state) ms1 (toList ms2)


// Q2.1
let rec f n =
    if n < 10 then "f" + g (n+1) else "f"
and g n =
    if n < 10 then "g" + f (n+1) else "g"

// Any argument 10 and above passed to f will result in a string that starts and ends with f
// Any even arguments 10 an below, will result in a string that starts and ends with f 

// It is possible to generate gfgfgfg by following:
let gfgfgfg = g 4

// It is not possible to start an infinite computation using g and f

// Q2.2
let rec fA n acc =
    if n < 10 then gA (n + 1) (acc + "f") else (acc + "f")
and gA n acc =
    if n < 10 then fA (n + 1) (acc + "g") else (acc + "g")

let gfgfgfg2 = gA 4 ""

// Q3.1
let myFinSeq (n,m) = seq { for i in [0 .. n] do
    yield! seq { for j in [0 .. m] do yield j }}

// n will determine how many sequences this sequence will consist of,
// so for n many times it will yield a sequence ranging from 0 to m

// myFinSeq not possible

// Q3.2
let myFinSeq2 (n,m) = seq { for i in [0 .. n] do yield i, Seq.init m (fun i -> i)}
myFinSeq2 (2,2)
Seq.take 4 (myFinSeq2 (4,2))

// Q4
type Row = int
type Col = char
type CellAddr = Row * Col
type ArithOp = Add | Sub | Mul | Div
type RangeOp = Sum | Count
type CellDef =
    FCst of float
    | SCst of string
    | Ref of CellAddr
    | RangeOp of CellAddr * CellAddr * RangeOp
    | ArithOp of CellDef * ArithOp * CellDef
type CellValue =
    S of string
    | F of float
type Sheet = Map<CellAddr,CellDef>

// Q4.1
let B4 = ((4, 'B'), SCst "NAME")
let B5 = ((5, 'B'), SCst "Hans")
let B6 = ((6, 'B'), SCst "Trine")
let B7 = ((7, 'B'), SCst "Peter")
let B9 = ((9, 'B'), RangeOp ((5, 'B'), (7, 'B'), Count))
let C4 = ((4, 'C'), SCst "HEIGHT")
let C5 = ((5, 'C'), FCst 167.40)
let C6 = ((6, 'C'), FCst 162.30)
let C7 = ((7, 'C'), FCst 179.70)
let sum = RangeOp ((5, 'C'), (7, 'C'), Sum)
let C9 = ((9, 'C'), ArithOp (sum, Div, Ref (9, 'B')))


let heights = Sheet [B4;B5;B6;B7;B9;C4;C5;C6;C7;C9]

let header = [((1,'A'),SCst "#EYES");((1,'B'),SCst "1");((1,'C'),SCst "2");
    ((1,'D'),SCst "3");((1,'E'),SCst "4");((1,'F'),SCst "5");
    ((1,'G'),SCst "6");((1,'H'),SCst "Total")]
let result = [((2,'A'),SCst "RESULT");((2,'B'),FCst 2.0);((2,'C'),FCst 1.0);
    ((2,'D'),FCst 5.0);((2,'E'),FCst 0.0);((2,'F'),FCst 2.0);
    ((2,'G'),FCst 2.0);((2,'H'),RangeOp((2,'B'),(2,'G'),Sum))]
let calcPct col = ArithOp(FCst 100.0, Mul, ArithOp(Ref(2,col),Div,Ref(2,'H')))
let pct = [((3,'A'),SCst "PCT");((3,'B'),calcPct 'B');((3,'C'),calcPct 'C');
    ((3,'D'),calcPct 'D');((3,'E'),calcPct 'E');((3,'F'),calcPct 'F');
    ((3,'G'),calcPct 'G');((3,'H'),calcPct 'H')]
let dice = Map.ofList (header @ result @ pct)


// Q4.2
let getF = function
    F f -> f
    | S s -> failwith "getF: expecting a float but got a string"

let evalRangeOp xs op =
    match op with
    | Sum -> List.fold (fun acc c -> acc + (getF c)) 0.0 xs
    | Count -> float (List.length xs)

evalRangeOp [F 33.0; F 32.0] Sum
evalRangeOp [F 33.0; F 32.0] Count
evalRangeOp [F 23.0; S "Hans"] Count

let evalArithOp v1 v2 op =
    match op with
    | Add -> getF v1 + getF v2
    | Sub -> getF v1 - getF v2
    | Mul -> getF v1 * getF v2
    | Div -> getF v1 / getF v2

evalArithOp (F 33.0) (F 32.0) Sub


// Q4.3
let cellList ca1 ca2 =
    match ca1, ca2 with
    | (r1,c1),(r2,c2) ->
        Seq.toList (seq { for i in c1 .. c2 do for j in r1 .. r2 -> (j,(char) i)})

let rec evalValue v sheet =
    match v with
    | FCst f -> F f
    | SCst s -> S s
    | Ref ca -> evalCell ca sheet
    | RangeOp (ca1, ca2, op) ->
        let evaluatedCells = List.map (fun cell -> evalCell cell sheet) (cellList ca1 ca2)
        F (evalRangeOp  evaluatedCells op)
    | ArithOp (v1, op, v2) -> F (evalArithOp (getAr v1 sheet) (getAr v2 sheet) op)
and evalCell ca sheet =
    match Map.tryFind ca sheet with
    | None -> S ""
    | Some v -> evalValue v sheet

and getAr v sheet =
    match v with
    | FCst v -> F v
    | Ref v -> evalCell v sheet
    | ArithOp (v1,op,v2) -> F (evalArithOp (evalValue v1 sheet) (evalValue v2 sheet) op)

evalCell (3,'G') dice

// Q4.4
let ppBoard sheet =

    // Print headers
    Map.iter (fun (row,col) v ->
        if row = 1 then
            printf "     %c |" col
        ) sheet
    
    printf "\n"
    // Print second row
    Map.iter (fun (row,col) v ->
        if row = 1 then
            printf "-------+"
        ) sheet 

ppBoard dice