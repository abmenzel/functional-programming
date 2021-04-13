// 2013-04-18 * sestoft@itu.dk

// Similar to the development of monad-based interpreters from
// scala/ExpressionsMonads.scala

// Very simple expressions

type expr =
    | CstI of int
    | Prim of string * expr * expr
    | Prim1 of string * expr
    | Prim3 of string * expr * expr * expr

// ------------------------------------------------------------

// Plain evaluator, return type int
let rec eval1 e : int =
    match e with
    | CstI i -> i
    | Prim(op, e1, e2) ->
        let v1 = eval1 e1
        let v2 = eval1 e2
        match op with
        | "+" -> v1 + v2
        | "*" -> v1 * v2
        | "/" -> v1 / v2
    | Prim1(op, e1) ->
        let v1 = eval1 e1
        match op with
        | "ABS" -> abs v1
    | Prim3(op, e1, e2, e3) ->
        let v1 = eval1 e1
        let v2 = eval1 e2
        let v3 = eval1 e3
        match op with
        | "+" -> v1 + v2 + v3
        | "*" -> v1 * v2 * v3
        | "/" -> v1 / v2 / v3

let opEval op v1 v2 : int =
    match op with
    | "+" -> v1 + v2
    | "*" -> v1 * v2
    | "/" -> v1 / v2

let opEval1 op v1 : int =
    match op with
    | "ABS" -> abs v1

let rec eval2 e : int =
    match e with
    | CstI i -> i
    | Prim(op, e1, e2) ->
        let v1 = eval2 e1
        let v2 = eval2 e2
        opEval op v1 v2
    | Prim1(op, e1) ->
        let v1 = eval2 e1
        opEval1 op v1
    | Prim3(op, e1, e2, e3) ->
        let v1 = eval2 e1
        let v2 = eval2 e2
        let v3 = eval2 e3
        opEval op (opEval op v1 v2) v3

type IdentityBuilder() =
    member this.Bind(x, f) = f x
    member this.Return x = x
    member this.ReturnFrom x = x

let identM = new IdentityBuilder();;

let rec eval3 e : int =
    match e with
    | CstI i -> identM { return i }
    | Prim(op, e1, e2) ->
        identM  { let! v1 = eval3 e1
                  let! v2 = eval3 e2
                  return! opEval op v1 v2 }
    | Prim1(op, e1) ->
        identM { let! v1 = eval3 e1
                 return! opEval1 op v1 }
    | Prim3(op, e1, e2, e3) ->
        identM { let! v1 = eval3 e1
                 let! v2 = eval3 e2
                 let! v3 = eval3 e3
                 return! opEval op (opEval op v1 v2) v3}

// ------------------------------------------------------------

// Evaluator that may fail, return type: int option

let rec optionEval1 e : int option =
    match e with
    | CstI i -> Some i
    | Prim(op, e1, e2) ->
        match optionEval1 e1 with
        | None -> None
        | Some v1 ->
            match optionEval1 e2 with
            | None -> None
            | Some v2 ->
                match op with
                | "+" -> Some(v1 + v2)
                | "*" -> Some(v1 * v2)
                | "/" -> if v2 = 0 then None else Some(v1 / v2)
    | Prim3(op, e1, e2, e3) ->
        match optionEval1 e1 with
        | None -> None
        | Some v1 ->
            match optionEval1 e2 with
            | None -> None
            | Some v2 ->
                match optionEval1 e3 with
                | None -> None
                | Some v3 ->
                    match op with
                    | "+" -> Some(v1 + v2 + v3)
                    | "*" -> Some(v1 * v2 * v3)
                    | "/" -> if (v2 = 0 || v3 = 0) then None else Some(v1 / v2 / v3)
    | Prim1(op, e1) ->
        match optionEval1 e1 with
        | None -> None
        | Some v1 ->
            match op with
            | "ABS" -> Some(abs v1)

let opEvalOpt op v1 v2 : int option =
    match op with
    | "+" -> Some(v1 + v2)
    | "*" -> Some(v1 * v2)
    | "/" -> if v2 = 0 then None else Some(v1 / v2)

let opEvalOpt3 op v1 v2 v3 : int option =
    match op with
    | "+" -> Some(v1 + v2 + v3)
    | "*" -> Some(v1 * v2 * v3)
    | "/" -> if v2 = 0 || v3 = 0 then None else Some(v1 / v2 / v3)

let opEvalOpt1 op v1 : int option =
    match op with
    | "ABS" -> Some(abs v1)
                
let rec optionEval2 e : int option =
    match e with
    | CstI i -> Some i
    | Prim(op, e1, e2) ->
        match optionEval2 e1 with
        | None -> None
        | Some v1 ->
            match optionEval2 e2 with
            | None -> None
            | Some v2 -> opEvalOpt op v1 v2
    | Prim3(op, e1, e2, e3) ->
        match optionEval2 e1 with
        | None -> None
        | Some v1 ->
            match optionEval2 e2 with
            | None -> None
            | Some v2 ->
                match optionEval2 e3 with
                | None -> None
                | Some v3 -> opEvalOpt3 op v1 v2 v3
    | Prim1(op, e1) ->
        match optionEval2 e1 with
        | None -> None
        | Some v1 -> opEvalOpt1 op v1

let optionFlatMap (f : 'a -> 'b option) (x : 'a option) : 'b option =
    match x with
    | None   -> None
    | Some v -> f v;;

type OptionBuilder() =
    member this.Bind(x, f) =
        match x with
        | None   -> None
        | Some v -> f v
    member this.Return x = Some x
    member this.ReturnFrom x = x
 
let optionM = OptionBuilder();;

let rec optionEval3 e : int option =
    match e with
    | CstI i -> optionM { return i }
    | Prim(op, e1, e2) ->
        optionM { let! v1 = optionEval3 e1
                  let! v2 = optionEval3 e2
                  return! opEvalOpt op v1 v2 }
    | Prim3(op, e1, e2, e3) ->
        optionM { let! v1 = optionEval3 e1
                  let! v2 = optionEval3 e2
                  let! v3 = optionEval3 e3
                  return! opEvalOpt op (opEval op v1 v2) v3 }
    | Prim1(op, e1) ->
        optionM { let! v1 = optionEval3 e1
                  return! opEvalOpt1 op v1 }

// ------------------------------------------------------------                

// Evaluator that returns a set of results, return type: int Set

let opEvalSet op v1 v2 : int Set =
    match op with
    | "+" -> Set [v1 + v2]
    | "*" -> Set [v1 * v2]
    | "/" -> if v2 = 0 then Set.empty else Set [v1 / v2]
    | "choose" -> Set [v1; v2]

let opEvalSet3 op v1 v2 v3 : int Set =
    match op with
    | "+" -> Set [v1 + v2 + v3]
    | "*" -> Set [v1 * v2 * v3]
    | "/" -> if v2 = 0 || v3 = 0 then Set.empty else Set [v1 / v2 / v3]
    | "choose" -> Set [v1; v2; v3]

let opEvalSet1 op v1 : int Set =
    match op with
    | "ABS" -> Set [abs v1]

let rec setEval1 e : int Set =
    match e with
    | CstI i -> Set [i]
    | Prim(op, e1, e2) ->
        let s1 = setEval1 e1
        let yss = Set.map (fun v1 ->
                       let s2 = setEval1 e2
                       let xss = Set.map (fun v2 -> opEvalSet op v1 v2) s2
                       Set.unionMany xss)
                      s1
        Set.unionMany yss
    | Prim3(op, e1, e2, e3) ->
        let s1 = setEval1 e1
        let yss = Set.map (fun v1 ->
                    let s2 = setEval1 e2
                    let xss = Set.map (fun v2 ->
                                   let s3 = setEval1 e3
                                   let uss = Set.map (fun v3 -> opEvalSet3 op v1 v2 v3) s3
                                   Set.unionMany uss)
                                  s2
                    Set.unionMany xss)
                    s1
        Set.unionMany yss
    | Prim1(op, e1) ->
        let s1 = setEval1 e1
        let ys = Set.map (fun v1 -> opEvalSet1 op v1) s1
        Set.unionMany ys

let setFlatMap (f : 'a -> 'b Set) (x : 'a Set) : 'b Set =
    Set.unionMany (Set.map f x);;

type SetBuilder() =
    member this.Bind(x, f) =
        Set.unionMany (Set.map f x)
    member this.Return x = Set [x]
    member this.ReturnFrom x = x
 
let setM = SetBuilder();;

let rec setEval3 e : int Set =
    match e with
    | CstI i -> setM { return i }
    | Prim(op, e1, e2) ->
        setM { let! v1 = setEval3 e1
               let! v2 = setEval3 e2
               return! opEvalSet op v1 v2 }
    | Prim3(op, e1, e2, e3) ->
        setM { let! v1 = setEval3 e1
               let! v2 = setEval3 e2
               let! v3 = setEval3 e3
               return! opEvalSet op (opEval op v1 v2) v3 }
    | Prim1(op, e1) ->
        setM { let! v1 = setEval3 e1
               return! opEvalSet1 op v1}


// ------------------------------------------------------------

// Evaluator that records sequence of operators used,
// return type: int trace

let random = new System.Random()

type 'a trace = string list * 'a

let opEvalTrace op v1 v2 : int trace =
    match op with
    | "+" -> (["+"], v1 + v2)
    | "*" -> (["*"], v1 * v2)
    | "/" -> (["/"], v1 / v2)
    | "choose" -> (["choose"], if random.NextDouble() > 0.5 then v1 else v2)

let opEvalTrace3 op v1 v2 v3 : int trace =
    match op with
    | "+" -> (["+"], v1 + v2 + v3)
    | "*" -> (["*"], v1 * v2 * v3)
    | "/" -> (["/"], v1 / v2 / v3)
    | "choose" ->
        let rand = random.NextDouble()
        (["choose"], if rand > 0.33 then v1 elif rand > 0.66 then v2 else v3)

let opEvalTrace1 op v1 : int trace =
    match op with
    | "ABS" -> (["ABS"], v1)

let rec traceEval1 e : int trace =
    match e with
    | CstI i -> ([], i)
    | Prim(op, e1, e2) ->
        let (trace1, v1) = traceEval1 e1
        let (trace2, v2) = traceEval1 e2
        let (trace3, res) = opEvalTrace op v1 v2
        (trace1 @ trace2 @ trace3, res)
    | Prim3(op, e1, e2, e3) ->
        let (trace1, v1) = traceEval1 e1
        let (trace2, v2) = traceEval1 e2
        let (trace3, v3) = traceEval1 e3
        let (trace4, res) = opEvalTrace3 op v1 v2 v3
        (trace1 @ trace2 @ trace3 @ trace4, res)
    | Prim1(op, e1) ->
        let (trace1, v1) = traceEval1 e1
        let (trace2, res) = opEvalTrace1 op v1
        (trace1 @ trace2, res)

let traceFlatMap (f : 'a -> 'b trace) (x : 'a trace) : 'b trace =
    let (trace1, v) = x
    let (trace2, res) = f v
    (trace1 @ trace2, res)

type TraceBuilder() =
    member this.Bind(x, f) =
        let (trace1, v) = x
        let (trace2, res) = f v
        (trace1 @ trace2, res)
    member this.Return x = ([], x)
    member this.ReturnFrom x = x
 
let traceM = TraceBuilder();;

let rec traceEval3 e : int trace =
    match e with
    | CstI i -> traceM { return i }
    | Prim(op, e1, e2) ->
        traceM { let! v1 = traceEval3 e1
                 let! v2 = traceEval3 e2
                 return! opEvalTrace op v1 v2 }
    | Prim3(op, e1, e2, e3) ->
        traceM { let! v1 = traceEval3 e1
                 let! v2 = traceEval3 e2
                 let! v3 = traceEval3 e3
                 return! opEvalTrace3 op v1 v2 v3 }  
    | Prim1(op, e1) ->
        traceM { let! v1 = traceEval3 e1
                 return! opEvalTrace1 op v1 }

// ------------------------------------------------------------

let expr1 = Prim("+", CstI(7), Prim("*", CstI(9), CstI(10)))
let expr2 = Prim("+", CstI(7), Prim("/", CstI(9), CstI(0)))
let expr3 = Prim("+", CstI(7), Prim("choose", CstI(9), CstI(10)))
let expr4 = Prim("choose", CstI(7), Prim("choose", CstI(9), CstI(13)))
let expr5 = Prim("*", expr4, Prim("choose", CstI(2), CstI(3)))

let expr10 = Prim1("ABS", Prim("+", CstI(7), Prim("*", CstI(-9), CstI(10))))

let expr12 = Prim("+", CstI(7), Prim("choose", Prim1("ABS", CstI(-9)), CstI(10)))

(*
    Exercise 3.1. 
Make both a standard explicit version of (A) and a monadic version.
You need to create a new monad OptionTraceABuilder, among other things.
*)
type OptionTraceABuilder() =
    member this.Bind(x, f) =
        match x with
        | None -> None
        | Some v -> f v
    member this.Return x = ([], x)
    member this.ReturnFrom x = x

let rec explOptTraceEvalA e : int trace option =
    

let rec optTraceEvalA e = failwith "3.1 not implemented"

(*
    Exercise 3.2. 
Make both a standard explicit version of (B) and a monadic version.
You need to create a new monad OptionTraceBBuilder, amongother things.
*)

let rec optTraceEvalB e : int option trace = failwith "3.2 not implemented"

let rec explOptTraceEvalB e : int option trace = failwith "3.2 not implemented"
