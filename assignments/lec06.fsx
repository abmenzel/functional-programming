
// 6.1 (HR 6.2)
type Fexpr = 
    | Const of float
    | X
    | Add of Fexpr * Fexpr
    | Sub of Fexpr * Fexpr
    | Mul of Fexpr * Fexpr
    | Div of Fexpr * Fexpr
    | Sin of Fexpr
    | Cos of Fexpr
    | Log of Fexpr
    | Exp of Fexpr;;

let rec fexprToString expr =
    match expr with
    | Const(x) -> string(x)
    | X -> "x"
    | Add(x,y) -> fexprToString x + " " + fexprToString y + " +"
    | Sub(x,y) -> fexprToString x + " " + fexprToString y + " -"
    | Mul(x,y) -> fexprToString x + " " + fexprToString y + " *"
    | Div(x,y) -> fexprToString x + " " + fexprToString y + " /"
    | Sin(x) -> fexprToString x + " sin"
    | Cos(x) -> fexprToString x + " cos"
    | Log(x) -> fexprToString x + " log"
    | Exp(x) -> fexprToString x + " exp"

let test = Add(Const(2.0),X)
//fexprToString test

// 6.2 (HR 6.8)

type Stack = S of float list

type Instruction = | ADD | SUB | MULT | DIV | SIN
                   | COS | LOG | EXP | PUSH of float


let rec intpInstr s (instr: Instruction) =
    let (S oldStack) = s

    match instr with
    | PUSH(x) ->
        let newStack = x::oldStack
        S newStack
    | ADD ->
        let x = oldStack.[0]
        let y = oldStack.[1]
        intpInstr (S oldStack.Tail.Tail) (PUSH(x + y))
    | SUB ->
        let x = oldStack.[0]
        let y = oldStack.[1]
        intpInstr (S oldStack.Tail.Tail) (PUSH(y - x))
    | MULT ->
        let x = oldStack.[0]
        let y = oldStack.[1]
        intpInstr (S oldStack.Tail.Tail) (PUSH(x * y))
    | DIV ->
        let x = oldStack.[0]
        let y = oldStack.[1]
        intpInstr (S oldStack.Tail.Tail) (PUSH(y / x))
    | SIN ->
        let x = oldStack.[0]
        intpInstr (S oldStack.Tail) (PUSH(sin(x)))
    | COS ->
        let x = oldStack.[0]
        intpInstr (S oldStack.Tail) (PUSH(cos(x)))
    | LOG ->
        let x = oldStack.[0]
        intpInstr (S oldStack.Tail) (PUSH(log(x)))
    | EXP ->
        let x = oldStack.[0]
        intpInstr (S oldStack.Tail) (PUSH(exp(x)))


(* let test = S [1.0;2.0;]
let test2 = ADD;

intpInstr test test2 *)

let intpProg instrL =
    let finalStack = List.fold (fun accStack instr -> intpInstr accStack instr) (S []) instrL
    let (S resultList) = finalStack
    resultList.Head
    

let push1 = PUSH(1.0)
let push2 = PUSH(2.0)
let push3 = PUSH(3.0)
let add1 = ADD
let add2 = ADD
let instrL = [push1;push2;push3;add1;add2]

//intpProg instrL

let trans(fe:Fexpr,x:float) =
    let stringFormat = fexprToString fe
    let stringList = Array.toList(stringFormat.Split(" "))
    let instructionSet =
        List.foldBack (fun instr accList ->
            match instr with
            | "+" -> ADD::accList
            | "-" -> SUB::accList
            | "*" -> MULT::accList
            | "/" -> DIV::accList
            | "sin" -> SIN::accList
            | "cos" -> COS::accList
            | "log" -> LOG::accList
            | "exp" -> EXP::accList
            | "x" -> PUSH(x)::accList
            | _ -> PUSH(float instr)::accList
            )
            stringList [] 
    instructionSet

let f = Add(Const(2.0), Const(2.0))
let f2 = Add(Const(2.0), Const(2.0))
let f3 = Add(Const(5.0), X)
//(trans(f3, 4.0))

// 6.3 (HR 7.2)
// Signature
