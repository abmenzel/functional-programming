module a2
// Exercise 2.1 downTo + downTo2
let rec downTo n = 
    if n = 1 then [n]
    elif n > 1 then n::(downTo(n-1))
    else failwith "number must be greater than 1"

let rec downTo2 n =
    match n with
    | 1 -> [n]
    | n when n > 1 -> n::downTo(n-1)
    | _ -> failwith "number must be greater than 1"


// Exercise 2.2 removeOddIdx
let rec removeOddIdx (xs: int list) =
    match xs with
    | [] -> []
    | x::xs when x % 2 = 0 -> x::removeOddIdx xs
    | x::xs -> removeOddIdx xs

let rec removeOddIdx (xs: int list) =
    match xs with
    | [] -> []
    | x::y::xs -> x::removeOddIdx xs
    
removeOddIdx [-3;-2;-1;0;1;2];;

// Exercise 2.3 combinePair
let rec combinePair (xs :int list) =
    match xs with
    | x::y::xs -> (x, y)::combinePair(xs)
    | _ -> []

combinePair [-3;-2;-1;0;1];;
combinePair [-3];;

// Exercise 2.4 - HR 3.2 - British currency

// Money tuple addition
let (^+^) a b =
    match (a,b) with
    | (x1, y1, z1), (x2, y2, z2) ->
        let penceA = x1 * 20 * 12 + y1 * 12 + z1
        let penceB = x2 * 20 * 12 + y2 * 12 + z2

        let newPenceAmount = penceA + penceB 

        let newPence = newPenceAmount % 12
        let newShillings = newPenceAmount / 12 % 20
        let newPounds = newPenceAmount / 12 / 20

        (newPounds, newShillings, newPence)

(2,2,1) ^+^ (4,5,12);;

// Money tuple subtraction
let (^-^) a b =
    match (a,b) with
    | (x1, y1, z1), (x2, y2, z2) ->
        let penceA = x1 * 20 * 12 + y1 * 12 + z1
        let penceB = x2 * 20 * 12 + y2 * 12 + z2

        let newPenceAmount = penceA - penceB 

        let newPence = newPenceAmount % 12
        let newShillings = newPenceAmount / 12 % 20
        let newPounds = newPenceAmount / 12 / 20

        (newPounds, newShillings, newPence)

(4,5,12) ^-^ (2,6,13);;

type Money = {Pound : int; Shilling : int; Pence : int};;
// Money record addition
let (|+|) a b =
    let penceA = a.Pound * 20 * 12 + a.Shilling * 12 + a.Pence
    let penceB = b.Pound * 20 * 12 + b.Shilling * 12 + b.Pence

    let newPenceAmount = penceA + penceB 

    let newPence = newPenceAmount % 12
    let newShillings = newPenceAmount / 12 % 20
    let newPounds = newPenceAmount / 12 / 20

    let newMoneyBag = {Pound = newPounds; Shilling = newShillings; Pence = newPence}
    newMoneyBag

let moneyBag1 = {Pound = 1; Shilling = 4; Pence = 14}
let moneyBag2 = {Pound = 2; Shilling = 4; Pence = 5}

moneyBag1 |+| moneyBag2;;

type Money = {Pound : int; Shilling : int; Pence : int};;
// Money record subtraction
let (|-|) a b =
    let penceA = a.Pound * 20 * 12 + a.Shilling * 12 + a.Pence
    let penceB = b.Pound * 20 * 12 + b.Shilling * 12 + b.Pence

    let newPenceAmount = penceA - penceB 

    let newPence = newPenceAmount % 12
    let newShillings = newPenceAmount / 12 % 20
    let newPounds = newPenceAmount / 12 / 20

    let newMoneyBag = {Pound = newPounds; Shilling = newShillings; Pence = newPence}
    newMoneyBag

let moneyBag1 = {Pound = 4; Shilling = 5; Pence = 12}
let moneyBag2 = {Pound = 2; Shilling = 6; Pence = 13}

moneyBag1 |-| moneyBag2;;


// Exercise 2.5 - HR 3.3 - Complex numbers

//      1. Declare infix for addition and multiplication
let ( .+) (a:float,b:float) (c:float,d:float) = (a + c, b + d)
(2.0,3.0) .+ (7.0,11.0);;

let ( .*) (a:float,b:float) (c:float,d:float) = (a * c - b * d, b * c + a * d)
(2.0,3.0) .* (7.0,11.0);;

//      2. Declare infix for subtraction and division
let ( .-) (a:float,b:float) (c:float,d:float) = (a - c, b - d)
(2.0,3.0) .- (7.0,11.0);;

let ( ./) (a,b) (c,d) = (a / c - b / d, b / c + a / d)
(2.0,3.0) ./ (7.0,11.0);;

//      3. Use 'let' expressions in division to avoid repeated evals
let ( ../) (a:float,b:float) (c:float,d:float) = failwith "not implemented"


// Exercise 2.6 - HR 4.4 - altSum -> HR page 76
// function alternating between adding and subtracting the contents of a list.
let rec altsum = function
    | [] -> 0
    | x::xs -> x - altsum xs

altsum[2; -1;3]


