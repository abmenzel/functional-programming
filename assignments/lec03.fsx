// Exercise 2.1 downTo + downTo2
let rec downTo n = 
    if n = 1 then [n]
    elif n > 1 then n::downTo(n-1)
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
    | x::y::xs -> x::removeOddIdx xs // don't include the odd index

// Exercise 2.3 combinePair
let rec combinePair (xs :int list) =
    match xs with
    | x::y::xs -> (x, y)::combinePair(xs)
    | _ -> []

// Exercise 2.4 - HR 3.2 - British currency

// Money tuple addition
let (^+^) a b =
    match (a,b) with
    | (x1, y1, z1), (x2, y2, z2) when x2 >= 0 && y2 >= 0 && z2 >= 0 -> // allow for negative balance
        let penceA = x1 * 20 * 12 + y1 * 12 + z1
        let penceB = x2 * 20 * 12 + y2 * 12 + z2

        let newPenceAmount = penceA + penceB 

        let newPence = newPenceAmount % 12
        let newShillings = newPenceAmount / 12 % 20
        let newPounds = newPenceAmount / 12 / 20

        (newPounds, newShillings, newPence)

// Money tuple subtraction
let (^-^) a b =
    match (a,b) with
    | (x1, y1, z1), (x2, y2, z2) when x2 >= 0 && y2 >= 0 && z2 >= 0 -> // allow for negative balance
        let penceA = x1 * 20 * 12 + y1 * 12 + z1
        let penceB = x2 * 20 * 12 + y2 * 12 + z2

        let newPenceAmount = penceA - penceB 

        let newPence = newPenceAmount % 12
        let newShillings = newPenceAmount / 12 % 20
        let newPounds = newPenceAmount / 12 / 20

        (newPounds, newShillings, newPence)

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

// Exercise 2.5 - HR 3.3 - Complex numbers

//      1. Declare infix for addition and multiplication
let ( .+) (a:float,b:float) (c:float,d:float) = (a + c, b + d)

let ( .*) (a:float,b:float) (c:float,d:float) = (a * c - b * d, b * c + a * d)

//      2. Declare infix for subtraction and division
let ( .-) (a:float,b:float) (c:float,d:float) = (-a + -c, -b + -d)

let ( ./) (a,b) (c,d) =
    match (a,b) with
    | (0.0, 0.0) -> failwith "a and b cannot both be zero"
    | _ -> 
        let aInverse = a/(a**2.0 + b**2.0)
        let bInverse = (-b)/(a**2.0 + b**2.0)
        let cInverse = c/(c**2.0 + d**2.0)
        let dInverse = (-d)/(c**2.0 + d**2.0)
        ((aInverse * cInverse) - (bInverse * dInverse), (bInverse * cInverse) + (aInverse * dInverse))

//      3. Use 'let' expressions in division to avoid repeated evals
let ( ../) (a:float,b:float) (c:float,d:float) =
    match (a,b) with
    | (0.0, 0.0) -> failwith "a and b cannot both be zero"
    | _ ->
        let aX = a**2.0
        let bX = b**2.0
        let cX = c**2.0
        let dX = d**2.0
        
        let abX = aX + bX
        let cdX = cX + dX

        let aInv = a/abX
        let bInv = -b/abX
        let cInv = c/cdX
        let dInv = -d/cdX

        (aInv, bInv) .* (cInv, dInv)

// Exercise 2.6 - HR 4.4 - altSum -> HR page 76
// function alternating between adding and subtracting the contents of a list.
let rec altsum = function
    | [] -> 0
    | x::xs -> x - altsum xs