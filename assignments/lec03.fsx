// Exercise 2.1 downTo + downTo2
let rec downTo n = 
    if n=0 then []
    elif n < 0 then failwith "not allowed"
    else n::downTo(n - 1)

let rec downTo2 = function
    | 0 -> []
    | n when n < 0 -> failwith "not allowed"
    | n -> n::downTo2(n - 1)

// Exercise 2.2 removeOddIdx
let rec removeOddIdx (xs: int list) =
    match xs with
    | [] -> []
    | [x] -> [x]
    | x::y::xs -> x::removeOddIdx xs // we don't include the odd index. y is odd, because lists start at 0

// Exercise 2.3 combinePair
let rec combinePair (xs :int list) =
    match xs with
    | [] -> []
    | x::y::xs -> (x,y)::combinePair(xs)
    | _ -> []

// // Exercise 2.4 - HR 3.2 - British currency

// Money tuple addition
let (^+^) a b = 
    match a,b with// 'a -> 'b -> 'c
    | (x, y, z),(x1,y1,z1) when x >= 0 && y >= 0 && z >= 0 && x1 >= 0 && y1 >= 0 && z1 >= 0 -> // I assume you cannot have a negative balance or add a negative amount
        let pence = z + z1
        let shilling = y + y1 + (pence/12)
        let pound = x + x1 + (shilling/20)
        (pound, shilling % 20, pence % 12)
    | _ -> failwith "invalid!"

// Money tuple subtraction
let (^-^) a b = 
    match a,b with 
    | (x, y, z),(x1,y1,z1) when x >= 0 && y >= 0 && z >= 0 && x1 >= 0 && y1 >= 0 && z1 >= 0 -> 
        let totalpence = (z - z1) + ((y - y1) * 12) + (((x - x1) * 20 ) * 12) // the sum of pounds, shillings and pence after the subtraction
        (^+^)(0,0,0)(0, 0, (totalpence)) // we borrow the function from above to get the value in pound, shillings and pence. This method raises an excepted if totalpence is negative
    | _ -> failwith "invalid!"

type Money = {pound : int; shilling : int; pence : int};;
// Money record addition
let (|+|) a b = 
    match a,b with
    | {Money.pound = x;Money.shilling = y;Money.pence = z },
      {Money.pound = x1;Money.shilling = y1;Money.pence = z1} 
        when x >= 0 && y >= 0 && z >= 0 && x1 >= 0 && y1 >= 0 && z1 >= 0 ->
            let pence = z + z1
            let shilling = y + y1 + (pence/12)
            let pound = x + x1 + (shilling/20)
            {Money.pound = pound; Money.shilling = shilling % 20; Money.pence = pence % 12}
    | _ -> failwith "invalid"
    
// (|+|){pound = 0; shilling = 0; pence = 11}{pound = 0; shilling = 0; pence = 1}

// Money record subtraction
let (|-|) a b =
    match a,b with
    | {Money.pound = x;Money.shilling = y;Money.pence = z },
      {Money.pound = x1;Money.shilling = y1;Money.pence = z1} 
        when x >= 0 && y >= 0 && z >= 0 && x1 >= 0 && y1 >= 0 && z1 >= 0 ->
            let totalpence = (z - z1) + ((y - y1) * 12) + (((x - x1) * 20 ) * 12)
            (|+|){pound = 0; shilling = 0; pence = 0}{pound = 0; shilling = 0; pence = totalpence}
    | _ -> failwith "invalid!"

// (|-|){pound = 12; shilling = 5; pence = 3}{pound = 12; shilling = 5; pence = 11}


// Exercise 2.5 - HR 3.3 - Complex numbers

//      1. Declare infix for addition and multiplication
let ( .+) (a:float,b:float) (c:float,d:float) = (a + c, b + d)

let ( .*) (a:float,b:float) (c:float,d:float) = ((a * c)-(b * d), ((b * c)+(a * d)))

//      2. Declare infix for subtraction and division
let ( .-) (a:float,b:float) (c:float,d:float) = (a + -c, b + -d)

let ( ./) (a,b) (c,d) =
    match (a,b) with
    | (0.0, 0.0) -> failwith "a and b cannot be both zero"
    | _ ->   
        let invC:float = c/(c**2.0+d**2.0)
        let invD:float = (-d)/(c**2.0+d**2.0)
        (a,b) .* (invC, invD)

//      3. Use 'let' expressions in division to avoid repeated evals
let ( ../) (a:float,b:float) (c:float,d:float) =
    match (a,b) with
    | (0.0, 0.0) -> failwith "a and b cannot be both zero"
    | _ ->
        let powCD = c**2.0 + d**2.0
        let invC = c/(powCD)
        let invD = (-d)/(powCD)
        (a, b) .* (invC, invD)

// Exercise 2.6 - HR 4.4 - altSum -> HR page 76
// function alternating between adding and subtracting the contents of a list.
let rec altsum = function
    | [] -> 0
    | x::xs -> x - altsum xs

