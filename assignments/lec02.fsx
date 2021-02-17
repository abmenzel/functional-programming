// 2.1 time difference:
let timediff (h0, m0) (h1, m1) = ((h1*60)+m1) - ((h0*60)+m0)

// 2.2 function minutes
let minutes (h, m) = h*60+m

// 2.3 / HR 2.2
let rec pow (s,n) =
    match n with
    | n when n <= 0 -> ""
    | _ -> s + pow(s, (n - 1))


// 2.4 / HR 2.8
let rec bin (a,b) = 
    match a, b with
    | a, b when a < b || a < 0 || b < 0 -> 0
    | a, b when a = b -> 1
    | _, _ -> bin(a-1,b-1) + bin(a - 1, b)

// 2.5 / HR 2.9
let rec f = function
    | (0,y) -> y
    | (x,y) -> f(x-1, x*y);;

// f is of type int * int -> int
// f terminates for arguments: x=0
// evaluation steps for f(2,3) are: f(1,6) -> f(0,6) -> 6
// y * x!

// 2.6 / HR 2.10
let test(c,e) = if c then e else 0;;
// test is of type bool * int -> int
// result is 0
// result is -1

// 2.7 / HR 2.13 Curry and Uncurry
let curry f x y = f(x,y)

let uncurry g (x,y) = g x y
