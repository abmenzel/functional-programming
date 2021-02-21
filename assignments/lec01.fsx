
(* Exercise 1.1 *)
let sqr x = x * x

(* Exercise 1.2 *)
let pow a b = System.Math.Pow(a, b)

let rec fact = function // int -> int
   | 0 -> 1 // if n == 0, evaluate to 1
   | n -> n * fact(n-1) // multiply n with the integer n-1

(* Exercise 1.3 (HR 1.1) *)
let g n = n + 4

(* Exercise 1.4 (HR 1.2) *)
let h(x:float, y:float) = System.Math.Sqrt((x*x) + (y*y))

(* Exercise 1.5 (HR 1.4) *)
let rec f = function
| 0 -> 0
| n -> n + f(n-1)
f 4
// f(4)
// ~> 4 + f(4-1)
// ~> 4 + f(3) 
// ~> 4 + (3 + f(3-1))
// ~> 4 + (3 + f(2))
// ~> 4 + (3 + (2 + f(2-1)))
// ~> 4 + (3 + (2 + f(1)))
// ~> 4 + (3 + (2 + (1 + f(1-1))))
// ~> 4 + (3 + (2 + (1 + f(0))))
// ~> 4 + (3 + (2 + (1 + 0)))
// ~> 4 + (3 + (2 + 1))
// ~> 4 + (3 + 3))
// ~> 4 + 6
// ~> 10

(* Exercise 1.6 (HR 1.5) *)
let rec fib = function
|   0 -> 0
|   1 -> 1
|   n -> fib(n-1)+fib(n-2)
fib 4
// fib 4
// ~> fib(4-1) + fib(4-2)
// ~> fib(3) + fib(2)
// ~> (fib(3-1) + fib(3-2)) + (fib(2-1)+f(2-2))
// ~> (fib(2) + fib(1)) + (fib(1)+f(0))
// ~> ((fib(2-1)+fib(2-2)) + 1) + (1 + 0)
// ~> (fib(1)+fib(0) + 1) + 1
// ~> ((1 + 0) + 1) + 1
// ~> 2 + 1
// ~> 3

(* Exercise 1.7 (HR 1.6) *)
let rec sum = function
| (m, 0) -> m // if n == 0, evaluate to m
| (m, n) -> m + n + sum(m, n-1) // recursively evaluate sum by adding together the sum of m and n with the sum of m and n-1  

(* Exercise 1.8 (HR 1.7) *)
// float * int
// int
// float
// (float * int -> float) * (int -> int)

(* Exercise 1.9 (HR 1.8) *)
// let a = 5
// let f a = a + 1
// let g b = (f b) + a
// environment:
// a -> 5
// f a -> a + 1
// g b -> (f b) + a
// evaluations
// f(3) ~> 3 + 1 ~> 4
// g(3) ~> (f 3) + 5 ~> (3 + 1) + 5 ~> 4 + 5 ~> 9  

(* Exercise 1.10 *)
let dup a:string = a + a
dup "Hi "

(* Exercise 1.11 *)
let rec dupn (s: string) n = 
  match n with
  | 0 -> "" // if n is 0, evaluate to / concatenate the empty string.
  | n when n > 0 -> s + (dupn s (n-1)) // if n is greater than 0, concatenate s n times, recursively
  | _ -> "" // wildcard pattern: if n is a negative number, the function evaluates to an empty string
dupn "Hi " -1
dupn "Hi " 3