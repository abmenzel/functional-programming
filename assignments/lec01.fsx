let rec fact = function 
    | 0 -> 1
    | n -> n * fact(n-1)

// Exercise 1.1
let sqr x = x * x

// Exercise 1.2
let pow x n = System.Math.Pow(x,n)

// Exercise 1.3
let g x = x + 4

// Exercise 1.4
let h(x:float , y:float) = System.Math.Sqrt(x * x + y * y)

// Exercise 1.5
let rec f = function
    | 0 -> 0
    | x -> f(x-1) + x

// Exercise 1.6
let rec fib = function
    | 0 -> 0
    | 1 -> 1
    | x -> fib(x-1) + fib(x-2)

// Exercise 1.7
let rec sum = function
    | m, 0 -> m
    | m, n -> m + n + sum(m, n-1)

// Exercise 1.8
// float * int
// int
// (float -> float) * int
// (float -> float -> float) * (int -> int)

// Exercise 1.9
// Environment 1:
// a -> 5
// Environment 2:
// f a -> 6
// f a -> a + 1
// Environment 3:
// g b -> b + 1 + 6
// g b -> b + 1 + a

// f 3 -> 4
// g 3 -> 10

// Exercise 1.10
let dup (a:string) = a + a

// Exercise 1.11
let rec dupn = function
    |s, 0 -> ""
    |s, n -> s + dupn(s,n - 1)
