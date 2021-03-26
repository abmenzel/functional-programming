(* Assignment 7.1, HR 9.1 *)
(* Not covered by Code Judge *)
let xs = [1;2]

let rec g = function
    0 -> xs
  | n -> let ys = n::g(n-1)
         List.rev ys

g 2

// Resulting stack:
// g 2 = List.rev 2::g 1 -> [2;2;1;1] -> [1;1;2;2]
// g 1 = List.rev 1::g 0 -> [1;1;2] -> [2;1;1]
// g 0 = [1;2]
//
// sf1 { 
// sf0 { xs[] -> [1][] -> [2][]
//
// sf2  { n      [0]
//        xs     [1;2]
//        result [?] }
// sf1  { n      [1]
//        xs     [1;2]
//        result [?] }
// sf0  { n      [2]s
//        xs     [1;2]
//        result [?] }


(* Assignment 7.2, HR 9.3 *)

let sum(m, n) =
    let rec sum(m, n, acc) =
        match n with
        | 0 -> acc
        | _ -> sum(m, n-1, acc + m + n)
    sum(m,n,m)

(* Example *)
sum(10,10) // 165
// 10 + (10 + 1) + (10 + 2) + (10 + 3) + (10 + 4) + (10 + 5) + (10 + 6) + (10 + 7) + (10 + 8) + (10 + 9) + (10 + 10)

(* Assignment 7.3, HR 9.4 *)
let length xs =
    let rec length xs acc = 
        match xs with
        | [] -> 0
        | xs -> length xs.Tail acc+1
    length xs 0

(* Example *)
length [1]

(* Assignment 7.4, HR 9.6 *)
let rec facC n c =
    match n with
    | 0 -> c 1
    | _ -> facC (n-1) (fun res -> c(res * n))

(* Example *)
facC 5 id

(* Assignment 7.5, HR 8.6 *)
let fib n =
    let ni = ref n
    let a = ref 0
    let b = ref 1
    let acc = ref 0
    while !ni > 0 do 
        acc := !a + !b
        b := !a
        a := !acc
        ni := !ni - 1
        
    !acc

(* Example *)
fib 2

(* Assignment 7.6, HR 9.7 *)
let rec fibA n n1 n2 =
    match n with
    | 0 -> n1
    | 1 -> n2
    | _ -> fibA n2 (n1+n2) (n-1)

(* Example *)
fibA 10 0 1


let rec fibC n c =
    match n with
    | 1
    | 2 -> c 1
    | n ->
        let first x =
            let second y =
                c(x + y)
            fibC (n-2) second
        fibC (n-1) first
(* Example *)
fibC 3 id
