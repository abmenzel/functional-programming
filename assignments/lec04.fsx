
// Exercise 2.7 - explode - string to char list
let explode (s:string) =
    List.ofArray(s.ToCharArray())

let rec explode2 (s:string) =
    match s with
    | s when s.Length >= 1 ->
        s.Chars(0)::explode2 (s.Remove(0,1))
    | _ -> []

// Exercise 2.8 - implode - char list to string
let implode list =
    List.foldBack (fun (x:char) (y:string) -> string(x) + y) list ""

let rec implode2 list =
    match list with
    | x::list -> string(x) + implode2 list 
    | _ -> ""

let implodeRev list =
    List.fold (fun (x:string) (y:char) -> string(y) + x) "" list

// Exercise 2.9 - toUpper
let toUpper (s:string) = implode (List.map System.Char.ToUpper (explode(s)))

// explode >> map >> implode
let toUpper1 (s:string) =
    (explode >> List.map System.Char.ToUpper >> implode) s

// let toUpper2 (s:string) = implode (explode s |> List.map(fun x -> (Char.ToUpper(x))));;
let toUpper2 (s:string) =
    explode(s) |> (implode << List.map System.Char.ToUpper)

// Exercise 2.10 - palindrome - treating empty strings as palindromes too.
let rec palindrome (s:string) =
    match s with
    | s when s.Length < 2 -> true
    | s when s.Chars(0) = s.Chars(s.Length - 1) -> palindrome (s.Remove(s.Length - 1).Remove(0,1))
    | _ -> false

// Exercise 2.11 - ack
let rec ack t =
    match t with
    | 0,n -> n + 1
    | m,n when m > 0 && n = 0 -> ack(m - 1, 1)
    | m,n when m > 0 && n > 0 -> ack(m - 1, ack(m,n-1))
    | _ -> failwith "only use non-negative numbers"

// Exercise 2.12 - time
let time f =
    let start = System.DateTime.Now in
    let res = f () in
    let finish = System.DateTime.Now in 
    (res, finish - start);

let timeArg1 f a =
    time (fun () -> f a)

// Exercise 2.13 - HR 5.4 - downTo2 f n e
let rec downto1 f (n, e) =
    match (n,e) with
    | (n,e) when n <= 0 -> e
    | (n,e) when n > 0 -> f (n, downto1 f (n-1, e))
    | _ -> failwith("does not work")

// factorial function using downto1 for recursion.
let fact n =
    match n with
    | n when n > 0 -> downto1 (fun (x,y) -> x * y) (n-1, n)
    | _ -> failwith("n must be a positive number")

let buildList g n =
    List.fold
        (fun rs x -> x::rs) []
        (g n::downto1 (fun (x,y) -> g x::y) (n-1,[]))

buildList fact 5