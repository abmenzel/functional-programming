
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

let implode2 list = failwith "not implemented"

let implodeRev list =
    List.fold (fun (x:string) (y:char) -> string(y) + x) "" list

// Exercise 2.9 - toUpper
let toUpper (s:string) = implode (List.map System.Char.ToUpper (explode(s)))

// explode >> map >> implode
let toUpper1 (s:string) =
    (explode >> List.map System.Char.ToUpper >> implode) s

toUpper1 ("Hej");;

// let toUpper2 (s:string) = implode (explode s |> List.map(fun x -> (Char.ToUpper(x))));;
let toUpper2 (s:string) =
    explode(s) |> (implode << List.map System.Char.ToUpper)

toUpper2 ("Hej");;

// Exercise 2.10 - palindrome - treating empty strings as palindromes too.
let rec palindrome (s:string) = failwith "not implemented"
// Exercise 2.11 - ack
let rec ack t = failwith "not implemented"
    // ack(3, 11) = 1638ss1

// Exercise 2.12 - time
let time f = failwith "not implemented"
    
let timeArg1 f a = failwith "not implemented"

// Exercise 2.13 - HR 5.4 - downTo2 f n e
let rec downto1 f (n, e) = failwith "not implemented"
// factorial function using downto1 for recursion.
let fact n = failwith "not implemented"
let buildList g n = failwith "not implemented"