// Question 1.1

type mymap<'a,'b> = MyMap of list<'a*'b>

// Declare map values dice1 and dice2, representing the two maps
let dice1 = MyMap [(1,4);(2,2);(3,3);(4,2);(5,2);(6,2)]
let dice2 = MyMap [(1,4);(2,2);(3,3);(4,3);(5,5);(6,3)]

// Explain the type of the two values
// Both values are of type mymap which holds a key of type int mapped to a value of type int

// Declare an F# function emptyMap() of type ...
let emptyMap () = MyMap []

// Declare F# function size m of type ...
let size m =
    match m with 
    | MyMap m -> List.length m

// Question 1.2

// Declare an F# function isEmpty m
let isEmpty m =
    match m with
    | MyMap m when List.length m = 0 -> true
    | _ -> false

// Declare an F# function tryFind of type ...
let tryFind k m =
    match m with
    | MyMap m -> List.tryFind (fun (x,_) -> x = k) m

// Declare an F# function remove k m of type ...
let remove k m =
    match m with
    | MyMap m ->
        let result = List.tryFind (fun (x,_) -> x = k) m
        match result with
        | Some t -> MyMap (List.except [t] m) 
        | None -> MyMap (m)

// Declare an F# function add k v m of type ...
let add k v m =
    match m with
    | MyMap m ->
        let result = List.tryFind (fun (x,_) -> x = k) m
        match result with
        | Some t -> MyMap ((k,v)::(List.except [t] m))
        | None -> MyMap ((k,v)::m)

// Question 1.3

// Declare an F# function upd f k v m
let upd f k v m =
    match m with
    | MyMap m ->
        let result = List.tryFind (fun (x,y) -> x = k || y = v) m
        match result with
        | Some (x,y) -> MyMap ((x,f y v)::m)
        | None -> MyMap ((k,v)::m)

// Declare an F# function map f m of type ...
let map f m =
    match m with
    | MyMap m -> MyMap (List.map (fun (k,v) -> (k, f v)) m)

//let test = map (fun (x,v) -> (x,v+2)) dice1
        
let fold f s m =
    match m with
    | MyMap m -> List.fold (fun k -> f k) s m