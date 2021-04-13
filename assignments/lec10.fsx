// Computing slowfib(42) is CPU-intensive, ca 2 sec. Mono Mac 

let rec slowfib n = if n<2 then 1.0 else slowfib(n-1) + slowfib(n-2);;

// Don Syme examples, adapted
// Times are Mono 2.10.8 on MacOS 10.6.8 Core 2 Duo 2.66 GHz 

let fib42 = slowfib(42);;
// Real: 00:00:02.668, CPU: 00:00:02.668, GC gen0: 0

let fibs = [ slowfib(41); slowfib(42) ];;
// Real: 00:00:04.313, CPU: 00:00:04.312, GC gen0: 0

let fibs =
  let tasks = [ async { return slowfib(41) };
                async { return slowfib(42) } ]
  Async.RunSynchronously (Async.Parallel tasks);;
// Real: 00:00:02.690, CPU: 00:00:04.274, GC gen0: 0

let fibs = [ for i in 0..42 do yield slowfib(i) ];;
// Real: 00:00:06.983, CPU: 00:00:06.979, GC gen0: 0

let fibs =
    let tasks = [ for i in 0..42 do yield async { return slowfib(i) } ]
    Async.RunSynchronously (Async.Parallel tasks);;
// Real: 00:00:04.072, CPU: 00:00:06.938, GC gen0: 0

// Dissection of this:

// async { return slowfib(i) }
// has type: Async<float>
// an asynchronous tasks that, when run, will produce a float

// let tasks = [ for i in 0..42 do yield async { return slowfib(i) } ]
// has type: Async<float> list
// a list of asynchronous tasks, each of which, when run, will produce a float

// Async.Parallel tasks
// has type: Async<float []>
// an asynchronous tasks that, when run, will produce a list of floats

// Async.RunSynchronously (Async.Parallel tasks)
// has type: float []
// a list of floating-point numbers

// F# parallel computing examples inspired by Hansen and Rischel
// chapter 13.6 * sestoft@itu.dk * 2013, 2017-03-20

let isPrime n =
    let rec testDiv a = a*a > n || n % a <> 0 && testDiv (a+1)
    n>=2 && testDiv 2;;

let factors n =
    let rec factorsIn d m =
        if m <= 1 then []
        else if m % d = 0 then d :: factorsIn d (m/d) else factorsIn (d+1) m
    factorsIn 2 n;;

let random n =
    let generator = new System.Random ()
    fun () -> generator.Next n;;

let r10000 = random 10000;; // 150x faster than creating a new System.Random

let rec ntimes (f : unit -> 'a) n =
    if n=0 then () else (ignore (f ()); ntimes f (n-1));;
    
let bigArray = Array.init 5000000 (fun _ -> r10000 ());;

#time;;

Array.map isPrime bigArray;;
// Real: 00:00:00.320, CPU: 00:00:00.319
Array.Parallel.map isPrime bigArray;;
// Real: 00:00:00.080, CPU: 00:00:00.617

// Better example: Prime factors of random numbers (more work)

Array.map factors bigArray;;
// Real: 00:00:22.076, CPU: 00:00:22.041
   
Array.Parallel.map factors bigArray;;
// Real: 00:00:06.538, CPU: 00:00:33.396
   
// Even better example: Prime factors of [1..200000]
let factors n =
    let rec factorsIn d m =
        if m <= 1 then []
        else if m % d = 0 then d :: factorsIn d (m/d) else factorsIn (d+1) m
    factorsIn 2 n;;
Array.init 200000 factors;;
let factors200000 = Array.Parallel.init 200000 factors;;

//Seq.iter (fun elem -> List.iter (fun fs -> printf "%d " fs) elem) (Seq.ofArray factors20)

// > Array.init 200000 factors;;
// Real: 00:00:11.650, CPU: 00:00:11.631

// > Array.Parallel.init 200000 factors;;
// Real: 00:00:02.490, CPU: 00:00:17.859

//let histogram = Array.init 200000 (fun i -> 0)
//let incr i = histogram.[i] <- histogram.[i] + 1
//Array.iter (fun fs -> List.iter incr fs) factors200000;;

//let test = Seq.init 5 (fun n -> 1)
//Seq.iter (fun elem -> printf "%d " elem) test
//let count = Seq.countBy (fun elem -> elem) test
//printfn "%A" (Seq.toList count)

let factorsList = (Seq.fold (fun acc x -> Seq.append acc (Seq.ofList x)) Seq.empty factors200000)
let countOfPrimes = Seq.countBy id factorsList

printfn "%A" (Seq.toList countOfPrimes ) 
//Seq.iter (fun elem -> List.iter (fun fs -> printf "%d " fs) elem) out

