// Question 1

// Question 1.1

// Declare an F# function infSeq3
let infSeq3 = 
    Seq.initInfinite (fun index -> index*3)

// Declare an F# function finSeq3
let finSeq3 n =
    Seq.take n infSeq3

// Declare a function sumSeq3 n of type ...
let sumSeq3 n =
    Seq.fold 