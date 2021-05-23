// Q1

type multimap<'a,'b when 'a: comparison> = MMap of Map<'a,list<'b>>

let ex = MMap (Map.ofList [("record",[50]);("ordering",[36;46;70])])


// Q1.1
let studReg = MMap (Map.ofList [("Grete", []); ("Hans", ["TOPS"; "HOPS"]); ("Peter", ["IFFY"]); ("Sine", ["HOPS"; "IFFY"; "BFNP"])])

let studReg2 = MMap (Map.ofList [("Grete", []); ("Hans", ["TOPS"; "HOPS"]); ("Peter", ["IFFY"]); ("Sine", ["IFFY"; "HOPS"; "BFNP"])]) 

let compare = studReg = studReg2
// As the lists are considered as sets, the order in which strings occur is irrelevant.
// However, changing the order means the data is not the same, resulting in a false comparison.

// Q1.2
let canonical (MMap map) = MMap (Map.map (fun _ v -> List.sort v) map)

let toOrderedList map = match canonical map with MMap m -> Map.toList m

let studRegOrdered = toOrderedList studReg

// Q1.3
let newMultiMap () = MMap Map.empty

let sizeMultimap (MMap map) =
    let subSetsSize = Map.fold (fun size k v -> size + (List.length v)) 0 map
    (map.Count, subSetsSize)

let sizeStudReg = sizeMultimap studReg

// Q1.4
let addMultimap k v (MMap m) =
    match Map.tryFind k m with
    | Some list -> MMap (Map.add k (v::(List.filter (fun elm -> elm <> v) list)) m)
    | None -> MMap (Map.add k [v] m)

sizeMultimap (addMultimap "Sine" "BFNP" studReg)
sizeMultimap (addMultimap "Grete" "TIPS" studReg)
sizeMultimap (addMultimap "Pia" "" studReg)

let removeMultimap k vOpt (MMap m) =
    match vOpt with
    | None -> MMap (Map.remove k m)
    | Some v -> 
        match Map.tryFind k m with
        | None -> MMap (m)
        | Some list -> MMap (Map.add k (List.filter (fun elm -> elm <> v) list) m) 

sizeMultimap (removeMultimap "Sine" None studReg)
sizeMultimap (removeMultimap "Sine" (Some "PLUR") studReg)
sizeMultimap (removeMultimap "Kenneth" (Some "BLOB") studReg)
sizeMultimap (removeMultimap "Peter" (Some "IFFY") studReg)