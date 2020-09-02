(* Helpers from Haskell *) 

let rec iterate f v =
    seq { yield v
          yield! iterate f (f v) }

let init xs =
    let l = (List.length xs) - 1
    Seq.take l xs |> Seq.toList

let constant a _ = a

(* Transform *)

let leftRotate xs = List.append (List.tail xs) [List.head xs]
let rightRotate xs = List.append [List.last xs] (init xs)

let rotations (xs: 'a list) : 'a list list =
    let l = List.length xs
    let leftRotate xs = List.append (List.tail xs) [List.head xs]
    Seq.take l (iterate leftRotate xs)
    |> Seq.toList

let transform (xs: 'a list) : int * 'a list =
    let xss = rotations xs |> List.sort 
    let position xs xss =
        Seq.length (Seq.takeWhile (fun l -> l <> xs) xss)
    position xs xss, (List.map List.last xss)

let input = ['y'; 'o'; 'k'; 'o'; 'h'; 'a'; 'm'; 'a']
let testTransform = transform input

(* Untransform = Recreate *) 

let takeCols (j: int) = List.map (List.take j)
let hdsort xss = List.sortBy List.head xss
let consCol xs xss = List.map2 (fun x lst -> x::lst) xs xss
let fork (f, g) x = (f x, g x)

let rec recreate k ys =
    match k with
    | 0 -> List.map (constant []) ys
    | _ -> let arr, arrs = fork (id, recreate (k-1)) ys
           consCol arr arrs |> hdsort

let untransform transformed =
    let k, ys = transformed
    let n = List.length ys
    List.nth (recreate n ys) k

(* Untransform without recreate *)

let untransform' transformed =
    let k, ys = transformed
    let n = List.length ys   
    let ya = ys |> List.toArray
    let pa = List.map snd (List.sort (List.zip ys [0..(n-1)])) |> List.toArray
    Seq.map (Array.get ya) (iterate (Array.get pa) k)
    |> Seq.tail
    |> Seq.take n
    |> Seq.toList

let testUntransform = untransform testTransform
let testUntransform' = untransform' testTransform
