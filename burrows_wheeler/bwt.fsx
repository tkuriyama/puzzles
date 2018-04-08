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

let rec recreate j xs =
    match j with
    | 0 -> List.map (constant []) xs
    | _ -> let arr, arrs = fork (id, recreate (j-1)) xs
           consCol arr arrs |> hdsort

let untransform transformed =
    let pos, xs = transformed
    let l = List.length xs
    List.nth (recreate l xs) pos

let testUntransform= untransform testTransform
