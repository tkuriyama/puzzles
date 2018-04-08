(* Burrows-Wheeler Transform *) 

let rec iterate f v =
    seq { yield v
          yield! iterate f (f v) }

let rotations (xs: char list) : char list list =
    let l = List.length xs
    let leftRotate xs = List.append (List.tail xs) [List.head xs]
    Seq.take l (iterate leftRotate xs)
    |> Seq.toList

let transform (xs: char list) : char list * int =
    let xss = rotations xs |> List.sort 
    let position xs xss =
        Seq.length (Seq.takeWhile (fun l -> l <> xs) xss)
    (List.map List.last xss), position xs xss

let input = ['y'; 'o'; 'k'; 'o'; 'h'; 'a'; 'm'; 'a']
let test = transform input
