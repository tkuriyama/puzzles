#time

type Row<'a> = 'a list
type Matrix<'a> = 'a Row list

type Digit = int
type Grid = Digit Matrix
type Choices = Digit list

let digits = [1..9]
let blank a = a = 0

// generate choices
let choice d = if blank d then digits else [d]
let choices (g: Grid) : Choices Matrix =
    List.map (List.map choice) g

// expand choices as cartesian product
let rec cp (lss: 'a list list) : 'a list list =
    match lss with 
    | []          -> [[]]
    | (xs :: xss) -> [for ys in cp xss do 
                        for x in xs do 
                            yield x :: ys]

let expand (m: Choices Matrix) : Grid list =
    List.map cp m |> cp

// better expand
let breaks p xs = (Seq.takeWhile (not << p) xs |> Seq.toList, 
                   Seq.skipWhile (not << p) xs |> Seq.toList) 

let counts = List.concat >> List.map List.length >> List.filter (fun x -> x <> 1)

let expand1 (rows: Choices Matrix) : Choices Matrix list = 
    let n                       = List.min (counts rows) in
    let smallest cs             = List.length cs = n in 
    let (rows1, row :: rows2)   = breaks (List.exists smallest) rows in
    let (row1, cs :: row2)      = breaks smallest row in 
    [for c in cs do yield rows1 @ [row1 @ [c] :: row2] @ rows2]

// valid predicate
let rec nodups lst : bool = 
    match lst with 
    | []        -> true 
    | (x :: xs) -> (List.forall (fun y -> x <> y) xs) && nodups xs  

let rows (m: 'a Matrix) : 'a Matrix = m

let rec cols (m: 'a Matrix) : 'a Matrix = 
    match m with 
    | []            -> [[]]
    | [xs]          -> [for x in xs do yield [x]]
    | (xs :: xss)   -> List.map2 (fun a b -> a :: b) xs (cols xss)

let ungroup (lss : 'a list list) : 'a list =
    List.concat lss

let rec group (lst: 'a list) : 'a list list = 
    match lst with 
    | []    -> []
    | xs    -> (List.truncate 3 xs |> List.ofSeq) :: 
               group (Seq.skip 3 xs |> List.ofSeq)

let boxs (m: 'a Matrix) : 'a Matrix =
    List.map group m |> group |> 
    List.map cols |> ungroup |> 
    List.map ungroup

let valid (g: Grid) : bool =
    List.forall nodups (rows g) && List.forall nodups (cols g) &&
    List.forall nodups (boxs g)

// list difference infix operator (from Stack Overflow)
let flip f x y = f y x

let rec delete x = function
  | [] -> []
  | h :: t when x = h -> t
  | h :: t -> h :: delete x t

let inline ( /-/ ) xs ys = List.fold (flip delete) xs ys

// pruning
let single xs = List.length xs = 1

let yieldSingle row = [for xs in row do 
                        if single xs then yield List.head xs]

let remove (xs: Choices) (ds: Choices) : Choices = 
    if single ds then ds else (ds /-/ xs) 

let pruneRow (row: Choices Row) : Choices Row =
   let fixed_ds = yieldSingle row in
   List.map (remove fixed_ds) row

let pruneBy f = f >> List.map pruneRow >> f

let prune = pruneBy rows >> pruneBy cols >> pruneBy boxs

// checks
let complete (m: Choices Matrix) : bool = 
    List.forall (List.forall single) m

let ok row = nodups (yieldSingle row)

let safe (m: Choices Matrix) : bool =
    List.forall ok (rows m) && List.forall ok (cols m) && 
    List.forall ok (boxs m)

// solver
let solve (g: Grid) : Grid list= 
    choices g 
    |> prune 
    |> expand 
    |> List.filter (rows >> List.forall nodups) 
    |> List.filter (cols >> List.forall nodups)
    |> List.filter (boxs >> List.forall nodups)

// better solver

let rec search (m: Choices Matrix) : Grid list=
    let m' = prune m in  
    if (safe m = false) then [] else
    if complete m' then [List.map (List.map List.head) m] else
    List.concat (List.map search (expand1 m'))

let solve1 (g: Grid) : Grid list = 
    choices g |> search