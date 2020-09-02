#time

type Digit = int
type Factor = Digit list
type Term = Factor list
type Expression = Term list
type Values = (Digit * Digit * Digit * Digit)

let good c (k, f, t, e) = (f * t + e = c)
let ok c (k, f, t, e) = (f * t + e <= c)

let glue (x: Digit) (ev: Expression * Values) : (Expression * Values) list =
    match x, ev with 
    | x, ((xs :: xss) :: xsss, (k, f, t, e)) ->  
        [(((x :: xs) :: xss) :: xsss, (10 * k, k * x + f, t, e));
         (([x] :: xs :: xss) :: xsss, (10, x, f * t, e));
         ([[x]] :: (xs :: xss) :: xsss, (10, x, 1, f * t + e))]

let expand (c: Digit) (x: Digit) (evs: (Expression * Values) list) = 
    match c, x, evs with
    | c, x, []    -> [([[[x]]], (10, x, 1, 0))]
    | c, x, evs   -> List.map (List.filter (ok c << snd) << glue x) evs
                     |> List.concat 

let solver2 (c: Digit) (ds: Digit list) : Expression list = 
    List.foldBack (expand c) ds [] 
    |> List.filter (good c << snd) 
    |> List.map fst

// string display 

let fold1 f items =
    List.fold f (List.head items) (List.tail items)

let valFact = fold1 (fun n d -> 10 * n + d) 

let fToString (f: Factor) : string = 
    string (valFact f)

let tToString (t: Term) : string = 
    String.concat " x " (List.map fToString t)

let eToString (e: Expression) : string =
    "100 = " + String.concat " + " (List.map tToString e)

let display (e: Expression list) = 
    let equations = String.concat "\n" (List.map eToString e)
    printfn "%s" equations

