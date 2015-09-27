
type Digit = int
type Factor = Digit list
type Term = Factor list
type Expression = Term list

let good v = v = 100

let product = List.fold (*) 1

let fold1 f items =
    List.fold f (List.head items) (List.tail items)

let valFact = fold1 (fun n d -> 10 * n + d) 

let valTerm = product << List.map valFact

let valExpr = List.sum << List.map valTerm

let concatMap f m = List.map (fun x -> f x) m |> List.concat

let glue (x: Digit) (es: Expression) : Expression list = 
    match x, es with 
    | x, ((xs::xss) :: xsss) -> [((x :: xs) :: xss) :: xsss;
                                ([x] :: xs :: xss) :: xsss;
                                [[x]] :: (xs :: xss) :: xsss]
    | _                      -> [[[[]]]]

let extend (x: Digit) (es: Expression list) : Expression list = 
    match x, es with 
    | x, [] -> [[[[x]]]]
    | x, es -> concatMap (glue x) es

let expressions es = List.foldBack extend es []

let solver1 = List.filter (good << valExpr) << expressions

// string display 

let fToString (f: Factor) : string = 
    string (valFact f)

let tToString (t: Term) : string = 
    String.concat " * " (List.map fToString t)

let eToString (e: Expression) : string =
    "100 = " + String.concat " + " (List.map tToString e)

let display (e: Expression list) = 
    let equations = String.concat "\n" (List.map eToString e)
    printfn "%s" equations
