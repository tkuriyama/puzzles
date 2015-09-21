#time 

// Pearl

type Op =
    | Add
    | Sub
    | Mul
    | Div

type Expr =  
    | Num of int
    | App of Op * Expr * Expr

type Value = int

let rec subseqs list : int list list =
    match list with 
    | []      -> []
    | x :: xs -> let xss = subseqs xs in 
                 xss @ ([x] :: List.map (fun elem -> x :: elem) xss)

let apply (action:Op) (v1:Value) (v2:Value) : Value = 
    match action, v1, v2 with 
    | Add, v1, v2 -> v1 + v2
    | Sub, v1, v2 -> v1 - v2
    | Mul, v1, v2 -> v1 * v2
    | Div, v1, v2 -> v1 / v2

let rec value (x:Expr) : Value =
    match x with 
    | Num(x) -> x
    | App(op, expr1, expr2) -> apply op (value expr1) (value expr2)

let legal (action:Op) (v1:Value) (v2:Value) : bool = 
    match action, v1, v2 with 
    | Add, v1, v2 -> (v1 <= v2)
    | Sub, v1, v2 -> (v2 < v1)
    | Mul, v1, v2 -> (1 < v1) && (v1 <= v2)
    | Div, v1, v2 -> (1 < v2) && (v1 % v2 = 0)

let concatMap f m = List.map (fun x -> f x) m |> List.concat

let rec unmerges ints : ('a list * 'a list) list = 
    match ints with    
    | []      -> []
    | [x; y]  -> [([x], [y])]
    | x::xs -> [([x], xs)] @ 
               (let add x (ys, zs) = [(x::ys, zs); (ys, x::zs)] in 
                concatMap (add x) (unmerges xs))

let divMod n d = (n / d, n % d)

let comb1 ((e1:Expr), (v1:Value)) ((e2:Expr), (v2:Value)) : (Expr * Value) list =
    [(App(Add, e1, e2), v1 + v2); (App(Sub, e2, e1), v2 - v1)] @
    if 1 < v1 then
        let q, r = divMod v2 v1
        [(App(Mul, e1, e2), v1 * v2)] @ 
        if r = 0 then [(App(Div, e2, e1), q)] 
        else []
    else []

let comb2 ((e1:Expr), (v1:Value)) ((e2:Expr), (v2:Value)) : (Expr * Value) list =
    [(App(Add, e1, e2), v1 + v2)] @
    if 1 < v1 then [(App(Mul, e1, e2), v1 * v2); (App(Div, e1, e2), 1)] 
    else []
      
let combine ((e1:Expr), (v1:Value)) ((e2:Expr), (v2:Value)) : (Expr * Value) list =
    if v1 < v2 then comb1 (e1, v1) (e2, v2) 
    else if v1 = v2 then comb2 (e1, v1) (e2, v2)
    else comb1 (e2, v2) (e1, v1) 

let rec mkExprs (ints:'a list) : (Expr * Value) list =
    match ints with 
    | [] -> []
    | [x] -> [(Num(x), x)]
    | xs -> [for ys, zs in unmerges xs do
             for ev1 in mkExprs ys do
             for ev2 in mkExprs zs do
             for ev in combine ev1 ev2 do
             yield ev]

let rec search n d ev evs : (Expr * Value) =
    match n, d, ev, evs with 
    | n, d, ev, []            -> ev
    | n, d, ev, ((e, v)::evs) -> let d' = abs (n - v)
                                 if d' = 0 then (e, v)
                                 else if d' < d then search n d' (e, v) evs
                                 else search n d ev evs
                                                       
let nearest n evs : (Expr * Value) =
    let e, v = List.head evs
    let d = abs (n - v)
    if d = 0 then (e, v)
    else search n d (e, v) (List.tail evs)

let countdown2 n ints = subseqs ints |> concatMap mkExprs |> nearest n

// Printing Solution

let opToString (op: Op) = 
    match op with 
    | Add -> "+"
    | Sub -> "="
    | Mul -> "*"
    | Div -> "/"

let rec eToString (e: Expr) = 
    match e with 
    | Num(x) -> string x
    | App(op, e1, e2) -> "(" + eToString e1 + (opToString op) +  eToString e2 + ")"

let display (e, v) = 
    let e = eToString e
    let v = string v 
    printfn "%s" (e + " = " + v)