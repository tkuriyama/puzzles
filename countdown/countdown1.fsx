#time 

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
    | Add, v1, v2 -> true
    | Sub, v1, v2 -> (v2 < v1)
    | Mul, v1, v2 -> true
    | Div, v1, v2 -> v1 % v2 = 0

let concatMap f m = List.map (fun x -> f x) m |> List.concat

let rec unmerges ints : ('a list * 'a list) list = 
    match ints with    
    | []      -> []
    | [x; y]  -> [([x], [y]); ([y], [x])]
    | x::xs -> [([x], xs); (xs, [x])] @ 
                 (let add x (ys, zs) = [(x::ys, zs); (ys, x::zs)] in 
                  concatMap (add x) (unmerges xs))

let combine ((e1:Expr), (v1:Value)) ((e2:Expr), (v2:Value)) : (Expr * Value) list =
    [for op in [Add; Sub; Mul; Div] do
        if legal op v1 v2 then yield (App(op, e1, e2), apply op v1 v2)]

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

let countdown1 n ints = subseqs ints |> concatMap mkExprs |> nearest n
