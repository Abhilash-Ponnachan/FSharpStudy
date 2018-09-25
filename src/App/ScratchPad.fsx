open System
open System.Drawing
open System.Windows.Forms
open System
open System.Web.UI.WebControls

let rec seive = function
    | h :: t -> h :: seive [for x in t do if x % h <> 0 then yield x]
    | [] -> []
let prm = seive [2..50]

let rec seive_1 = function
    | h :: t -> h :: seive_1 ( t |> List.filter (fun x -> x % h <> 0))
    | [] -> []
let prm_1 = seive_1 [2..1000]
let rec seive_2 lst_nums lst_prms =
    match lst_nums with
    | h :: t -> seive_2 ( t |> List.filter (fun x -> x % h <> 0)) (h :: lst_prms)
    | [] -> lst_prms
let prm_2 = seive_2 [2..1000] []

let arrAlphaPairs = Array.init 25 (fun i -> new string ([|char (i + int 'a');  char (i + 1 + int 'a')|]))

let arr2X2 = [|[|0; 1; 2|]; [|10; 11; 12|]|]
arr2X2.[0].[2]
let arr2D = array2D arr2X2
arr2D.[1, 2]


let matrix = Array3D.init 5 5 5 (fun i j k -> i*j*k)
matrix.[0..1, 0..1, *]
matrix.[4, 4, 4]

let seqEvens = Seq.init 10 (fun x -> printfn ".. seq item - %d.. " x; 2 * x)
let seqEnum = seqEvens.GetEnumerator()
seqEnum.MoveNext()
seqEnum.Current
seqEnum.MoveNext()
seqEnum.Current

let piSeries = Seq.initInfinite (fun i ->
                1.0 / (2.0 * float(i) + 1.0) * (if i % 2 = 0 then 1.0 else -1.0)
            )
let pi = (piSeries |> Seq.take 1000 |> Seq.sum ) * 4.0
let piSeries_1 = Seq.unfold (fun s -> Some(1.0/s, if s > 0.0 then -(s + 2.0) else -(s - 2.0))) 1.0
//for x in piSeries_1 |> Seq.take 10 do printfn "%A" x
let pi_1 = 4.0 * (piSeries_1 |> Seq.take 1000 |> Seq.sum )

let numPlates = seq {
                    for a in 'A'..'Z' do
                        for i in 0..9 do    
                            yield sprintf "%c%d" a i
                    }
printfn "%A" (numPlates |> Seq.take 30)

let seqTest = Seq.init 3 (fun x -> printf " item %d .." x; x)
seqTest |> Seq.iter (ignore) // empty iteration - 1st
// item 0 .. item 1 .. item 2 .. - gets printed
printfn "^ = 1st iteration"
seqTest |> Seq.iter (ignore) // empty iteration - 2nd
// item 0 .. item 1 .. item 2 .. - gets printed again
printfn "^ = 2nd iteration"

// cached sequence
let seqCached = Seq.cache seqTest
seqCached |> Seq.iter (ignore) // empty iteration - 1st
// item 0 .. item 1 .. item 2 .. - gets printed
printfn "^ = 3rd iteration"
seqCached |> Seq.iter (ignore) // empty iteration - 2nd
// nothing printed !! - the sequence is not evaluated again
printfn "^ = 4th iteration"

let point3D = (26.1, 23.6, 45.2)
let trd (_, _, x) = x
printfn "third element of %A is %A" point3D (trd point3D)

let distFrom0 pt = 
    match pt with   // pattern match on tuple
    | (x, y, z) -> sqrt (float(x * x + y * y + z *z))
let d = distFrom0 point3D
// val d : float = 57.28184704

// NOTE : pattern match directly in function parameter
let distFrom0X (x, y, z) = sqrt (float(x * x + y * y + z *z))


type switchState =
| On
| Off
| Between of float
// represents a switch state which can be On, Off or at a point in between
let toggleLight = function
| On -> Off     // Off if On                         
| Off -> On     // On if Off
| Between (brightness)->
        if brightness >= 0.5 then       // if midway or more
            match brightness - 0.5 with // reduce by half
            | d when d <= 0.0 -> Off    // if new <= 0 the off
            | d -> Between (d)          // else dial down half
        else                            // if below midway
            match brightness + 0.5 with 
            | d when d >= 1.0 -> On     // if new >= 1 then On
            | d -> Between (d)          // dial up

let lw1, lw2 = toggleLight On, toggleLight Off
// val lw1 : switchState = Off
// val lw2 : switchSatate = On
let lw3, lw4 = toggleLight (Between(0.7)), toggleLight (Between(0.3))
// val lw3 : switchState = Between(0.2)
// val lw4 : switchSatate = Between(0.8)
let lw5 = toggleLight (Between(0.5))
// val lw5 : switchState = Off

type Tree =
| Leaf of int
| Node of left: Tree * right: Tree

let tr1 = Node(
    left = Node(
        left = Leaf 1,
        right = Node(
            left = Leaf 2,
            right = Leaf 4
        )
    ),
    right = Node(
        left = Leaf 3,
        right = Leaf 5
    )
)

let sumTree tree =
    let rec walk acc = function
        | Leaf (x) -> acc + x
        | Node (l, r) -> acc + (walk 0 l) + (walk 0 r)
    walk 0 tree

let s = sumTree tr1
// val s : int = 15

type Expression = // An expression is :
| Num of int     
| Var of string
| Add of Expression * Expression
| Sub of Expression * Expression
| Mul of Expression * Expression
| Div of Expression * Expression
// NOTE: The recursive definitions!!
// It is idiomatic of heirarchical data types in FP

// function to evaluate an experssion
let rec evalExp (env: Map<string, int>) = function
    | Num i -> i
    | Var v -> env.[v]
    | Add (exp1, exp2) -> evalExp env exp1 + evalExp env exp2
    | Sub (exp1, exp2) -> evalExp env exp1 - evalExp env exp2
    | Mul (exp1, exp2) -> evalExp env exp1 * evalExp env exp2
    | Div (exp1, exp2) -> evalExp env exp1 / evalExp env exp2

// declare an expression
let exp1 = Add (Mul (Num(5), Var("x")), Num (2))
// exp1 = 5*x + 2
let r1 = evalExp (Map.ofList ["x", 20]) exp1
// val r1 : int = 102

type  Process = | Service of id : int

let goexec = Service 20034
// we can unwrap this value directly 
let (Service id) = goexec
// val id : int = 20034