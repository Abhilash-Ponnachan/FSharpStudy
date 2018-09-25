open System
open System.Xml

module PatternMatching =
    (* ==== match expression ==== *)
    (*
        'match..with..' expressions provide a powerful branching
        control structure based on patterns. It is like
        switch-case on steroids!

        match <expression> with
            | <pattern1> [when <condition>] -> <result_expr_1>
            | <pattern2> [when <condition>] -> <result_expr_2>
            | _ -> <default_result_expr>
    *)
    let x = "x"
    let v = 
        match x.ToUpper() with
        | "ALPHA" -> 'a'
        | "BRAVO" -> 'b'
        | "CHARLIE" -> 'c'
        | _ -> char(0)
    // val v : char = 'a'
    // --- pattern with function parameters using 'function' keyword ---
    let isVowel = function
    | 'a' | 'e' | 'i' | 'o' | 'u' -> true
    | _ -> false
    // val isVowel : _arg1:char -> bool
    // NOTE: automatic argument '_arg1', we do not specify a parameter, it is implied
    // NOTE: How we can specify multiple patterns in a single line
    printfn "%A" (isVowel 'i')      // true

    let rec fib n = 
        match n with
        | 0 -> 0
        | 1 -> 1
        | _ -> fib (n-1) + fib (n-2)
    printfn "Fib of %d is %d" 10 (fib 10) // Fib of 10 is 55

    // --- pattern guards/conditions ---
    (*
        since patterns can only contain literals, if we need to 
        have any extra logic in the patterm matching, we have to 
        use the 'when <condition>' (aka when guards)
    *)
    let checkRange value start ``end`` = 
        match value with
        | v when v < start -> -1
        | v when v >= start && v <= ``end`` -> 0
        | v when v > ``end`` -> 1
    printfn "%d" (checkRange 2 0 10)        // 0 
    printfn "%d" (checkRange 11 0 10)       // 1
    // NOTE: using the varibale 'v' for the conditions
    // NOTE: use of ``end`` because 'end' is a keyword and is used as an identifier

    // Using pattern matching with tuples
    let compare = function
    | a, b when a = b -> sprintf "%d equals %d" a b
    | a, b when a > b -> sprintf "%d greater than %d" a b
    | a, b -> sprintf "%d less than than %d" a b
    printfn "%s" (compare (23,45))      // 23 is less than 45

    // pattern matching with records
    type point = {x: float; y: float}
    let checkPoint = function
    | {x=0.0; y=0.0} -> "Point at origin"
    | {x=xVal; y=0.0} -> sprintf "Point on X-axis at %f" xVal
    | {x=0.0; y=yVal} -> sprintf "Point on Y-axis at %f" yVal
    | {x=xVal; y=yVal} -> sprintf "Point at %f at %f" xVal yVal
    printfn "%s" (checkPoint {x=23.0; y=0.0}) // Point on X-Axis at 23.0

    // ---- Pattern matching on Lists ----
    // Add after learning lists

    (* ==== Active Patterns ==== *)
    // Add after learning Discriminated unions!!