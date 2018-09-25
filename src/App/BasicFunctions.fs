open System
open System.Web.Security

module BasicFunctions =
    (* ==== Function Definition ==== *)
    // let <function_name> <param_list> [:<return_type>] = <body_expression>
    // return type is optional, F# has strong type inference 
    // functions have to return a value (even if it is 'unit')
    // function will return the value of the last expression
    let dbl (x: int64) = x * 2L

    let circArea r = 
        let pi = 3.141
        pi * r * r
    // return will be result of last expression
    (* ==== Function Invocation ==== *)
    // <function_name> <arg1> <arg2> .. <argn>
    // invoking a function does not use parentheses 
    printfn "%A" (circArea 4.0)     // 50.256
    // use parentheses to specify precedence (when nesting function calls)

    (* ==== Partial Application & Cyrrying ==== *)
    (* 
        Functional langauges like F# curries function invocation.
        This means that invoking a function with multiple parameters
        is decomposed into a chain of invocations with each having one 
        parameter and returning a function that takes the remaining 
        parameters ...
        F a b c => F a -> F` b -> F`` c
        Therefore when we invoke a function with a subset 
        of teh paramters list, the result is a new function 
        which takes the remaining parameters - this is
        called partial-application!
    *)
    let prod x y = x * y
    printfn "%d" (prod 3 2)  // 6

    // partial application of 1 parameter
    let twice = prod 2
    printfn "%d" (twice 3)    // 6

    let thrice = prod 3
    printfn "%d" (thrice 3)    // 9
    // Note: How "Partial Application" be used as a "Function Factory"!!

    (* ==== Recursive Function ==== *)
    // let rec <function_name> <param_list> 
    let rec fact n = if n = 1 then 1 else n * fact (n - 1)
    printfn "%d" (fact 5)       // 120
    // ---- Tail Recursion ---
    (*
        In the above example of 'fact' the depth of the call-stack keeps
        growing with each recursive call -
        fact 5
            = 5 * fact 4
                    = 4 * fact 3
                            = 3 * fact 2
                                    = 2 * fact 1
                                            = 1 * 1
                                    = 2 * 1
                            = 3 * 2
                    = 4 * 6
            = 5 * 24
        = 120
        This nesting is ok for small values, but for large values it might 
        result in stack overflow! This is especially so with collections.

        In this approach, at each stage we have to wait for the recursive call to return
        before we can perform the computation. Hence the entire call chain has to
        unwind and wind back again! 
        If we change this model slightly by carrying the computation along with each 
        recursive call by providing an additional parameter (often named 'acc' for accumulator),
        the compiler can optimize this as a iterative call reusing the stack. This is 
        called Tail-Recursion and F# supports this.
        In order for F# to optimize for this though we have change the way we implement
        the function -
    *)
    let rec tailFact n =
        let rec loop n acc =
            if n = 1 then acc
            else loop (n - 1) (n * acc)
        loop n 1
    printfn "factorial of %d is %d" 5 (tailFact 5)
    // factorial of 5 is 120
    
    (* ==== First Class Functions ==== *)
    (* 
        Like in all functional programming languages functions in F#
        are first class members. This means that a function is just 
        like any other value. It can be passed around as arguments
        and returned from functions. The function value has a type
        denoted using '->' notation.
        for example the function -
        let foo (x: int) : int = ...
        Takes and 'int' and returns an 'int', this will have the type -
        foo : int -> int

        A function with multiple parameters -
        let bar (x: int) (y: float) : float = ...
        Takes an 'int' and a 'float' and returns a 'float', will have type -
        bar : int -> (float -> float)
        Reduced due to currying as -
        bar : int -> float -> float
    *)
    let foo (x : int) : int = x
    // val foo:int -> int
    let bar (x:int) (y:float) = y
    //val bar:int -> float -> float
    // We can define a higher orfer function (HOF) that takes
    // another function as an argument
    let myApply (f: int -> int) x = f x
    //val myApply: (int -> int) -> int ->int
    printfn "%d" (myApply twice 10)     // 20
    printfn "%d" (myApply thrice 10)    // 30

    (* ==== Lambda Expressions ==== *)
    (* 
        We can declare functions on the fly to use one time using
        lambda functions. We do not need to define a named fucntion 
        and then use it. This is a common construct & practice in all 
        programming lanuages that support functional programming.
        this is done using the 'fun' keyword and '->'.
    *)
    // We can use that in the above example
    printfn "%d" (myApply (fun x -> x * 2) 100)  // 200
    printfn "%d" (myApply (fun x -> x * 3) 100)  // 300

    // Use lambdas to create a function factory
    let stepper_factory step = 
        fun x -> x + step
    let step_by_1 = stepper_factory 1
    printfn "%d" (step_by_1 10)     // 11
    let step_by_100 = stepper_factory 100
    printfn "%d" (step_by_100 10)   // 110
    (* ==== Function Composition ==== *)
    (*
        We can compose multiple function into a single one using
        the '>>' composition operator
    *)
    let sixTimes = twice >> thrice
    // val sixTimes: (int -> int)
    printfn "%d" (sixTimes 100)

    // let us examine the sequence of application of the composed function
    let append_a (s: string) = s + "(+ A)"
    let append_b (s: string) = s + "(+ B)"

    let append_a_b = append_a >> append_b
    printfn "%s" (append_a_b "XXX")     // XXX(+ A)(+ B) => append_b(append_a x)
    let append_b_a = append_b >> append_a
    printfn "%s" (append_b_a "XXX")     // XXX(+ B)(+ A) => append_a(append_b x)

    // We can make our own compose function
    let compose (f: string -> string) (g: string -> string) = fun x -> g (f x)
    // This can be made resuable with generics, which we shall see later
    let myAppend_a_b = compose append_a append_b
    printfn "%s" (myAppend_a_b "XXX")   // XXX(+ A)(+ B) => append_b(append_a)
    
    // the '>>' operator can be used in the reverse direction as '<<'
    // the order of the composite reverse
    let revAppend_a_b = append_a << append_b
    printfn "%s" (revAppend_a_b "XXX")  // XXX(+ B)(+ A) => append_b then do append_a

    (* 
        Like Currying, Composition is another function factory mechanism.
        While Currying breaks down bigger functions into specific smaller ones
        Composition builds up smaller ones into more complex functions!
    *)

    (* ==== Function Pipeline ==== *)
    (*
        The "|>" operator can be used to chain "function invocation" in a
        linear flow.
    *)
    let r1 = "XXX" |> append_a |> append_b
    printfn "%s" r1     // XXX(+ A)(+ B) => To "XXX" append_a then append_b
    // The "|>" operator results in a value rather than a composed function, unlike ">>"

    // let us consider a function that takes two parameters
    let a_then_b s1 s2 : string = s1 + " -(+)-> " + s2
    let r2 = "First" |> a_then_b "Second"
    printfn "%s" r2     // Second -(+)-> First
    (*
        NOTE: the "|>" operator simply takes what is on the LHS 
        and passes it in as the LAST argument to the function on the right
    *) 