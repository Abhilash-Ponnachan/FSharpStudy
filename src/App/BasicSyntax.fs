open System
open System.Diagnostics

module BasicSyntax =
  // This is a single line comment
    (* This is a..
       .. multi-line comment*)
    /// XML comments - generates XML documentation
    (* ==== 'let' binding and common-data types ===== *)
    // ---- variables (not really) - 'let' bindings ----
    // 'let' creates immutable value bindings
    let myInt = 23 // binds 'myInt' to an integer value = 23
    // val myInt : int = 23
    let myFloat = 23.1  // float or double (64 bit)
    // val myFloat : float = 23.1
    // ---- Type suffix/prefix ----
    let myByte = 88y
    // val myByte : sbyte = 88y
    let myUByte = 88uy  // unsigned byte
    // val myByte : byte = 88uy
    let myLong = 4294967296I  // bigint
    // val myLong : System.Numerics.BigInteger = 4294967296
    let myLong_2 = 4_294_967_296I  // underscore separators
    // val myLong_2 : System.Numerics.BigInteger = 4294967296
    let myChar = 'a'
    // val myChar : char = 'a'
    let myChrByte = 'a'B
    // val myChrByte : byte = 97uy

    // --- string ----
    let myStr_1 = "I am a string!"
    // val myStr_1 : string = "I am a string!"
    let myStr_2 = @"I can have special \chars including "" "
    // ignores special chars - for " use ""
    let myStr_3 = """ Me too, but need only one " """
    // ignores special chars including " - similar to Python

    // ---- multiple assignment ----
    let a, b, c = 23, 78uy, true
    // val a : int = 23
    // val b : byte = 78uy
    // val c : bool = true
    // This is useful for "deconstruction" assignments

    (* === mutable bindings ==== *)
    // we can explicitly make a binding mutable using the 'mutable' keyword
    // it then behaves like a 'variable'
    let mutable counter = 0
    counter <- counter + 1      // note the "<-" assignment operator
    // val mutable counter : int = 1

    (* ==== print functions ==== *)
    // ---- printf & printfn - print to console ----
    // print to console using 'printfn' function (like System.WriteLine() in C#)
    // 'printfn' prints with a trailing new-line character '\n', 'printf' does NOT
    // Uses 'C' style format specifiers as first argument
    // This has the form - %[flags][width][.precision][type]
    // flags = '0', '-', '+', '' - These act as fillers for minimum width
    // width = minimum width
    // precision = number of digits after floating point
    // type = %d (decimal), %i(integer), %f(float), %e(exp), %f%b(bool), %s(string), %A(any), %O(ToString),  
    printfn "%c" 'a'
    // c
    printfn "%05.2f" 1.1
    //01.10
    printfn "%5.2f" 1.1
    // 1.10 (space prefix for filling width)
    printfn "%-5.2f" 1.1
    //1.10 (no space prefix)
    printfn "%A" System.DateTime.Now
    // 6/8/2018 1:01:33 PM

    // ---- sprintf - assign to a string ---
    let age = sprintf "I am %i years old" (System.DateTime.Now.Year - 2000)
    printfn "%s" age
    // I am 18 years old#

    (* ==== Conditionals ==== *) 
    // --- if <boolean condition> then <true expression> [elif ..] [else <else expression>]
    // Unlike other languages in F# "if then else" is an expression
    // It will always return a value
    // F# has 'elif' like Python
    let x, y, z = 1, 2, 3
    let check = 
        if x = z then "x equals z"
        elif y = z then "y equals z" 
        else "z is unique"
        // note - boolean equality uses '='
    printfn "%s" check

    (* ==== Loops ==== *)
    (* 
        ---- for .. do ----
        for <identifier> = <start> to|downto <finish> do
            loop-body-expression

        The range from <start> to <finish> has to be int32

        This is akin to the standard loop construct in
        imperative programming
        In fact this is almost same as Pascal syntax
    *)
    for i = 0 to 5 do
        // do something
        printfn "%A" i
    // NOTE: this is not very useful with functional paradigm

    (*
        ---- for .. in ----
        The for..in is used to loop over an iterable such as -
            - range expression
            - list, array, sequence
            - any iterable that implements IEnumerable
        It is like foreach in C#, but also supports pattern matching

        for <idertifier_pattern> in <iterable_expression> do
            <body_expression>

    *)
    let mutable vows = 0
    for chr in 'a' .. 'z' do
        match chr with
        | 'a' | 'e' | 'i' | 'o' | 'u' -> vows <- 1 + vows
        | _ -> vows <- vows
    printfn "number of vowels = %A" vows
    // number of vowels is 5

    // pattern match the identifier
    let sqrs = seq {for i in 2 .. 5 -> (i, i * i)}
    for (x, s) in sqrs do
        printfn "%d - (sqr) -> %d " x s
    
    (*
        ---- while .. do ----
        while <test_expression> do
            <loop_body>
    *)
    let mutable n = 5
    while n > 0 do
        // do something
        printfn "%d" n
        n <- n - 1
    
    //NOTE : As evident, loop constructs are imperative by nature