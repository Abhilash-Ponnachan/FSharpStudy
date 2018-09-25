open System

module DataTypes = 
    (* ==== Records ==== *)
    (*
        A record represents a simple aggregate of named values.
        It is similar to Struct in C. 
        It has the general form -
        
        [<attribute>]
        type [accessibility-modifier] <rec_name> =
        {
            <label 1> : <type 1>
            <lable 2> : <type 2>
            [mutable] <label 3> : <type 3>
        }

        NOTE : 
            - use the '{}' to demarcate a record type
            - we can use ';' to separate record items in the same line
            - items can be made mutable if needed
            - by default records are 'reference' types, use the [<Struct>] to make it struct type
    *)

    // ---- defining record types ----
    type Point = {x: float; y: float; z: float}
    // NOTE: record labels can be in the same line if separated by ';'
    type Customer = {
        firstName: string
        lastName: string
        SSN: int
    }
    // Labels separated by new lines
    [<Struct>]
    type Student = {
        name: string
        id: int
        mutable ``class``: string
    }
    // A struct record of student, with class field mutable
    // NOTE: use of ``class`` since 'class' is a keyword!

    // ---- declaring record type values ----
    let origin = {x = 0.0; y = 0.0; z = 0.0}
    // val origin : Point = {x = 0.0; y = 0.0; z = 0.0}
    // the type is inferred using the label names

    type Point2D = {x: float; y: float}
    // NOTE: Point2D is very similar to Point

    let d2X1 = {x = 1.0; y = 0.0}
    // val d2X1 : Point2D = {x = 1.0; y = 0.0}

    (*
        If there is another record with the same labels, then type inference
        might not be as expected -
        - using only the label names will result in the the latest record type
        to be applied
        - use fully qualified label names (with record prefix) to be specific
    *)
    type Point3D = {
        x: float
        y: float
        z: float
    }

    let x1y2z1 = {x = 1.0; y = 2.0; z = 1.0}
    // val x1y2z1 : Point3D

    let px1y2z1 = {Point.x = 1.0; y = 2.0; z = 1.0}
    // NOTE:  usage of Point.x to specify the value as a Point type

    // ==== Copy and Update pattern
    let px1y2z2 = {px1y2z1 with z = 2.0}
    // create a new value desired values changed

    // this can be used for 'default record pattern'
    let defaultPoint = {Point.x = 0.0; y = 0.0; z = 0.0}
    let px0y0z1 = {defaultPoint with z = 1.0}

    // ==== Pattern matching on Records ====
    // pattern matching to unpack
    let {Point.x = xval; y = _ ; z = _} = px1y2z1
    // xval will have value 1.0
    // NOTE: record unpacking requires {} and labels on the left, unlike tuple unpacking

    // pattern matching with fucntions
    let magnitude = function
    | {x = xval; y = yval; z = zval} -> sqrt(xval * xval + yval * yval + zval * zval)

    printfn "%A" (magnitude x1y2z1)
    // 2.449
    printfn "%A" (magnitude {x = 2.5; y = 0.0; z = 1.3})
    // 2.817

    (*
        Classes versus Records : 
            - Record fields/labels are automatically exposed as properties
            - Records do not have behavior as methods
            - Records do not have constructors
            - Records have structural equality semantics, whereas classes have
                reference equality semantics
    *)

    // structural equality
    let p1 = {x = 2.1; y = 3.4}
    let p2 = {x = 2.1; y = 3.4}

    let sameOrNot = p1 = p2
    // val : bool = true
    // NOTE :  p1 and p2 are same becuse the values are same!


    (* ==== Enums ==== *)
    (*
        Enums are ordinal types with lables associated for integral values.
        They are used to make code for readable.
        They take the form :
        
        type <enum-name> =
        | <label 1> = <integer literal 1>
        | <label 2> = <integer literal 2>
        ...
    *)
    // ---- defining an enum ---
    type Color = 
    | Red = 0
    | Green = 2
    | Blue = 4

    // the underlying type of an enum is the type of its literal values
    // underlying integral types for enums can be :
    //      sbyte, byte, int16, uint16, int32, unint32, int64, uint64, char
    //  int32 is the default
    // however we can change it to any integral type

    type uColor = 
    | Red = 0u
    | Green = 1u
    | Blue = 2u

    // ---- using enums
    let col1 = Color.Green
    printfn "my favourite colour is %A" col1
    // my favourite color is Green

    // convert enum -> underlying type value
    let col_val = int Color.Red
    // val col_val : int = 0

    // convert literal -> enum - use 'enum<T'>' function
    let col_enum = enum<Color> 2
    // val col_enum : Color = Green

    //NOTE: 'enum' function only works with int32 underlying type
    // for other types we have to use another library function

    let ucol_enum = Microsoft.FSharp.Core.LanguagePrimitives.EnumOfValue<uint32, uColor> 2u
    // val ucol_enum : uColor = Blue

    // ---- iterating over enum domain ----
    for col in Enum.GetValues typeof<Color> do
        printfn "%A" col
    // Enum.GetValues function - returns enum items as an array!

    // pattern matching with enums
    let isHot = function
    | Color.Red -> "It seems hot!"
    | _ -> "It is not"

    printfn "%A" (isHot Color.Blue)
    // It is not

    (* ==== Discrimninated Unions ==== *)
    (*
        Union types in programming languages such as C is a way for
        the same variable to hold values of different types. In VB and Delphi
        this is achieved using 'Variant' types.
        It is a type definition that can have a choice of other types within them.
        I like to call these 'delegated types'
        The varibale of a Union type can have a value which is any one of those 
        delegated types.
        In F# (and TypeScript) we have Discriminated Unions, these are Union types
        with an identifier for each case of the delegated type.

        It has the syntax - 
        type [access-modifier] <type-name> = 
        | case-identifier-1 [of [field-name-1 : ] type-1 [* [field-name-2 : ] type-2]]
        | case-identifier-2 [of [field-name-1 : ] type-1 [* [field-name-2 : ] type-2]]

        NOTE: 
            1) The case-identifier is the alias for the delegated type
            2) The delegated type itself is optional, it is then just like an Enum value
            3) The delegated type definition can have optional field names
            4) This is based on the Algebraic Type theory
    *)

    (* ==== Option type ==== *)
    // ---- An example with a light switch ----
    // ---- define a discriminated union type ---
    type switchState =
    | On    // -> only case-indentifier
    | Off   // -> only case-identifier
    | Between of float
    // represents a switch state which can be On, Off or at a point in between

    // ---- define a pattern function that operates on it ----
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
    // NOTE: extensive use of pattern matching!

    let lw1, lw2 = toggleLight On, toggleLight Off
    // val lw1 : switchState = Off
    // val lw2 : switchSatate = On
    let lw3, lw4 = toggleLight (Between(0.7)), toggleLight (Between(0.3))
    // val lw3 : switchState = Between(0.2)
    // val lw4 : switchSatate = Between(0.8)
    let lw5 = toggleLight (Between(0.5))
    // val lw5 : switchState = Off

    // ---- Hierarchical Data Structures ----
    // Discriminated Unions are commonly used to model 
    // hierarchical data structures such as trees

    // Define a binary tree as Disc Union
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
    // recursively walk through the tree
    let sumTree tree =
        let rec walk acc = function
            | Leaf (x) -> acc + x
            | Node (l, r) -> acc + (walk 0 l) + (walk 0 r)
        walk 0 tree
    let s = sumTree tr1
    // val s : int = 15

    // It is useful for expressing syntax trees
    // Consider an AST for a simple language for expressions with -
    // int primitives, variables & add/sub/mult/div operations

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
    let r1 = evalExp (Map.ofList ["x", 2]) exp1
    // val r1 : int = 102   <- 5 * 20 + 2

    // ==== Domain modelling ====
    // Discriminated Unions are commonly used for domain modelling in F#

    // Often even single cases are wrapped in DU, this enables shaping data
    // into fields and have a type name
    type  Process = | Service of id : int

    let goexec = Service 20034
    // we can unwrap this value directly 
    let (Service id) = goexec
    // val id : int = 20034
    // NOTE: We just used the case-identifire on the left

    
