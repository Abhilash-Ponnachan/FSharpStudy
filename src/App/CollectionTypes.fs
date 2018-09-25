open System

module CollectionTypes =
    (* ==== F# has 5 essential collection types ==== *)
    (*
        A typical idiom in functional programming is to chain operations
        on collections. F# provides a lot of operations on collections.
        We shall take each of the standard collection types and 
        examine a few of the common operations on them.
    *)

    (* ==== List ==== *)
    // in F# a List is an immutable, ordered collection of elements of same type
    // It is implemented as a linked-list
    // A list lietral is defined using '[]' and elements separated using ';'
    let nothing = [] // empty list
    let fruits = ["Apple"; "Orange"; "Banana"]
    // new-line is also a separator
    let fruits_n = [
        "Cherries"
        "Grapes"
        "Tomatos"
    ]
    // using a range iterable
    let nums = [1 .. 10]
    // val nums: int list = [1;2;3;4;5;6;7;8;9;10]

    (*
        computation to generate list and 'yield' each element.
        This is called a "List Comprehension" and is equivalent
        to Comprehensions in Python & Haskell
        It has the form - 
        [for <variable> in <enumerable> do [if <comdition> then] yield <expression>]]
    *)
    let months = [for i in 1..12 do yield (new DateTime(1,i,1)).ToString("MMM")]
    //val months : string list = ["Jan"; "Feb"; .. "Dec"]
    // NOTE: do (similar to Ruby code block) & yield (C# yield)

    let e_s = [for i in 1..10 do if i % 2 = 0 then yield i * i]
    // val e_s : int list = [4; 16; 36; 64; 100]
    // in Python we would write this as -
    // e_s = [i * i for i in range(1, 11) if i % 2 == 0]

    // ---- Cons operator '::' - appends item to head of list
    let l1 = 100 :: []
    let l2 = 200 :: l1
    
    // "cons" :: pattern to get head and tail of list
    let h :: t = l2
    // h = 200 ; t = [100]

    // printing items of a list using pattern match & recursive function
    let rec printList = function
    | [] -> printfn ""
    | h :: t -> printf "%A " h; printList t
    printList months
    // "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"
    // NOTE: ';' for next command in second match expression!

    // Using Lists and recursion to generate prime numbers
    let rec stripList listF listN =
        match listF with
        | h :: tail -> stripList tail (List.filter (fun x -> x = h || x % h <> 0) listN)
        | [] -> listN

    let genPrimeNums n =
        let max = int (sqrt (float n))
        stripList [2 .. max] [1 .. n]

    printfn "prime numbers upto %d is %A" 100 (genPrimeNums 100)

    // another way to generate primes with cons pattern, recursion & comprehension
    let rec seive = function
        | h :: t -> h :: seive [for i in t do if i % h <> 0 then yield i]
        | [] -> []
    printfn "prime numbers upto %d is %A" 50 (seive [2 .. 50])

    // yet another way - using List.filter
    let rec seive_1 = function
        | h :: t -> h :: seive_1 (t |> List.filter (fun x -> x % h <> 0))
        | [] -> []
    printfn "prime numbers upto %d is %A" 50 (seive_1 [2 .. 50]) 

    // NOTE -  The above seive functions can be made tail-recursive - 
    let rec seive_2 lst_nums lst_prms =
        match lst_nums with
        | h :: t -> seive_2 ( t |> List.filter (fun x -> x % h <> 0)) (h :: lst_prms)
        | [] -> lst_prms
    let prm_2 = seive_2 [2..1000] []

    // ---- Concat operator '@' - concatenates two lists
    let evens = [for i in 1..10 do if i % 2 = 0 then yield i]
    // val evens : int list = [2; 4; 6; 8; 10]
    let odds = [for i in 1..10 do if i % 2 <> 0 then yield i]
    // val odds : int list = [1; 3; 5; 7; 9]
    let numsAll = odds @ evens
    // val numsAll : int list = [1;3;5;7;9;2;4;6;8;10]

    // Access elemenst - use '.[]' notation
    printfn "element %d of list is %d" 3 numsAll.[3]
    // element 3 of list is 7

    // Common F# idiom is to use Higher Order List functions to operate over them
    let sumOf3MultSqrs = 
        numsAll
        |> List.filter (fun i -> i % 3 = 0)     // [3, 9, 6]
        |> List.map (fun i -> i * i)            // [9, 81, 36]
        |> List.reduce (fun acc x -> acc + x)   // 126 
    // val sumOf3MultSqrs = 126

    // ---- Search Lists ----
    // List.find - locate first element that matches a condition
    let m1 = months |> List.find (fun mon -> mon |> String.exists (fun c -> c = 'e'))
    printfn "First month with 'e' is %s" m1
    // First month with 'e' is Feb
    // NOTE: if the item is not found it throws a 'KeyNotFoundException'!
    // To avoid this we can use tryFind - it returns an 'option' type
    let m2 = 
        match months |> List.tryFind (fun mon -> mon |> String.exists (fun c -> c = 'y')) with
        | None -> "No month has 'y'"
        | Some m -> sprintf "%s has 'y'" m
    // May has 'y'

    // ---- Sort Lists ----
    // List.sort
    let m_s_1 = months |> List.sort
    // ["Apr"; "Aug"; "Dec"; "Feb"; "Jan"; "Jul"; "Jun"; "Mar"; "May"; "Nov"; "Oct"; "Sep"]

    // List.sortBy - takes a 'projection' function compute value to be "sorted by"
    let m_s_2 = months |> List.sortBy (fun m -> m.[m.Length - 1])
    // ["Feb"; "Dec"; "Aug"; "Jul"; "Jan"; "Jun"; "Sep"; "Mar"; "Apr"; "Oct"; "Nov"; "May"]
    // sorted by last character

    // List.sortWith - takes a 'comparison' function for sorting
    let m_s_3 = months |> List.sortWith 
                            (fun x y -> 
                                match x.[0], y.[0] with
                                | 'J', 'J' -> String.Compare(x, y)
                                | 'J', _ -> -1
                                | _, 'J' -> 1
                                | _, _ -> String.Compare(x, y))
    // ["Jan"; "Jul"; "Jun"; "Apr"; "Aug"; "Dec"; "Feb"; "Mar"; "May"; "Nov"; "Oct"; "Sep"]
    // sorted with preference for starting with 'J'

    // ---- List of Tuples ---
    // List.zip - merge two lists into one list of tuples
    let month_nums = List.zip [1 .. 12] months
    //  [(1, "Jan"); (2, "Feb"); (3, "Mar"); .. ; (12, "Dec")]

    // List.unzip - separates list of tuples to a tuple of lists
    let unziped = [(1, 'A'); (2, 'B'); (3, 'C'); (4, 'D')] |> List.unzip
    // val unziped : int list * char list = ([1; 2; 3; 4], ['A'; 'B'; 'C'; 'D'])

    (* ==== Array ==== *)
    // F# arrays are fixed size, mutable, ordered collections
    // Define array using '[| |]' and separate using ';'
    let arr1 = [|'a'; 'b'; 'c'; 'd'; 'e'|]
    // ---- Accessing an element '.[]' notation ----
    printfn "element in array at %d is %c" 3 arr1.[3]
    // element in array at 3 is d

    // ---- Array comprehension ----
    let arrSqrs = [|for i in 1..10 do yield i * i|]
    // val arrSqrs: int [] = [|1; 4; 9; 16; 25; 36; 49; 64; 81; 100|]
    // Most of the similar operations like list

    // ---- Array creation methods ----
    let arrEmpty = Array.empty
    // val arrEmpty : a' []
    let arrZeros = Array.create 5 0.0
    // array of 5 0.0
    // arrZeros : float [] = [|0.0; 0.0; 0.0; 0.0; 0.0|]
    let arrAlphaPairs = Array.init 25 (fun i -> new string ([|char (i + int 'a');  char (i + 1 + int 'a')|]))
    // val arrAlphaPairs : string [] =   [|"ab"; "bc"; "cd"; "de".. "xy"; "yz"|]

    // ---- Array slices ----
    arrAlphaPairs.[0..5]
    // val it : string [] = [|"ab"; "bc"; "cd"; "de"; "ef"; "fg"|]

    arrAlphaPairs.[5..8]
    // val it : string [] = [|"fg"; "gh"; "hi"; "ij"|]

    arrAlphaPairs.[..10]
    // val it : string [] =   [|"ab"; "bc"; "cd"; "de"; "ef"; "fg"; "gh"; "hi"; "ij"; "jk"; "kl"|]

    arrAlphaPairs.[20..]
    // val it : string [] = [|"uv"; "vw"; "wx"; "xy"; "yz"|]
    
    // ---- Multi-dimensional Arrays ----

    // array of arrays
    let arr2X2 = [|[|0; 1; 2|]; [|10; 11; 12|]|]
    // val arr2x2 : int [] [] = [| [|0; 1; 2|]; [10; 11; 12]|]
    arr2X2.[0].[2]
    // val it: int = 2

    // 2 dimensional array
    let arr2D = array2D arr2X2
    // val arr2D : int [,] = [[]; []]
    arr2D.[1, 2]
    // val it: int = 12

    // multi-dimensional array slices
    let matrix = Array3D.init 5 5 5 (fun i j k -> i*j*k)  
    matrix.[0..1, 0..1, *]

    // ---- Array items can be modified ----
    let arrNums = [|1 .. 10|]
    arrNums.[5] <- arrNums.[5] * 10

    // Array.fill
    Array.fill arrNums 5 5 0
    printfn "%A" arrNums
    // [|1; 2; 3; 4; 5; 0; 0; 0; 0; 0|]

    // ---- Higher Order Array functions ----
    let arrWords = [|"allen"; "bob"; "cathy"; "dan"|]
    let cntWordsWithA = 
        arrWords
        |> Array.filter (fun s -> s |> String.exists (fun c -> c = 'a'))
        |> Array.sumBy (fun _ -> 1)
    // val cntWordsWithA : int = 3

    (* ==== seq ==== *)
    (*
        "Sequence" is F# implementation of "iterable" (or .Net IEnumerable).
        It is represented by "seq<'T>" type which is an alias for 
        System.Collections.IEnumerable<T>. 
        It is a "Lazy Evaluated " collection. The values are generated only as needed
        and hence they are more efficient for large data sets.
        The "Seq" module provides the methods to operate over sequnces.
    *)
    // ---- Creating sequnces ----
    let seqEmpty = Seq.empty
    // val seqEmpty: seq<'a>
    // Seq.init - very similar to List.init
    let seqEvens = Seq.init 10 (fun x -> printfn ".. seq item - %d.. " x; 2 * x)
    // using an enumerator to read through the elements -
    let seqEnum = seqEvens.GetEnumerator()
    seqEnum.MoveNext()
    seqEnum.Current
    // .. seq item - 0 .. - gets called only when item is requested from sequence
    // val it : int = 0
    seqEnum.MoveNext()
    seqEnum.Current
    // .. seq item - 1 .. - gets called only when item is requested from sequence
    // val it : int = 2

    // since sequences are lazy evaluated, they can support infinite sequences!
    // calculating Pi using Leibniz formula 
    // pi = 4 * (1 - 1/3 + 1/5 - 1/7 + 1/9 ...)
    let piSeries = Seq.initInfinite (fun i ->
                    1.0 / (2.0 * float(i) + 1.0) * (if i % 2 = 0 then 1.0 else -1.0)
                    )
    let pi = 4.0 * (piSeries |> Seq.take 1000 |> Seq.sum )
    // val pi : float = 3.140592654

    // whilst Seq.initInfinite takes closed form functions
    // if we need a state to be passed along duing computation
    // we can use Seq.unfold
    // it has a generator func that has an Option type as state
    let piSeries_1 = Seq.unfold (fun s -> Some(1.0/s, if s > 0.0 then -(s + 2.0) else -(s - 2.0))) 1.0
    //for x in piSeries_1 |> Seq.take 10 do printfn "%A" x
    let pi_1 = 4.0 * (piSeries_1 |> Seq.take 1000 |> Seq.sum )
    // val pi_1 : float = 3.140592654

    // ---- Sequence expression ----
    // an expression that evaluates to a sequence
    let seqNums = seq {1 .. 100}
    printfn "%A" (seqNums |> Seq.take 10)
    // seq [1; 2; 3; 4; ...]
    let seqOdds = seq {1 .. 2 .. 100}
    printfn "%A" (seqOdds |> Seq.take 10)
    // seq [1; 3; 5; 7; ...]

    // ---- Sequence comprehension ----
    let seqComp = seq {for i in 1..100 do if i % 10 = 0 then yield i + 1}
    printfn "%A" (seqComp |> Seq.take 10)
    // seq [11; 21; 31; 41; ..]

    // short-hand form using '->' instead of 'do yield'
    let seqSqrs = seq {for i in 1..100 -> i * i}

    // permutation generator
    let numPlates = seq {
                            for a in 'A'..'Z' do
                                for i in 0..9 do    
                                    yield sprintf "%c%d" a i
                            }
    printfn "%A" (numPlates |> Seq.take 30)
    // seq ["A0"; "A1"; "A2"; ..] 

    // prime nums seq
    let isPrime n = 
        let l = n / 2
        let rec check i = 
            (i > l) || (n % i <> 0 && check(i + 1))
        check 2
    let seqPrimes = seq {for i in 2..100 do if isPrime i then yield i}
    printfn "%A" (seqPrimes |> Seq.take 20)
    // seq [2; 3; 5; 7; ..]

    // subsequence
    printfn "%A" (seqPrimes |> Seq.truncate 5)
    // Seq.truncate does NOT throw error if number of items is less than argument
    // - unlike Seq.take

    // ---- transforming sequences ----
    let seqAplhPairs = seq {'a' .. 'z'} |> Seq.pairwise
    // makes a sequence of pairwise tuples of adjacent elements
    printfn "%A" (List.ofSeq seqAplhPairs ) 
    // ('a', 'b'); ('b', 'c'); ('c', 'd'); .. ('y', 'z')
    
    let seqNumTrpls = seq {1.0 .. 0.5 .. 10.0} |> Seq.windowed 3
    // makes a sequence of n-item arrays of adjacent elements
    printfn "%A" (List.ofSeq seqNumTrpls)
    // [[|1.0; 1.5; 2.0|]; [|1.5; 2.0; 2.5|]; .. [|9.0; 9.5; 10.0|]]

    // counting items in a sequence by some key/criteria
    let multFives = seq {1 .. 100} |> Seq.countBy (fun i ->
                                                if i % 5 = 0 then 'Y'
                                                else 'N')
    // val multFives : seq<char * int>  
    // makes a sequence of tuples with key * count by criteria/key                                                                                 
    printfn "%A" (List.ofSeq multFives)
    // [('N', 80); ('Y', 20)]

    // sorting a sequence
    // NOTE: sorting will iterate though the whole sequnce so it is not suited  
    // for large or infinite sequences
    let seqRandom = [('a', 23); ('b', 76); ('c', 13); ('d', 8); ('e', 34); ('f', 56)] |> Seq.ofList
    let srtSeq = seqRandom |> Seq.sortBy (fun t -> snd t)
    srtSeq |> Seq.iter (printf "%A "); printfn ""
    // ('d', 8) ('c', 13) ('a', 23) ('e', 34) ('f', 56) ('b', 76)

    // grouping a sequnce
    let seqGrpsOf10 = seq {1 .. 50} |> Seq.groupBy (fun e -> 
                                                        match e / 10 with 
                                                        | x when x < 1 -> 1
                                                        | x when x < 2 -> 2
                                                        | x when x < 3 -> 3
                                                        | x when x < 4 -> 4
                                                        | x when x < 5 -> 5
                                                        | _ -> 0                                                       
                                                        )
    // val seqGrpsOf10 : seq<int * seq<int>>
    printfn "%A" seqGrpsOf10
    // seq [(1, seq [1; 2; 3; .. 9]); (2, seq [10; 11; 12 .. 19]); .. (5, seq [40; 41; .. 49])]
    // This returns a sequnce of tuples each of which has teh first element as teh projection key
    // and the second element as a sub sequence.
    // NOTE : The whole sequnce gets unwound, so do NOT use with large/infinite sequences!

    // ---- read only sequences ----
    // used to derive a readonly sequence from an array, often for other functions
    let arrForSeq = Array.ofSeq {1 .. 5}
    // arrForSeq : int [] = [|1; 2; 3; 4; 5|]
    let seqRdOnly = Seq.readonly arrForSeq
    // readonly sequence

    // ---- cached sequences ----
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
    // nothing printed !! - the sequence is evaluated only once
    printfn "^ = 4th iteration"

    (* ==== Tuples ==== *)
    (*
    A tuple is an ordered grouping of values of possibly different types.
    The general syntax -
        reference tuple - (element1, element2, ...)
        struct tuple - struct(element1, element2, ...)
    NOTE : Separator is ',' unlike lists/arrays which have ';'
    *)

    // --- creating a tuple ---
    let score = ("Alan", 87.1)
    // val score : string * float = ("Alan", 87.1)
    // NOTE: the type of tuple is represented with types separated by '*'

    // --- accessing values ---
    let name = fst score
    // val name : string = "Alan" - first element
    let marks = snd score
    // val marks : float = 87.1 - second element
    // NOTE - There is no method for 3rd, 4th elements etc.
    
    // tuple deconstruction 
    let nm, mk = score
    // val nm : string = "Alan"
    // val mk : float = 87.1
    // NOTE : tuple deconstruction is similar to Python
    // however in F# we can use pattern matching

    // use wild-card '_' when we need only part of the elements
    let (_, s) = score
    // val s : float = 87.1
    
    // using pattern matching -
    let point3D = (26.1, 23.6, 45.2)
    let distFrom0 pt = 
        match pt with
        | (x, y, z) -> sqrt (float(x * x + y * y + z *z))
    let d = distFrom0 point3D
    // val d : float = 57.28184704

    // NOTE : pattern match directly in function parameter
    let distFrom0X (x, y, z) = sqrt (float(x * x + y * y + z *z))
    printfn "%f" (distFrom0X point3D)

    // function to access 3rd element
    let trd (_, _, x) = x
    printfn "third element of %A is %A" point3D (trd point3D)
    // third element of (26.1, 23.6, 45.2) is 45.2