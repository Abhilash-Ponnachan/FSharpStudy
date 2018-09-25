open System

module ActivePatterns =
    (* ==== Active Patterns ==== *)
    (*
        In F# pattern matching the pattern cases
        can only be literals and if we need additional logic, 
        then if has to be hendled via 'when guards'
    *)
    // ---- pattern matching example ----
    let ticketDiscount = function
        | x when x < 1 -> 0.5
        | x when x < 3 -> 0.25
        | _ -> 0.0

    (*
        Active patterns provide a way to define an anonymous type
        which can partition these pattern logics into named parts, 
        which can be used directly in pattern matching!
    *)
    // ---- active pattern for the above ----
    let (|Infant|Child|Adult|) age =
        if age < 1 then Infant
        elif age < 3 then Child
        else Adult
    (*
        NOTE: The active pattern returns 3 categories based on the input (age).
        The expression here is simple range check, but it can be 
        any valid F# expression.
        
        The active pattern has a signature like a function, e.g.
        int -> Choice<unit,unit,unit>

        => "active patterns are kind of like - functions & union types together!!"
        --------------------------------------------------------------------------

        Now let us see how we can use teh above active pattern in situations
        requiring pattern matching.
    *)
    let ticketPrice fare = function
        | Infant -> 0.5 * fare
        | Child -> 0.25 * fare
        | _ -> fare
    (*
        NOTE: 
            1) The age matching logic is now abstracted away -> cleaner code
            2) The partition names make the code intent clearer
            3) The partition logic (condition) and what we do with it (action)
                are now decoupled
    *)

    // ==== Parameterized active patterns ====
    // active pattern partitions themselves can take parameters, they resemble costructors!
    let (|PositiveMax|NegativeMax|) x = if x < 0 then PositiveMax else NegativeMax

    let getMax = function
        | PositiveMax -> Int32.MaxValue
        | NegativeMax -> 0

    // ==== Partial active patterns ====
    (*
        Sometimes active patterns do not always produce a value, these are called
        partial active patterns. They have a return types that is an Option 
    *)
    let (|DivisibleBy|_|) by n = if n % by = 0 then Some DivisibleBy else None

    let isLeapYear y =
        match y with
        | DivisibleBy 100 -> 
        | DivisibleBy 4 -> true
        | _ -> false
