module Stack
(*

    A generic resizeable stack
    it handle "empty slots" by copying known values into unused space
    it will either copy primitive values (1 - 8 bytes) or a pointers to a complex ones (4 or 8 bytes)
    
    The user is to check if the stact is empty to catch the cases

*)
/// A generic resizeable stack type based on arrays
type 'a Stack =
    val mutable private stack: 'a []
    val mutable private count: int
    val mutable internal readable : int 

    /// Instantiate a new stack
    new() = { stack = [||]; count = 0; readable = 0 }

    member internal S.Size = S.stack.Length

    /// Return the number of elements on the stack
    member S.Count = S.count

    member S.Show map =
        printf "["
        for i in 0 .. S.count - 1  do
            printf "%A " (map S.stack.[i])
        printfn "]\n"
    /// Push the value onto the stack
    member S.Push value =
        if S.count >= S.Size then
            let size = if S.Size = 0 then 1 else S.Size
            S.stack <- [| for elm in 0 .. 2 * size - 1 -> if elm < S.Size then S.stack.[elm] else value |]
            // since we already has copied the value to the right position we simply increment count
            S.count <- S.count + 1
        else
            S.stack.[S.count] <- value
            S.count <- S.count + 1 

    /// Pop the top most element of the stack
    member S.Pop =
        let size = S.Size >>> 2 // division by 4
        if S.count < size then 
            S.stack <- [| for elm in 0 .. size - 1 -> S.stack.[elm]|]
        
        S.count <- S.count - 1
        S.stack.[S.count]

    // Return true if the stack is empty
    member S.IsEmpty = S.count = 0

    /// Index into the readable part of the stack from the top down
    member S.Item with get(i) = 
        let index = S.count - i - 1
        // to prevent miss usage of the stack content inside the parser
        // we have added the readable handle to the stack
        if index < S.readable then 
            printfn "index error: i was %d, index was %d readable was %d\n\n" i index S.readable
            exit -1
            // System.IndexOutOfRangeException() |> raise
        else
            // since index is zero based we subtract 1
            // indexing is from the top of the stack and down
            S.stack.[index]

    /// Peek at the top most element of the stack
    member S.Top = S.stack.[S.count-1]

    /// Drop the n top elements of the stack
    member S.Drop n =
        S.count <- S.count - n
        if S.count < 0 then 
            S.count <- 0

    /// Return None when the stack is empty else it return Some Top
    member S.TryTop = if S.count = 0 then None else Some S.Top

    // Return None when the stack is empty else Return the top most element as an option type 
    member S.tryPop =
        if S.count = 0 then None else Some S.Pop