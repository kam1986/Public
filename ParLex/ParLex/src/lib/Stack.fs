module Stack


type Stack<'item> =
    val mutable stack : 'item option []
    val mutable top : int
    new() = 
        { 
            stack = [|None; None|]
            top = -1
        }

with 
    override S.ToString() =
        Array.map (function Some t -> string t | _ -> "") S.stack
        |> Array.filter ((<>) "")
        |> Array.rev
        |> String.concat ", " 
        |> fun stack -> $"[{stack}]"

    member S.isEmpty() = S.top = -1

    member S.Peek() = 
        if S.top < 0 then
            failwith "stack is empty"

        S.stack.[S.top].Value


    member S.Push item =
        S.top <- S.top + 1
        if S.stack.Length = S.top then
            S.stack <- 
                Array.init 
                    (S.stack.Length * 2) 
                    (fun i -> 
                        if i < S.stack.Length then 
                            S.stack.[i]
                        else
                            None
                    )

        S.stack.[S.top] <- Some item
    

    member S.Drop() = S.Pop() |> ignore

    member S.Drop n = S.Pop n |> ignore

    member S.Pop() =
        if S.top < 0 then
            failwith "stack is empty"

        let item = S.stack.[S.top]
        S.stack.[S.top] <- None

        S.top <- S.top - 1
        // if only 1/4 of the stack is full 
        if S.stack.Length >>> 2 = S.top && S.stack.Length > 2 then
            S.stack <-
                Array.init (S.stack.Length >>> 1) (fun i -> S.stack.[i])
        item.Value

    member S.Pop n = Array.rev [|for _ in 1 .. n -> S.Pop() |]