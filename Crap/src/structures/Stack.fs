module Stack

type 'a Stack = Stack of 'a list with static member inline empty : 'a Stack = Stack [] 


let Pop (Stack s as st) =
    match s with
    | [] -> None, st
    | x :: xs -> Some x, Stack xs

let Push (Stack s) item = item :: s |> Stack

let Size (Stack s) = s.Length

let ToArray (Stack s) = Array.ofList s