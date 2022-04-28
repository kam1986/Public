namespace CodeGen

module Parser =

    type Parser<'input, 'output> = Parser of ('input seq -> Result<'output * 'input seq, string>)


    let Run (Parser p) = p


    let Expect item =
        fun input ->
            match Seq.tryHead input with
            | None -> Error "EOF"
            | Some x when x = item -> Ok(item, Seq.tail input)
            | Some x  -> Error $"the value {x} is not {item}"
        |> Parser

    let Predict pred =
        fun input ->
            match Seq.tryHead input with
            | None -> Error "EOF"
            | Some x when pred x -> Ok(x, Seq.tail input)
            | Some x  -> Error $"the value {x}"
        |> Parser

    let Bind item = 
        fun input -> Ok(item, input)
        |> Parser


    let ( <|> ) parse1 parse2 =
        fun input ->
            match Run parse1 input with
            | Ok ret -> Ok ret
            | _ -> Run parse2 input
        |> Parser

    let Choose plt =
        if List.isEmpty plt then
           fun _ -> Error "Empty list in choose function"
           |> Parser
        else
            List.reduce (<|>) plt


    let (<&>) parse1 parse2 =
        fun input ->
            match Run parse1 input with
            | Error msg -> Error msg
            | Ok (ret1, input) ->
                Run parse2 input
                |> Result.map (fun (ret2, input) -> ((ret1, ret2), input))
        |> Parser

    let Map f parse =
        fun input -> 
            Run parse input 
            |> Result.map (fun (ret, input) -> f ret, input)
        |> Parser



    let Apply func param = Map (fun (f, ret) -> f ret) (func <&> param)

    let (=>) func param = Apply func param

    let Lift2 func param1 param2 = 
        func => param1 => param2
         

    let inline Sequence f acc plst =
        Seq.foldBack (fun p acc -> (Lift2 (Bind f)) acc p) plst (Bind acc) 


    