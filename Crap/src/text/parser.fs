module parser

open Token
open Position
open lexer
open Syntax

exception Syntax of Message: string
exception EndOfText of Message: string
exception Indentation of Message: string

let Error (msg: string) pos =
    Syntax $"Parse Error: {msg} at {pos}"
    |> raise

let EOT pos =
    EndOfText $"Parse Error: End of tex at {pos}"
    |> raise

let IError msg pos = 
    Indentation $"Parse Error: {msg} at {pos}"
    |> raise

let rec inline Next line indent tokens = 
    let token, tokens = Seq.head tokens, Seq.tail tokens 
    let pos = PosOf token
    if pos.Line = line || (pos.Offset >>> 2) >= indent then
        token, tokens
    else
        Error $"indentation not correct" token.pos

and Tuple line indent tokens =
    try
        let value, tokens = Expr line indent tokens
        let token, tokens = Next line indent tokens
        match TypeOf token with
        | COMMA -> 
            let values, tokens = Tuple line indent tokens
            value :: values, tokens
        | RP -> [value], tokens
        | ty -> Error $"The token {ty} does not fit any pattern of a value" token.pos
    with err ->
        let token, tokens = Next line indent tokens
        match TypeOf token with
        | RP -> [], tokens
        | _ -> raise err


and Array line indent tokens =
    let value, tokens = Expr line indent tokens
    let token, tokens = Next line indent tokens
    match TypeOf token with
    | COMMA -> 
        let values, tokens = Array line indent tokens
        value :: values, tokens
    | RS -> [value], tokens
    | ty -> Error $"The token {ty} does not fit any pattern of a value" token.pos
    

and value line indent tokens = 
    let token, tokens = Next line indent tokens
    match TypeOf token with
    | EOF -> EOT token.pos
    | ID -> Variable(ValueOf token, info token), tokens
    | VALUE  -> Value((ValueOf token: Value), info token), tokens
    | LP -> 
        let e, tokens = Tuple line indent tokens
        if e.Length = 1 then
            List.head e, tokens
        else
            Syntax.Expr.Tuple(List.toArray e, info token), tokens
        
    | LS ->
        let e, tokens = Array line indent tokens
        Syntax.Expr.Array(List.toArray e, info token), tokens
        
    | ty  -> Error $"The token {ty} does not fit any pattern of a value" token.pos


and TupleBinding line indent tokens =
    let bind, tokens = Binding line indent tokens
    let token, tokens = Next (getinfo bind).pos.Line indent tokens
    match TypeOf token with 
    | COMMA -> 
        let bindings, tokens = TupleBinding line indent tokens
        bind::bindings, tokens

    | RP -> [bind], tokens
    | _ -> Error "not a Tuple binding" token.pos


// need to implement nested bindings
and Binding line indent tokens = 
    let token, tokens = Next line indent tokens
    match TypeOf token with
    | ID -> Variable(ValueOf token, info token), tokens
    | LP -> 
        let tp, tokens = TupleBinding line indent tokens
        Syntax.Expr.Tuple(List.toArray tp, info token), tokens
      
    | ty -> Error $"{ty} is not a legal declaration token" token.pos


and Bindings line indent tokens =
    let binding, tokens = Binding line indent tokens
    let token, tokens = Next (getinfo binding).pos.Line indent tokens
    match TypeOf token with
    | COMMA -> 
        let bindings, tokens = Bindings (getinfo binding).pos.Line indent tokens
        binding :: bindings, tokens
    | EQ ->
        [binding], tokens

    | ty -> Error $"{ty} is not a part of a valid binding" token.pos


and Args line indent tokens = 
    try
        let token, tokens' = Next line indent tokens
        match TypeOf token with
        | ID -> 
            let ids, tokens = Args line indent tokens'
            ValueOf token :: ids, tokens
        | EQ -> [], tokens'
        | ty -> Error $"expected a = but found {ty}" token.pos
    with
        | :? Indentation -> [], tokens
        | err -> raise err


and Params line indent tokens =
    try
        let param, tokens = Expr line indent tokens
        let params', tokens = Params line indent tokens
        param::params', tokens
    with
        | _ -> [], tokens


and Exprs line indent tokens =
    try
        let e, tokens = Expr line indent tokens
        let token, tokens' = Next line indent tokens
        match TypeOf token with
        | COMMA ->
            let es, tokens = Exprs line indent tokens'
            e :: es, tokens
        | IN -> 
            let cont, tokens = Expr line indent tokens'
            [Syntax.Expr.Continuation(e, cont, getinfo e)], tokens // indentation insensitive

        | EOF 
        | END -> [e], tokens'
        | ty -> Error $"{ty} not part of an expression Tuple" token.pos 
    with
        | :? Indentation -> [], tokens
        | err -> raise err


and Expr line indent tokens = 
    let token, tokens' = Next line indent tokens
    let indent' = indent + 1
    match TypeOf token with
    | LET ->
        let bindings, tokens = Bindings line indent' tokens'
        let es, tokens = Exprs line indent' tokens
        Declare(List.toArray bindings, List.toArray es, info token), tokens

    | FUN ->
        let token', tokens = Next line indent' tokens'
        match TypeOf token' with
        | ID -> 
            let arguments, tokens = Args line indent' tokens

            let body, tokens = 
                let cont, tokens = Continuation line  indent' tokens
                List.reduceBack (fun e cont -> Syntax.Expr.Continuation(e, cont, getinfo e)) cont, tokens

            Fun(ValueOf token', List.toArray arguments, body, info token), tokens

        | _ -> Error $"" start

    | MAP ->
        let map, tokens = Expr line indent' tokens'
        let iter, tokens = Expr line indent' tokens
        Map(map, iter, info token), tokens

    | FILTER ->
        let pred, tokens = Expr line indent' tokens'
        let iter, tokens = Expr line indent' tokens
        Filter(pred, iter, info token), tokens

    | FOLD ->
        let folder, tokens = Expr line indent' tokens'
        let acc, tokens = Expr line indent' tokens
        let iter, tokens = Expr line indent' tokens
        Fold(folder, acc, iter, info token), tokens

    | SCAN ->
        let folder, tokens = Expr line indent' tokens'
        let acc, tokens = Expr line indent' tokens
        let iter, tokens = Expr line indent' tokens
        Fold(folder, acc, iter, info token), tokens

    | FROM ->
        let pred, tokens = Expr line indent' tokens'
        let iter, tokens = Expr line indent' tokens
        From(pred, iter, info token), tokens

    | UNTIL ->
        let pred, tokens = Expr line indent' tokens'
        let iter, tokens = Expr line indent' tokens
        Until(pred, iter, info token), tokens

    | ID ->
        let params', tokens' = Params line indent' tokens'
        if List.isEmpty params' then
           binop1 line indent tokens 
        else
            Call(ValueOf token, List.toArray params', info token), tokens'
    | EOF -> EOT token.pos
    | _ -> binop1 line indent tokens 


and Continuation line indent tokens =
    try
        let e, tokens = Expr line indent tokens
        let cont, tokens = Continuation (line + 1) indent tokens
        e::cont, tokens
    with
        | :? EndOfText -> [], tokens
        | :? Indentation -> [], tokens
        | err -> raise err


and binop1 line indent tokens = 
    let left, tokens = binop2 line indent tokens
    let token, tokens' = Next line indent tokens
    match TypeOf token with
    | PLUS -> 
        let right, tokens = binop1 line indent tokens'
        Binary(Add, left, right, info token), tokens
    
    | MINUS -> 
        let right, tokens = binop1 line indent tokens'
        Binary(Sub, left, right, info token), tokens

    | _ -> left, tokens


and binop2 line indent tokens = 
    let left, tokens = value line indent tokens
    let token, tokens' = Next line indent tokens
    match TypeOf token with
    | STAR -> 
        let right, tokens = binop2 line indent tokens'
        Binary(Mul, left, right, info token), tokens
    
    | DASH -> 
        let right, tokens = binop2 line indent tokens'
        Binary(Div, left, right, info token), tokens

    | _ -> left, tokens



and Global line tokens =
    try
        let e, tokens = Expr line 0 tokens
        let globals, tokens = Global (getinfo e).pos.Line tokens
        e :: globals, tokens
    with :? EndOfText -> [], tokens

and Program tokens = 
    Global 0 tokens
    |> fst
    |> fun globals -> 
        Syntax.Program([||], List.toArray globals) 

let ParseCrap tokens = Program tokens
