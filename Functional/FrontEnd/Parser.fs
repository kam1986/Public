module Parser

open CompilerInfo
open AbstractSyntax.C
open Tokens
open Token
open Position
open Productions
open Parser
let ( <! ) (args: _[]) index = ValueOf args.[index]


exception ParserError of Message: string 

    
let ParserError (msg: string) (pos: Position) =
    ParserError $"Syntax Error:\n {msg} at {pos}\n"
    |> raise


let Info pos = Information<_,_,_>.Info pos

let AddType ty info = { info with Type = ty} 


// WARNING: dynamic typing i.e. type hidden away as obj type until needed 
// hence typing/argument order must be correct or else the program throw an exception
// This is on purpose because of two things, first we wouldn't be able to have unknown number of types (token type + up to each case of the productions) without compile error.
// Two it enable fine grain error handling directly in code.
// The user can use the two defined exception LexerError 'msg' and ParserError 'msg' or define all the exception they want to target specific type of errors and handle them here
// alternatively on exception the user can simple test each argument to find the correct error and return the right message with positional info. 
let parser tokens =
    Productions [
        File => [
            // global declared variables
            [!LET; !ID; !COLON; %Type; !EQUAL; %Expression;!SEMICOLON; %File]
            >> fun args -> 
                let globs, funcs = args <! 7
                let info = PosOf (args <! 0) |> Info |> AddType (Some((args <! 3)()))
                Global(args <! 1, info, args <!5) :: globs, funcs

            [!LET; !ID; !LPAR; %Ids; !RPAR; !EQUAL; !LBRA; %Statement; !RBRA; %File]
            >> fun args -> 
                let globs, funcs = args <! 9
                globs, Fun(args <! 1, args <! 3, None, args <! 7) :: funcs

               
            [] // end of file
            >> fun _ -> ([], [])
        ]
       
        Ids => [
            [!ID; !COMMA; %Type; %Ids]
            >> fun args -> ((args <! 2) (args <! 0)) :: (args <! 3)
            
            // tuple as input
            [!LPAR; %Ids; !RPAR; !COMMA; %Ids]
            >> fun args -> Tuple(args <! 1) :: (args <! 4)

            []
            >> fun _ -> []
        ]

        Statement => [
            [!LET; !ID; !COLON; %Type; !EQUAL; %Expression; !SEMICOLON]
            >> fun args -> 
                try
                    Dec((args <! 3)(), args <! 1, args <! 5)
                with 
                | :? ParserError as err -> raise err 
                | _ -> ParserError "Variable Declaration Error" (PosOf (args <! 0))

            [!ID; !ASSIGN; %Expression; !SEMICOLON]
            >> fun args -> 
                try
                    Assign(args <! 0, args <! 2)
                with
                | :? ParserError as err -> raise err  
                | _ -> ParserError "Assignment Error" (PosOf (args <! 0))
                    

            [!WHILE; %Expression; !LBRA; %Statement; !RBRA]
            >> fun args -> 
                try
                    While(args <! 1, args <! 3, None)
                with
                | :? ParserError as err -> raise err
                | _ -> ParserError "Loop Error" (PosOf (args <! 0))


            [!IF; %Expression; !THEN; !LBRA; %Statement; !RBRA; %Ite]
            >> fun args -> 
                try
                    If(args <! 1, args <! 4, args <! 6)
                with
                | :? ParserError as err -> raise err
                | _ -> ParserError "Branch Error" (PosOf (args <! 0))

                
            [!RETURN; % Expression; !SEMICOLON]
            >> fun args -> 
                try
                    Sequence(Expr (args <! 1), Return)
                with
                | :? ParserError as err -> raise err
                | _ -> ParserError "Return Error" (PosOf (args <! 0))


            [!BREAK; !SEMICOLON]
            >> fun args -> 
                try
                    Break
                with
                | :? ParserError as err -> raise err
                | _ -> ParserError "break Error" (PosOf (args <! 0))


            [!CONTINUE; !SEMICOLON]
            >> fun args -> 
                try
                    Continue
                with
                | :? ParserError as err -> raise err
                | _ -> ParserError "Continue Error" (PosOf (args <! 0))


            [%Statement; %Statement]
            >> fun args -> 
                try
                    Sequence(args <! 0, args <! 1)
                with
                | :? ParserError as err -> raise err
                | _ -> ParserError "Sequence Error" (PosOf (args <! 0))
        ]

        Ite => [
            [!ELSE; !LBRA; %Statement; !RBRA]
            >> fun args -> 
                try
                    Some(args <! 1)
                with
                | :? ParserError as err -> raise err
                | _ -> ParserError "Else Error" (PosOf (args <! 0))

            [!ELIF; %Expression; !THEN; !LBRA; %Statement; !RBRA; %Ite]
            >> fun args -> 
                try
                    Some(If(args <! 1, args <! 4, args <! 6))
                with
                | :? ParserError as err -> raise err
                | _ -> ParserError "Elif Error" (PosOf (args <! 0))
            
            [] // no error can happen here since it matches the empty case
            >> fun  _ -> None
        ]
        

        BinOps => [
            [!PLUS]
            >> fun arg -> 
                try
                    binOp.Add
                with
                | :? ParserError as err -> raise err
                | _ -> ParserError "Binop Error" (PosOf (arg <! 0))

            [!MINUS]
            >> fun arg -> 
                try
                    binOp.Sub
                with
                | :? ParserError as err -> raise err
                | _ -> ParserError "Binop Error" (PosOf (arg <! 0))



            [!STAR]
            >> fun arg -> 
                try
                    binOp.Mul
                with
                | :? ParserError as err -> raise err
                | _ -> ParserError "Binop Error" (PosOf (arg <! 0))

            [!SLASH]
            >> fun arg -> 
                try
                    binOp.Div
                with
                | :? ParserError as err -> raise err
                | _ -> ParserError "Binop Error" (PosOf (arg <! 0))


            [!REMINDER]
            >> fun arg -> 
                try
                    binOp.Rem
                with
                | :? ParserError as err -> raise err
                | _ -> ParserError "Binop Error" (PosOf (arg <! 0))

            [!AND]
            >> fun arg -> 
                try
                    binOp.Add
                with
                | :? ParserError as err -> raise err
                | _ -> ParserError "Binop Error" (PosOf (arg <! 0))

            [!OR]
            >> fun arg -> 
                try
                    binOp.Or
                with
                | :? ParserError as err -> raise err
                | _ -> ParserError "Binop Error" (PosOf (arg <! 0))

            [!XOR]
            >> fun arg -> 
                try
                    binOp.Xor
                with
                | :? ParserError as err -> raise err
                | _ -> ParserError "Binop Error" (PosOf (arg <! 0))

            [!SHIFTLEFT]
            >> fun arg -> 
                try
                    binOp.LeftShift
                with
                | :? ParserError as err -> raise err
                | _ -> ParserError "Binop Error" (PosOf (arg <! 0))


            [!SHIFTRIGHT]
            >> fun arg -> 
                try
                    binOp.RightShift
                with
                | :? ParserError as err -> raise err
                | _ -> ParserError "Binop Error" (PosOf (arg <! 0))


            [!ROTATELEFT]
            >> fun arg -> 
                try
                    binOp.Rotl
                with
                | :? ParserError as err -> raise err
                | _ -> ParserError "Binop Error" (PosOf (arg <! 0))


            [!ROTATERIGHT]
            >> fun arg -> 
                try
                    binOp.Rotr
                with
                | :? ParserError as err -> raise err
                | _ -> ParserError "Binop Error" (PosOf (arg <! 0))


            [!MIN]
            >> fun arg -> 
                try
                    binOp.Min
                with
                | :? ParserError as err -> raise err
                | _ -> ParserError "Binop Error" (PosOf (arg <! 0))

            
            [!MAX]
            >> fun arg -> 
                try
                    binOp.Max
                with
                | :? ParserError as err -> raise err
                | _ -> ParserError "Binop Error" (PosOf (arg <! 0))

            
            [!COPYSIGN]
            >> fun arg -> 
                try
                    binOp.CopySign
                with
                | :? ParserError as err -> raise err
                | _ -> ParserError "Binop Error" (PosOf (arg <! 0))
        ]

        RelOps => [
            [!EQUAL]
            >> fun arg -> 
                try
                    relOp.Eq
                with
                | :? ParserError as err -> raise err
                | _ -> ParserError "Relop Error" (PosOf (arg <! 0))
            
            [!NOT_EQUAL]
            >> fun arg -> 
                try
                    relOp.Nq
                with
                | :? ParserError as err -> raise err
                | _ -> ParserError "Relop Error" (PosOf (arg <! 0))
                
            [!LESS]
            >> fun arg -> 
                try
                    relOp.Less
                with
                | :? ParserError as err -> raise err
                | _ -> ParserError "Relop Error" (PosOf (arg <! 0))
                    
            [!LEQ]
            >> fun arg -> 
                try
                    relOp.Leq
                with
                | :? ParserError as err -> raise err
                | _ -> ParserError "Relop Error" (PosOf (arg <! 0))
                    


            [!GREATER]
            >> fun arg -> 
                try
                    relOp.Greater
                with
                | :? ParserError as err -> raise err
                | _ -> ParserError "Relop Error" (PosOf (arg <! 0))
                    
            [!GEQ]
            >> fun arg -> 
                try
                    relOp.Geq
                with
                | :? ParserError as err -> raise err
                | _ -> ParserError "Relop Error" (PosOf (arg <! 0))

        ]

        UnOps => [
            [!MINUS]
            >> fun args -> unOp.Neg
            
            [!ABS]
            >> fun args -> unOp.Neg
            
            [!CEIL]
            >> fun args -> unOp.Neg
            
            [!FLOOR]
            >> fun args -> unOp.Neg
            
            [!TRUNC]
            >> fun args -> unOp.Neg
            
            [!NEAREST]
            >> fun args -> unOp.Neg
            
            [!SQRT]
            >> fun args -> unOp.Neg            
        ]
        
        Expression => [
            [%Value]
            >> fun args -> 
                try
                    args <! 0
                with
                | :? ParserError as err -> raise err
                | _ -> ParserError "Relop Error" (PosOf (args <! 0))


            [%Type; %Expression]
            >> fun args -> 
                try
                    (args <! 0) (args <! 1)
                with
                | :? ParserError as err -> raise err
                | _ -> ParserError "Relop Error" (PosOf (args <! 0))

            [%Value; %BinOps; %Expression]
            >> fun args -> Binop(None, args <! 1, args <! 0, args <! 2)
        
            [%Value; %RelOps; %Expression]
            >> fun args -> Relop(None, args <! 1, args <! 0, args <! 2)

            [%UnOps; %Value]
            >> fun args -> Unop(None, args <! 0, args <! 1)

            [!IF; %Expression; !THEN; !LBRA; %Expression; !RBRA; %EIte]
            >> fun args -> Expr.If(None, args <! 1, args <! 4, args <! 6)
        ]

        EIte => [
            [!ELSE; !LBRA; %Expression; !RBRA]
            >> fun args -> Some(args <! 1)

            [!ELIF; %Expression; !THEN; !LBRA; %Expression; !RBRA; %Ite]
            >> fun args -> Some(If(args <! 1, args <! 4, args <! 6))
        ]

        Value => [
            [!ID]
            >> fun args -> Var(args <! 0)

            [!TRUE]
            >> fun args -> Val(COp.I32 1)

            [!FALSE]
            >> fun args -> Val(COp.I32 0)

            [!INTEGER]
            >> fun args -> 
                try 
                    Val(COp.I64(args <! 0)) 
                with
                | _ -> ParserError "Integer Value Error" (PosOf(args <! 0))
            [!FLOAT]
            >> fun args -> 
                try 
                    Val(COp.F64(args <! 0)) 
                with
                | _ -> ParserError "Integer Value Error" (PosOf(args <! 0))
        ]

        Type => [

            [!I8]
            >> fun args -> COp.I8
            
            [!I16]
            >> fun args -> COp.I16

            [!I32]
            >> fun args -> COp.I32

            [!I64]
            >> fun args -> COp.I64

            [!U8]
            >> fun args -> COp.U8
            
            [!U16]
            >> fun args -> COp.U16

            [!U32]
            >> fun args -> COp.U32

            [!U64]
            >> fun args -> COp.U64

            [!F32]
            >> fun args -> COp.F32

            [!F64]
            >> fun args -> COp.F64 
        ]
    ]
    |> fun pattern -> 
        Run (SLR pattern) tokens
        |> Result.mapError (function (ParserError msg) -> msg | err -> err.Message )
        |> Result.map (fun (globals, functions) -> Enviroment(Globals globals, [], Funs functions))
