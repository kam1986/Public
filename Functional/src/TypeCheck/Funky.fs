namespace TypeCheck

open AbstractSyntax.Funky
open SymTab
open Helpers

module Funky =
    
    let TypeError expected actual info = CompileError "Type Error" $"Expected type {expected} but found {actual}" info.pos
    
    
    let i8  = T(I8())
    let i16 = T(I16())
    let i32 = T(I32())
    let i64 = T(I64())
    let u8  = T(U8())
    let u16 = T(U16())
    let u32 = T(U32())
    let u64 = T(U64())
    let f32 = T(F32())
    let f64 = T(F64())
    let array ty = T(Array[ty])
        

    let ( % ) = Set.union
    
    let IsNumType ty = 
        set[
            T(Bool())
            T(I8())
            T(I16())
            T(I32())
            T(I64())
            T(U8())
            T(U16())
            T(U32())
            T(U64())
            T(F32())
            T(F64())
            T One
            T Zero
        ] % ty 
        |> Set.isEmpty


    let rec TypeOf v =
        match v with
        | V(Zero)    -> T(Zero)
        | V(One)     -> T(One)
        | V(Bool _)  -> T(Bool())
        | V(I8  _)   -> T(I8())
        | V(I16 _)   -> T(I16())
        | V(I32 _)   -> T(I32())
        | V(I64 _)   -> T(I64())
        | V(U8  _)   -> T(U8())
        | V(U16 _)   -> T(U16())
        | V(U32 _)   -> T(U32())
        | V(U64 _)   -> T(U64())
        | V(F32 _)   -> T(F32())
        | V(F64 _)   -> T(F64())
        | V(Array v) -> T(Array(List.map TypeOf v))
        | V(Record fields) -> T(Record <| List.map (fun (id, v) -> id, TypeOf v) fields)




    let GetType = function
        | Val(_, info)       
        | Var(_, info)       
        | Declare(_, _, _, info)   
        | If(_, _, _, info)
        | Neg(_, info)       
        | Not(_, info)       
        | Add(_,_,info)       
        | Sub(_,_,info)       
        | Mul(_,_,info)       
        | Div(_,_,info)       
        | Rem(_,_,info)       
        | LeftShift (_,_,info) 
        | RightShift(_,_,info)
        | And (_,_,info)      
        | Or  (_,_,info)       
        | Xor (_,_,info)      
        | Imply  (_,_, info)  
        | Greater(_,_, info)   
        | Geq    (_,_, info)   
        | Eq     (_,_, info)   
        | Nq     (_,_, info)   
        | Leq    (_,_, info)   
        | Less   (_,_, info) -> info.ty 
        | Call   (_,_, info) -> 
            Set.map (function F(_, ret) -> set[ret]) info.ty
            |> Set.fold (+) Set.empty

    let Type info ty = { info with ty = ty }



    let IsBool ty = Set.contains (T(Bool())) ty

    let Unify type1 type2 =
        let union = type1 % type2
        if Set.isEmpty union then
            Error "Unification Error"
        else
            Ok union 
            
                    
    

    // For now Zero and One are just generic Types
    // they are substituded by specific types in a later fase
    let rec CheckExpr vtab ftab e =
        match e with
        | Val(v, info) -> Ok(Val(v, Type info (set[TypeOf v])))
        | Var(name, info) -> Result.map (fun ty -> Var(name, Type info (GetType ty))) (LookUp vtab name)
        | Declare(name, body, context, info) -> 
            Result.mapWithError (fun body ->
                let vtab = Bind vtab name body
                Result.map (fun context -> Declare(name, body, context, { info with ty = GetType context })) (CheckExpr vtab ftab body)
                ) (CheckExpr vtab ftab context)
        
        | If(cond, left, right, info) ->
            let cond' = CheckExpr vtab ftab cond
            let left' = CheckExpr vtab ftab left
            let right' = CheckExpr vtab ftab right
            match cond', left', right' with
            | Error msg, _, _ | _, Error msg, _ | _, _, Error msg -> Error msg
            | Ok cond, Ok left, Ok right when GetType cond |> IsBool -> 
                let type1 = GetType left
                let type2 = GetType right
                Unify type1 type2
                |> Result.map (fun newtype -> If(cond, left, right, Type info newtype))
            | Ok cond, _, _ -> Error $"Expected a boolean but found {GetType cond} at {info.pos}"

        | Not(e, info) -> 
            let e = CheckExpr vtab ftab e
            match e with
            | Error msg -> Error msg
            | Ok e when GetType e |> IsBool -> Ok(Neg(e, Type info (set[T(Bool())])))
            | Ok e -> Error $"Expected a boolean but found {GetType e} at {info.pos}"
        
        | Neg(e, info) -> 
            let e = CheckExpr vtab ftab e
            match e with
            | Error msg -> Error msg
            | Ok e when GetType e |> IsNumType -> Ok(Neg(e, Type info (GetType e)))
            | Ok e -> Error $"Expected a numtype but found {GetType e} at {info.pos}"

        | Add(left, right, info)        -> CheckBinOp vtab ftab Add left right info      
        | Sub(left, right, info)        -> CheckBinOp vtab ftab Sub left right info      
        | Mul(left, right, info)        -> CheckBinOp vtab ftab Mul left right info      
        | Div(left, right, info)        -> CheckBinOp vtab ftab Div left right info      
        | Rem(left, right, info)        -> CheckBinOp vtab ftab Rem left right info      
        | LeftShift(left, right, info)  -> CheckBinOp vtab ftab LeftShift left right info
        | RightShift(left, right, info) -> CheckBinOp vtab ftab RightShift left right info
        | And(left, right, info)        -> CheckBinOp vtab ftab And left right info      
        | Or(left, right, info)         -> CheckBinOp vtab ftab Or  left right info
        | Xor(left, right, info)        -> CheckBinOp vtab ftab Xor left right info
        | Imply(left, right, info)      -> CheckBinOp vtab ftab Imply left right info
        | Greater(left, right, info)    -> CheckBinOp vtab ftab Greater left right info
        | Geq(left, right, info)        -> CheckBinOp vtab ftab Add left right info
        | Eq(left, right, info)         -> CheckBinOp vtab ftab Add left right info
        | Nq(left, right, info)         -> CheckBinOp vtab ftab Add left right info
        | Leq(left, right, info)        -> CheckBinOp vtab ftab Add left right info
        | Less(left, right, info)       -> CheckBinOp vtab ftab Add left right info
        | Call(name, args, _)           ->
            match LookUp ftab name with
            | Error msg -> Error msg
            | Ok (ids, body) -> 
                let args = 
                    List.map (CheckExpr vtab ftab) args
                    |> List.fold 
                        (fun args arg ->
                            match args, arg with
                            | Error msg, _ | _, Error msg -> Error msg 
                            | Ok args, Ok arg -> Ok(arg :: args) 
                        ) (Ok[])
                    |> Result.map List.rev

                match args with
                | Error msg -> Error msg
                | Ok args -> 
                    let vtab = List.fold2 (fun tab id arg -> Bind tab id arg) vtab ids args
                    CheckExpr vtab ftab body

    and CheckBinOp vtab ftab op left right info = 
        match CheckExpr vtab ftab left, CheckExpr vtab ftab right with
        | Error msg, _ | _, Error msg -> Error msg
        | Ok left, Ok right ->
            let ltype, rtype = GetType left, GetType right
            match Unify ltype rtype with
            | Error msg -> Error msg
            | Ok ntype -> Ok(op(left, right, Type info ntype))