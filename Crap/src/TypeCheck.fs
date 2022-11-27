module TypeCheck

#nowarn "25" "62" "1125"

open Syntax
open Table
open Stack
open Unique


exception TypeError of Message: string

let TypeError (msg: string) =
    TypeError $"Type Error: {msg}"
    |> raise



let mutable private t = 0
    
let internal newType() =
    let t' = t
    t <- t + 1
    t'

// polymorphic types
type ty =
    | No
    | Var  of string
    | Poly of string [] * ty
    | App  of tycon * ty []
    
// the primitive types are represented by Ty
and tycon =
    | Ty of Syntax.Type
    | Unit
    | Arrow
    | Array
    | Tuple
    | Record of fields: string []
    | TyFun of string [] * ty
    | Unique of tycon * unique
with
    // implement custom equality since Unique allows for fast equality comparison
    // and the TyFun can be optimized by testing return type first
    static member op_Equality (tc1, tc2) =
        match tc1, tc2 with
        | Unique(_, u1), Unique(_, u2) -> u1 = u2
        | Ty t, Ty t1 -> t = t1
        | TyFun(ids1,ty1), TyFun(ids2, ty2) -> ty1 = ty2 && ids1 = ids2
        | Arrow, Arrow -> true
        | _ -> false


let Poly ids t = Poly(ids, t)
let App tc ts = App(tc, ts)
let TyFun ids t = TyFun(ids, t)
let Unique tc = Unique(tc, unique())

let LookUp tab key =
    match Lookup key tab with
    | None -> TypeError $"the type id {key} is not defined"
    | Some t -> t


let rec subst t1 ttab =
    match t1 with
    | Var id ->
        match Lookup id ttab with
        | None -> Var id
        | Some t -> t
    
    | No -> No
    
    | App(TyFun(args, t), tys) ->
         let ttab' = Array.fold2 (fun tab key item -> Bind key item tab) ttab args tys
         subst (subst t ttab') ttab
    
    | App(tc, tys) -> 
        App tc (Array.map (fun t -> subst t ttab) tys)
    
    | Poly(alphas, u) ->
        let gammas = Array.map (fun _ -> $"tmp_{newType()}") alphas
        let ttab' = Array.fold2 (fun ttab key item -> Bind key (Var(item)) ttab) ttab alphas gammas
        let u' = subst u ttab'
        subst u' ttab
        |> Poly gammas 

// is 0-ary
let is0ary t =
    match t with
    | Arrow
    | Unit
    | Ty _ -> true
    | _ -> false

let ttab = Table.empty

let reportError msg f = f msg

// return a function which takes an error message
// this is done since we don't know where the error happens before
// application of unify
let rec unify t1 t2 =
    match t1, t2 with
    | App(tycon1, ts), App(tycon2, us) when is0ary tycon1 && tycon1 = tycon2 -> 
        fun msg -> Array.iter2 (fun t1 t2 -> unify t1 t2 msg) ts us

    | App(TyFun(args, u),ts), t ->
        fun msg ->
            Array.fold2 (fun tab key ty -> Bind key ty tab) ttab args ts
            |> subst u          // under the updated type table (ttab)
            |> unify t          // unify t with the resulted of substituded u
            |> reportError msg  // on error report msg

    | App(Unique _, ts), App(Unique _, ts') ->
        fun msg ->
            if t1 <> t2 then
                TypeError msg
            else
                // unify all types of the two
                Array.iter2 (fun t1 t2 -> unify t1 t2 msg) ts ts'

    | Poly(alphas, u), Poly(alphas', u') ->
        fun msg ->
            Array.fold2 (fun tab key item -> Bind key (Var(item)) tab) ttab alphas alphas'
            |> subst u'
            |> unify u
            |> reportError msg

    | Var _, Var _ -> fun _ -> ()
    // OBS! need record type implementation
    | _ -> TypeError 
    

let Unify t1 t2 =
    try
        unify t1 t2 ""
        Some t1
    with _ -> None


let rec expand t =
    match t with
    | App(TyFun(alphas, u), ts) ->
        Array.fold2 (fun tab key t -> Bind key t tab) ttab alphas ts
        |> subst u
        |> expand

    | App(Unique(tycon, _), ts) -> expand(App tycon ts)
    | _ -> t



//    Implement translating syntax tree into type tree


let typed info t =
    {
        pos = info.pos
        ty = t
    }

let ValiateValue v =
    match v with
    | Bool _ -> App (Ty(Bool())) [||]
    | I8 _   -> App (Ty(I8()))   [||]
    | I16 _  -> App (Ty(I16()))  [||]
    | I32 _  -> App (Ty(I32()))  [||]
    | I64 _  -> App (Ty(I64()))  [||]
    | U8 _   -> App (Ty(U8()))   [||]
    | U16 _  -> App (Ty(U16()))  [||]
    | U32 _  -> App (Ty(U32()))  [||]
    | U64 _  -> App (Ty(U64()))  [||]
    | F32 _  -> App (Ty(F32()))  [||]
    | F64 _  -> App (Ty(F64()))  [||] 


let bty = 
    [
        I8()
        I16()
        I32()
        I64()
        U8()
        U16()
        U32()
        U64()
        F32()
        F64()
    ]



let pop stack msg =
    match Pop stack with
    | None, _ -> TypeError msg
    | Some item, stack -> item, stack



let rec ValidateFunc info func args vtab ftab stack =
    match Lookup func ftab with
    | None -> TypeError $"The function {func} at {info.pos} are not defined"
    | Some (args, fty) ->
        
    vtab, ftab, stack



and ValidateExpr e vtab ftab stack =
    match e with
    | Value(v, _) ->  
        ValiateValue v 
        |> Push stack
        |> fun stack -> vtab, ftab, stack
        
    | Variable(name, info) -> 
        Lookup name vtab
        |> Option.defaultValue (TypeError $"The variable {name} at {info.pos} are not defined")
        |> Push stack
        |> fun stack -> vtab, ftab, stack

    | Declare(ids, bodies, info) ->
        let tys = 
            Array.foldBack (fun body (vtab, ftab, stack) -> ValidateExpr body vtab ftab stack) bodies (vtab,ftab, Stack.empty)
            |> fun (_, _, stack) -> Stack.ToArray stack

        let ids = 
            if Array.forall (function Variable _ -> true | _ -> false) ids then
                Array.map (function Variable(id, _) -> id) ids
            else
                TypeError $"Trying to bind a variable to a non identifer expression at {info.pos}"

        let vtab, ftab = 
            Array.fold2 
                (fun (vtab, ftab) id ty -> 
                    match ty with
                    | Poly (ids, ty) -> vtab, Bind id (ids, ty) ftab
                    | _ -> Bind id ty vtab, ftab
                ) (vtab, ftab) ids tys

        (vtab, ftab, stack)


    // OBS! the function type is accumulated as single argument functions to handle partial applied functions
    // need to handle mutiple instance types
    | Fun(f, args, body, info) ->

        // create unique type variables
        let tyvars = Array.map (fun _ -> (Var($"tyvar {newType()}"))) args

        // bind them to each argument identifier
        let vtab' = Array.fold2 (fun tab id tyvar -> Bind id tyvar tab) vtab args tyvars

        // define a generic return type
        let rty = Var($"tyvar {newType()}")

        // build generic function type with correct number of argument types
        let fty = 
            tyvars
            |> Array.fold (fun rty tyarg -> App Arrow [|tyarg; Var($"tyvar {newType()}")|]) rty
            |> Poly args

        // bind the function identifier to the function type
        let ftab' = Bind f (args, fty) ftab

        // Validate the body under the updated valuetype table and function type table.
        let vtab'', _, stack'' = ValidateExpr body vtab' ftab' Stack.empty

        // get the updated argument types
        let tyargs = Array.map (fun id -> LookUp vtab'' id) args

        // update return type
        let rty = 
            if Stack.Size stack'' = 0 then
                No
            elif Stack.Size stack'' = 1 then
                Pop stack''
                |> fst
                |> Option.defaultValue (TypeError "will never happen")
            else
                App Tuple (Array.rev <| Stack.ToArray stack'')
       
        // update function type
        let fty = 
            tyargs
            |> Array.fold (fun rty tyarg -> App Arrow [|tyarg; Var($"tyvar {newType()}")|]) rty
            |> Poly args


        // bind the function identifier to the updated function type
        let ftab' = Bind f (args, fty) ftab

        // return the valuetype table, the updated funciton type table, and the stack
        (vtab, ftab', stack)



    | Expr.Tuple(entries, info) ->
        let (vtab, ftab, stack') = Array.foldBack (fun entry (vtab, ftab, stack) -> ValidateExpr entry vtab ftab stack) entries (vtab, ftab, Stack.empty)
        let ty = App Tuple (Stack.ToArray stack')
        (vtab, ftab, Push stack ty)

    | Expr.Array(entries, info) ->
        let (vtab, ftab, stack') = Array.foldBack (fun entry (vtab, ftab, stack) -> ValidateExpr entry vtab ftab stack) entries (vtab, ftab, Stack.empty)
        let tys = Stack.ToArray stack'
        let ty' =
            if tys.Length = 0 then
                [|Var $"tyvar {newType()}"|]
            elif Array.forall (fun t -> (Unify t tys.[0]).IsSome) tys then
                [|tys.[0]|]
            else
                TypeError $"All entries of the array at {info.pos} much be of the same type"

        let ty = App Array ty'
        (vtab, ftab, Push stack ty)

    | If(cond, tb, fb, info) ->
        let _, _, cstack = ValidateExpr cond vtab ftab Stack.empty
        match Pop cstack with
        | Some (App(Ty(Bool()), [||])), _ -> 
            let _, _, rett = ValidateExpr tb vtab ftab Stack.empty
            let _, _, retf = ValidateExpr fb vtab ftab Stack.empty
            // check if the stack are unifyable
            Array.iter2 
                (fun tt tf -> 
                    unify tt tf $"The type of the two branches of the if expression at {info.pos} are not the same"
                ) (Stack.ToArray rett) (Stack.ToArray retf)
            vtab, ftab, rett
                        
        | _ -> TypeError $"The condition of the if expression at {info.pos} are not of type bool"

    | Convert(cvt, info) ->
        let e = op.Arg cvt
        ValidateExpr e vtab ftab stack
        |||> ValidateFunc info ((pptype cvt).ToLower()) 1

    | Call(f, args, info) ->
        Array.fold (fun env arg -> env |||> ValidateExpr arg) (vtab, ftab, stack) args
        |||> ValidateFunc info f args.Length
            
    | Continuation(first, follows, info) ->
        // it might need to be restricted to no function type on the 'first'?
        (vtab, ftab, stack)
        |||> ValidateExpr first
        |||> ValidateExpr follows

    | Unary(op, e, info) ->
        (vtab, ftab, stack)
        |||> ValidateExpr e 
        |||> ValidateFunc info (op.ToString().ToLower()) 1


    | Binary(op, left, right, info) ->
        (vtab, ftab, stack)
        |||> ValidateExpr left
        |||> ValidateExpr right
        |||> ValidateFunc info (op.ToString().ToLower()) 2
        
    | Zip(collections, info) ->
        (vtab, ftab, stack)

    | Map(f, collection, info) ->
        (vtab, ftab, stack)

    | Filter(pred, collections, info) ->
        (vtab, ftab, stack)
        
    | Fold(f, acc, collections, info) -> 
        (vtab, ftab, stack)

    | Scan(f, acc, collections, info) ->
        (vtab, ftab, stack)

    | Rev(e, info) ->
        (vtab, ftab, stack)

    | Until(pred, collections, info) ->
        (vtab, ftab, stack)

    | From(pred, collections, info) ->
        (vtab, ftab, stack)




(*      
 // polymorphic types
 type ty =
     | No
     | Var  of int
     | Poly of int [] * ty
     | App  of tycon * ty []
     
 // the primitive types are represented by Ty
 and tycon =
     | Ty of Syntax.Type
     | Unit
     | Arrow
     | Array
     | Tuple
     | TyFun of int [] * ty
     | Unique of tycon * unique
*)

(*
let rec CheckExpr stack ttab vtab ftab e =
    match e with
    | Value(v, info) ->
        let t = CheckValue v
        Value(v, typed info t)
    
    | Variable(name, info) ->
        let t = LookUp vtab name
        Variable(name, typed info t)

    // need to specify for special instance of op
    | Binary(op, left, right, info) ->
        let left = CheckExpr stack ttab vtab ftab left
        let right = CheckExpr stack ttab vtab ftab right
        let lt = (getinfo left).ty
        let rt = (getinfo right).ty
        
        // check it there is a function that 
        let t = 
            LookUp ftab (string op) 
            |> Array.map (fun t -> Unify t lt, Unify t rt, t)
            |> Array.choose (fun (o1, o2, t) -> Option.map2 (fun _ _ -> t) o1 o2)
            |> Array.tryHead
            |> function 
            | Some t -> t 
            | None -> TypeError "" // cast exception

        Binary(op, left, right, typed info t)

    | Unary(op, e, info) ->
        let e = CheckExpr stack ttab vtab ftab e
        let et = (getinfo e).ty
        let t = 
            LookUp ftab (string op) 
            |> Array.map (fun t -> Unify t et |> Option.map (fun _ -> t))
            |> Array.choose id // take all unifiable types
            |> Array.tryHead   // pick the last defined type
            |> function Some t -> t | None -> TypeError "" // if no type trow exception
            // need to insert fitting error message

        Unary(op, e, typed info t)


    | Expr.Array(entries, info) ->        
        let entries = Array.map (fun entry -> CheckExpr stack ttab vtab ftab entry) entries
        if Array.isEmpty entries then
            TypeError ""
        else
            let t = (getinfo entries.[0]).ty
            Array.iter 
                (fun e -> 
                    // check that entry unify with the type t i.e. has the same type
                    let t' = (getinfo e).ty
                    unify t' t ""
                ) entries
            // an array should only contain a single type parameter since all arrays of the same type
            // are can differ only in size and not type
            let t = App Array [t]
            Expr.Array(entries, typed info t)

    | Expr.Tuple(fields, info) ->
        // set type of all fields of the tuple
        let fields = Array.map (fun field -> CheckExpr stack ttab vtab ftab field) fields
        // get all types in order
        let ts = Array.map (fun field -> (getinfo field).ty) fields
        // return tuple as checked tuple and type field set
        Expr.Tuple(fields, typed info (App Tuple (Array.ofSeq ts)))

    | Zip(collections, info) ->
        let collections = Array.map (fun collection -> CheckExpr stack ttab vtab ftab collection) collections
        let t = App Tuple []
        // this is only to generalize a sometime convenient transformation
        // it make the user able to help the compiler optimize memory copying 
        // zipping n tuples together into one, this is reversible as long as we knows the size parameters
        if Array.forall (function Expr.Tuple _ -> true | _ -> false) collections then
            let fields = 
                Array.fold 
                    (fun fields col ->
                        // invariance of the if conditions
                        let (App(Tuple, fields')) = (getinfo col).ty
                        seq[yield! fields; yield! fields']
                    ) Seq.empty collections
            // returning a tuple with fields of all tuples patch together inorder of the tuples given
            // zip: r0t0 * .. * r0tn -> .. -> rmt0 * .. rmtk -> r0t0 * .. * r0tn * .. * rmt0 * .. * rmtk
            Zip(collections, typed info (App Tuple (Array.ofSeq fields)))

        elif Array.forall (function Expr.Array _ -> true | _ -> false) collections then
            let ty = 
                Array.fold 
                    (fun ty' col ->
                        // invariance of the if conditions
                        let (App(Array, ty)) = (getinfo col).ty
                        seq[yield! ty'; yield! ty]
                    ) Seq.empty collections
                |> Array.ofSeq
                |> App Tuple
            // Zip: t0[] -> ... -> tn[] -> (t0 * ... * tn)[]
            Zip(collections, typed info (App Array [ty]))
        else
            TypeError ""

    // need to be checked
    | Map(fn, collection, info) ->
        let fn = CheckExpr stack ttab vtab ftab fn 
        let collection = CheckExpr stack ttab vtab ftab collection
        match expand (getinfo fn).ty with
        | Poly([beta], App(Arrow, [t1; t2])) ->
            match (getinfo collection).ty with
            | App(Array, ty) ->
                unify ty.[0] t1 ""// the argument of the function should be the same as the type in the arrays
                Map(fn, collection, typed info t2)

            | _ -> TypeError ""

        | _ -> TypeError ""
            
*)