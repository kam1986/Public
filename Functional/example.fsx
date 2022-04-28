(*
    
    Hej Oliver har lavet et lille eksempel på functionel kode i F#

    Bruger du VS code EDI så burde den selv finde intellisens og det

    regler for syntax

    'let' 'mutable'? 'id' 'args'*? = body 
            starter en deklarering af enten en function, værdi eller variable
            mutable gør en værdi til en variable og kan referece med gennem byref, inref, outref
            args er en mulig tom sekvens af argument ids

    'rec' er nødvendigt for at kalde en function recursivt, det giver compileren ekstra information
          som gør optimering mere strømlinet.

    'and' bliver brugt til at deklarere mutual recursive functioner og type

    functioner:
        let rec fid1 args = 
            body

        and fid2 args =
            body
          
    type tid1 = declaration

    and tid2 = declaration

    typer er så godt som altid infered af compileren med undtagelse af 
    subtyping, interfaces, type renaming og overlap af typers navngivning

    nesting sker med indentation 4 whitespace/1 tab
    sidste linje i en nesting er ALTID retur typen
    Hvis sidste linje returnere unit (void) er der ingen return værdi
    
    light weight bruger man 'in' i stedet for nesting

    OBS: 
        intet i koden nedenfor er mutable så alle ændringer til værdier er 'copy by value'
        også kaldet overshadowing af variable og en værdi alle ændringer sket i en nesting bliver
        ikke videreført efter nestingen er slut
*)



// Eksempel er en lommeregner med symbolsk differentation med type check
// Type deklarering

// The her er en 'tagged type' også kaldet decriminent type.
// type name<generic variable names> = declaration
type Op<'i32,'f64> =
    | I32 of 'i32
    | F64 of 'f64


type Position =
    {
        line: int
        col:  int
        abs:  int
    }
with
    // overriding the default string converter
    override pos.ToString() = $"({pos.line}, {pos.col})" 

let startpos = { line = 0; col = 0; abs = 0}
 
let Move pos = 
    { pos with 
        col = pos.col + 1 // newline og ; gælder begge som seperator token
        abs = pos.abs + 1     
    }

// overheat by calculating col = col + 1 will be removed by redundency by the compiler
let NewLine pos = { Move pos with col = 0 }

// type renaming  
type Value = Op<int, float>

// type renaming
type Type = Op<unit, unit>

// deklarere en funktion 'TypeOf' med argument op 
// infered til typen Op<'a, 'b, 'c> af compileren fordi vi tester den med match

let rec TypeOf op : Type = 
    match op with
    | F64 _ -> F64()

let printType op =
    match op with
    | I32 _ -> "i32"
    | F64 _ -> "f64"


type ErrorType =
    | Lexer
    | Parser
    | Type
    | Evaluation
    | Lookup

let Error errortype msg pos =
    Result.Error $"{errortype} Error:\n  {msg} at {pos}"

let TypeError msg pos = Error Type msg pos
    

// ny infix operator
let inline (<?>) op1 op2 =
        // TypeOf used to allow different types of op1 and op2, it will be optimized away
        match TypeOf op1, TypeOf op2 with
        | I32 _, (I32 _ as ty) | F64 _, (F64 _ as ty) -> Ok ty 
        | t2, t2 -> 


// expression tree
type Expr =
    | Value     of Value * Position  // tag og type overordnet type kan godt dele navn
    | ID        of string * Position 
    | Convert   of Op<Expr, Expr> * Position
    | Add       of Expr * Expr * Position
    | Sub       of Expr * Expr * Position
    | Mul       of Expr * Expr * Position
    | Div       of Expr * Expr * Position



// adding static type to core lib type
type Result<'succes, 'error> with 
    static member mapWithError f ret = 
        match ret with
        | Error msg -> Error msg
        | Ok ret -> f ret

    


// list er mest effektivt her for små tables, alternativt ville jeg bruge Map typen som er et immutable hashtable
type Table<'id,'item> = Table of ('id * 'item) list

let Bind (Table tab) name item =
    (name, item) :: tab // :: infix operator for List.cons og bliver også brugt til dekonstruering under match
    |> Table


// '_' er et wildcart som fortæller compileren at værdien kan have alle værdier
// og det siger også vi skal ikke bruge værdien til noget.
let rec LookUp (Table tab) name pos =
    match tab with
    // empty list 
    | [] -> Error Lookup $"The id: {name} are not defined" pos
  
    // dekonstruering af en liste indeholdene en tuple
    // og binder navne til enkelte værdier efter struktur
    | (id, item) :: _ when id = name -> Ok item
    
    | _ :: tab -> LookUp (Table tab) name pos



let rec CheckExpr vtab ftab expr =
    match expr with
    | Value (v, _) -> TypeOf v |> Ok
    | ID (name, pos) -> LookUp vtab name pos
    | Convert (arg, _) ->  TypeOf arg |> Ok
            
    | Add(left, right, pos) -> (left <?> right) |> Ok


    | Sub(left, right, pos) ->
    | Mul(left, right, pos) ->
    | Div(left, right, pos) ->
