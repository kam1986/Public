module lexer

open Lexer
open Token
open Syntax


// symbols
let digit = '0' .-. '9'
let hexdigit = 'a' .-. 'f' <|> 'A' .-. 'F' <|> digit
let octdigit = '0'.-.'8'
let bindigit = !"1" <|> !"0"
let letter = 'a' .-. 'z' <|> 'A' .-. 'Z'

// operators
let plus'   = !"+"
let minus   = !"-"
let star'   = !"*"
let dash    = !"/"
let dot     = !"."  // dot product for arrays and tuples with same size and all fields of the same type 
let rr      = !"o>"
let lr      = !"<o"
let rshift  = !">>"
let lshift  = !"<<"


let eq      = !"="
let ne      = !"<>" 
let le      = !"<="
let ge      = !">="
let lt      = !"<"
let gt      = !">"

let and'    = !"&"
let or'     = !"|"
let xor     = !"^"
let imply   = !"->"

let land'    = !"&&"
let lor'     = !"||"
let lxor'    = !"^^"
let limply   = !"=>"


// seperators
let semi    = !";"
let colon   = !":"
let coma    = !","


let noise = choose " \t\r\n" 

// number values
let hexnum = !"0x" => plus hexdigit
let octnum = !"0o" => plus octdigit
let binum  = !"0b" => plus bindigit


let num = plus digit <|> hexnum <|> octnum <|> binum
let flt = plus digit => !"." => star digit
let id' = letter => star (letter <|> digit)

// brackets
let lp = !"("
let rp = !")"
let ls = !"["
let rs = !"]"
let lb = !"{"
let rb = !"}"


// types
let bool'   = !"bool"
let i8      = !"i8"
let i16     = !"i16"
let i32     = !"i32"
let i64     = !"i64"
let u8      = !"u8"
let u16     = !"u16"
let u32     = !"u32"
let u64     = !"u64"
let f32     = !"f32"
let f64     = !"f64"
let result  = !"result"
let option  = !"option"

let boolean  = !"true" <|> !"false"
let int8     = num => !"y"
let int16    = num => !"s"
let int32    = num => !"w"
let int64'   = num => maybe !"l"
let uint8    = num => !"uy"
let uint16   = num => !"us"
let uint32   = num => !"uw"
let uint64'  = num => !"ul"
let float32  = flt => !"f"
let float64  = flt => maybe !"d"

let integer  = int64 << String.filter (fun c -> ('0' <= c && c <= '9') || ('a' <= c && c <= 'f') || ('A' <= c && c <= 'F') || c = 'x' || c = 'o' || c= 'b' || c = 'e' || c = 'E')
let uinteger = uint64 << integer
let floating = float << String.filter (fun c -> ('0' <= c && c <= '9') || c = '.' || c = 'e' || c = 'E' || c = 'p' || c = 'P' || c = 'x')
let bool str = if str = "true" then true else false 
// unop
let not     = !"not"
let popcnt  = !"popcnt"
let clz     = !"clz"
let ctz     = !"ctz"
let sqrt    = !"sqrt"
let rnd     = !"rnd"
let nst     = !"nst "
let floor   = !"floor "
let ceil    = !"ceil "


// keywords
let let'   = !"let"
let in'    = !"in"
let end'   = !"end"
let func   = !"fun"
let type'  = !"type"
let map    = !"map"
let filter = !"filter"
let fold   = !"fold"
let scan   = !"scan"
let from   = !"from"
let until  = !"until"

type token =
    // arithmic ops
    | PLUS  
    | MINUS 
    | STAR  
    | DASH  
    | AND
    | OR
    | XOR
    | IMPLY
    
    // relational ops
    | LE
    | LT
    | GE
    | GT
    | EQ
    | NE

    // logic ops
    | LAND
    | LOR
    | LXOR
    | LIMPLY

    // seperators
    | COMMA
    | SEMI  

    // enclosing
    | LP    
    | RP    
    | LS
    | RS
    | LB
    | RB

    // values
    | ID       
    | VALUE
    // conditional output types
    | SOME
    | NONE
    | RETURN
    | ERROR
    
    | LET
    | FUN
    | MAP
    | UNTIL  // these are slicing by predicate
    | FROM   // these are slicing by predicate
    | FILTER
    | SCAN
    | FOLD
    | IN
    | END

    // types
    | OPTION
    | RESULT
    | TYPE
    | TYPEDEC
    
    | UNOP
    
    // filters and marker
    | EOF   
    | NOISE 

exception LexerError of Message : string

let LexerError msg = 
    LexerError msg
    |> raise

let patterns =
    lexer [|
        plus'            := PLUS
        minus            := MINUS
        star'            := STAR
        dash             := DASH
        coma             := COMMA
        lp               := LP
        rp               := RP
        lb               := LB
        rb               := RB
        ls               := LS
        rs               := RS

        // relational
        eq               := EQ
        ne               := NE
        le               := LE
        lt               := LT
        ge               := GE
        gt               := GT


        let'             := LET
        semi             := SEMI
        not              != (fun _ -> Not)   --> UNOP
        popcnt           != (fun _ -> Pc)    --> UNOP
        clz              != (fun _ -> Clz)   --> UNOP
        ctz              != (fun _ -> Ctz)   --> UNOP
        sqrt             != (fun _ -> Sqrt)  --> UNOP
        floor            != (fun _ -> Floor) --> UNOP
        ceil             != (fun _ -> Ceil)  --> UNOP
        
        // type
        i8               != (fun _ -> op.I8)   --> TYPE
        i16              != (fun _ -> op.I16)  --> TYPE
        i32              != (fun _ -> op.I32)  --> TYPE
        i64              != (fun _ -> op.I64)  --> TYPE
        u8               != (fun _ -> op.U8)   --> TYPE
        u16              != (fun _ -> op.U16)  --> TYPE
        u32              != (fun _ -> op.U32)  --> TYPE
        u64              != (fun _ -> op.U64)  --> TYPE
        f32              != (fun _ -> op.F32)  --> TYPE
        f64              != (fun _ -> op.F64)  --> TYPE
        bool'            != (fun _ -> op.Bool) --> TYPE
        option           := OPTION
        result           := RESULT

        // primitive values the functions bool, integer and floating are defined above, they assume correct formatted type instance and filter out type extensions
        boolean          != ((Bool   << bool): string -> Value)     --> VALUE
        int8             != ((op.I8  << integer): string -> Value)  --> VALUE
        int16            != ((op.I16 << integer): string -> Value)  --> VALUE
        int32            != ((op.I32 << integer): string -> Value)  --> VALUE
        int64'           != ((op.I64 << integer): string -> Value)  --> VALUE
        uint8            != ((op.U8  << uinteger): string -> Value) --> VALUE
        uint16           != ((op.U16 << uinteger): string -> Value) --> VALUE
        uint32           != ((op.U32 << uinteger): string -> Value) --> VALUE
        uint64'          != ((op.U64 << uinteger): string -> Value) --> VALUE
        float32          != ((op.F32 << floating): string -> Value) --> VALUE
        float64          != ((op.F64 << floating): string -> Value) --> VALUE

        // keyword
        let'             := LET
        type'            := TYPE
        in'              := IN
        end'             := END
        func             := FUN
        map              := MAP
        filter           := FILTER
        scan             := SCAN
        fold             := FOLD
        until            := UNTIL
        from             := FROM

        noise            := NOISE
        id'              != id --> ID
        !""              := EOF
    |]


let LexCrap str = 
    LexString patterns str
    |> Seq.filter (fun token -> TypeOf token <> NOISE)