module Regex

#nowarn "25"
(*

    This is an none pure ASCII lexer the transformation from char to byte is strictly to make sure that 
    we minimize memory compsumption.

    [â ... ] will make a complement of the interval given by ... with respect to the ASCII character set where
    [^ ... ] will make a complement of the interval given by ... of the full span of byte.
    ??


    OBS: it is possible to make a lexer from it that takes pattern of other encoding.
    but for the moment it is to be hand made, by encoding all possible patterns of UTF-8 or so as regex.


    TODO: Interval and complements
*)




// 
let ASCII    = set[0uy .. 127uy]
let All = set[0uy .. 254uy]
type regex =
    | Epsilon
    | Terminal of int
    | Atom of byte * int
    | Cat of regex * regex * int Set * int Set * bool
    | Or of regex * regex * int Set * int Set * bool 
    | Star of regex * int Set * int Set

// Construction functions

let IsNullAble reg =
    match reg with
    | Epsilon | Star _ -> true
    | Atom _ | Terminal _ -> false
    | Cat (_, _, _, _, n) | Or (_, _, _, _, n) -> n


let FirstPosOf reg =
    match reg with
    | Epsilon -> set[]
    | Atom (_, i) | Terminal i -> set[i]
    | Cat (_, _, fp, _, _) | Or (_, _, fp, _, _) | Star (_, fp, _) -> fp


let LastPosOf reg =
    match reg with
    | Epsilon -> set[]
    | Atom (_, i) | Terminal i -> set[i]
    | Cat (_, _, _, lp, _) | Or (_ , _, _, lp, _) | Star (_, _, lp) -> lp

let rec GetTerminals regex =
    match regex with
    | regex.Terminal i -> Set[i]
    | regex.Atom _ |Epsilon -> set[]
    | regex.Star(regex, _, _) -> GetTerminals regex
    | regex.Or(regex1, regex2, _ ,_, _) | regex.Cat(regex1, regex2, _ ,_, _) ->
        GetTerminals regex1 + GetTerminals regex2


let rec Followpos regex fp =
    match regex with
    | Cat(c1, c2, _, _, _) -> 
        Followpos c1 fp // compute followpos for the left side subtree
        |> Followpos c2 // compute followpos for the right side subtree
        |> Set.foldBack ( // compute followpos for this node
            fun i fp ->
                match Map.tryFind i fp with
                | None -> Map.add i (FirstPosOf c2) fp
                | Some f -> Map.add i (f + FirstPosOf c2) fp
        ) (LastPosOf c1)

    | Star(n, f, l) ->
        Followpos n fp // compute followpos for the subtree
        |> Set.foldBack ( // compute followpos for this node
            fun i fp ->
                match Map.tryFind i fp with
                | None -> 
                   Map.add i f fp
                | Some f' -> Map.add i (f' + f) fp
        ) l
    | Or(c1, c2, _, _, _) ->
        Followpos c1 fp // compute followpos for the left side subtree
        |> Followpos c2 // compute followpos for the right side subtree

    | _ -> fp // leaf cases 





// easy wrappers to hide the ugly stuff
let Cat reg1 reg2 =
    let fp = 
        if reg1 |> IsNullAble 
        then FirstPosOf reg1 + FirstPosOf reg2
        else FirstPosOf reg1
    
    let lp =
        if reg2 |> IsNullAble 
        then LastPosOf reg1 + LastPosOf reg2
        else LastPosOf reg2

    let n = IsNullAble reg1 && IsNullAble reg2

    Cat(reg1, reg2, fp, lp, n)

let Or reg1 reg2 =
    let fp = FirstPosOf reg1 + FirstPosOf reg2
    let lp = LastPosOf reg1 + LastPosOf reg2
    let n = IsNullAble reg1 || IsNullAble reg2

    Or(reg1, reg2, fp, lp, n)



let Star reg = Star(reg, FirstPosOf reg, LastPosOf reg) 

// do not need to rename the atoms ID
let Plus reg = Cat reg <| Star reg





(* ---------------------- Regex Lexer ---------------------------- 

    Two face parsing of regex 1. tokizing 2. construction
    This is done to make it possible to construct a regex parser as a 
    function represenation of that regex
    a DFA representation of that regex
    or NFA representation of that regex

*)
open Mapping
open Iter

// minimal token type
type RegexToken =
    | Atom of byte
    | Qustionmark
    | OR 
    | CAT
    | STAR
    | PLUS

let IsA col a = 
    match Map.tryFind a col with
    | None -> false
    | _ -> true
    
let IsNotA col a = IsA col a |> not

let keyword = 
    set['\\'; '|'; '*'; '+'; '?'; '-'; '['; ']'; '('; ')'; '^'; 'â']
    |> Set.map (fun b -> byte b, ())
    |> Map.ofSeq

let RegexError msg =
    sprintf "Regex Error:\n\t%s" msg
    |> Error



// Lexing after some predicate function to be true
let inline Expect pred  =
    fun input ->
        match Seq.tryHead input with
        | Some (Ok b) when pred b -> 
            Ok(b, Seq.tail input)
        | Some (Ok b)  ->
            sprintf "Error in parsing " 
            |> RegexError
        | Some (Error msg) -> Error msg
        | _ -> Error "EOF"
    |> Map



// Lexing after a legal atom
let Atom =
    fun input ->
        match Seq.tryHead input with
        | Some (Ok b) when b |> IsNotA keyword  -> 
            Ok(RegexToken.Atom b, Seq.tail input)
        | Some (Ok b) ->
            sprintf "Expected an atom but got %c" <| char b
            |> RegexError
        | Some (Error msg) -> Error msg
        | _ -> Error "EOF"
    |> Map
    |> (>>) (fun reg -> [reg])
 



// TODO need to add more general escape keys
// Lexing for escape atoms
let Escape =
    let pattern =
        Expect (fun b -> b = byte '\\') <&> Expect (fun _ -> true)
        |>  (>>) (fun (_, b) -> b)
    fun input ->
        match Run pattern input with
        | Error msg -> Error msg
        | Ok (116uy, iter) ->
            Ok(RegexToken.Atom 9uy, iter)
        | Ok (110uy, iter) ->
            Ok(RegexToken.Atom 10uy, iter)
        | Ok (114uy, iter) ->
            Ok(RegexToken.Atom 13uy, iter)
        | Ok (b, iter) ->
            Ok(RegexToken.Atom b, iter)
    |> Map
    |> (>>) (fun reg -> [reg])


let Interval =
    let rec inner input =
        ((Atom <|> Escape) <&> Expect (fun b -> b = byte '-') <&> (Atom <|> Escape)) 
        |> (>>) (fun (([RegexToken.Atom a], _), [RegexToken.Atom b]) -> 
                if a < b 
                then set[a..b]
                else set[b..a]                
            )
        |> fun pattern -> pattern <|> ((fun [Atom a] -> set[a]) >> Atom)
        |> fun pattern -> (fun (s1, s2) -> s1 + s2) >> (pattern <&> (Map inner)) <|> pattern // elemination doublicates
        |> fun pattern -> Run pattern input
    
    let oring lst =
        match lst with
        | [] -> []
        | [x] -> [x]
        | x :: xs -> List.fold (fun acc a -> OR :: a :: acc) [x] xs

    Expect (fun b -> b = byte '[') <&> Map inner <&> Expect (fun b -> b = byte ']')
    |> (>>) (fun ((_, regex), _) -> 
        regex
        |> Seq.map (fun a -> RegexToken.Atom a)
        |> Seq.toList
        |> oring
    )


let Complement =
    let rec inner input =
        ((Atom <|> Escape) <&> Expect (fun b -> b = byte '-') <&> (Atom <|> Escape)) 
        |> (>>) (fun (([RegexToken.Atom a], _), [RegexToken.Atom b]) -> 
                if a < b 
                then set[a..b]
                else set[b..a]                
            )
        |> fun pattern -> pattern <|> ((fun [Atom a] -> set[a]) >> Atom)
        |> fun pattern -> (fun (s1, s2) -> s1 + s2) >> (pattern <&> (Map inner)) <|> pattern // elemination doublicates
        |> fun pattern -> Run pattern input
    
    let oring lst =
        match lst with
        | [] -> []
        | [x] -> [x]
        | x :: xs -> List.fold (fun acc a -> OR :: a :: acc) [x] xs

    Expect (fun b -> b = byte '[') <&> Expect (fun b -> b = byte '^') <&> Map inner <&> Expect (fun b -> b = byte ']')
    |> (>>) (fun ((_,regex),_) -> 
        ASCII - regex // taking the compliement
        |> Seq.map (fun a -> RegexToken.Atom a)
        |> Seq.toList
        |> oring
    )

// Parsin paranteses
let Paranteses pattern =
    Expect (fun b -> b = byte '(') <&> pattern <&> Expect (fun b -> b = byte ')')
    |> (>>) (fun ((_, regex), _) -> regex)


let cat pattern =
    let rec c input =
        (fun (reg1, reg2) -> CAT :: reg1 @ reg2) >> (pattern <&> Map c)
        |> fun pattern' -> pattern' <|> pattern
        |> fun pattern' -> Run pattern' input
    Map c


let orr pattern =
    let rec c input =
        (fun ((reg1, _), reg2) -> OR :: reg1 @ reg2) >> (pattern <&> (Expect (fun b -> b = byte '|')) <&> Map c)
        |> fun pattern' -> pattern' <|> pattern
        |> fun pattern' -> Run pattern' input
    Map c

// doesn't work
let Maybe pattern =
    pattern <&> Expect (fun b -> b = byte '?')
    |> (>>) (fun (reg, _) -> Qustionmark :: reg) 

// Parsing star regex over some pattern and prefixing the or for simpler parsing to syntaks tree
let star pattern =
    pattern <&> Expect (fun b -> b = byte '*')
    |> (>>) (fun (reg, _) -> STAR :: reg)


// Parsing plus regex over some pattern and prefixing the or for simpler parsing to syntaks tree
let plus pattern =
    pattern <&> Expect (fun b -> b = byte '+')
    |> (>>) (fun (reg, _) -> PLUS :: reg)

let Primitives pattern = Atom <|> Paranteses pattern <|> Escape <|> Interval <|> Complement


let starPlusPrim pattern =
    // to minimize copying 
    let p = Primitives pattern
    Maybe p <|> star p <|> plus p <|> p

let Tokenizer = 
    let rec regex input =
        // The star/plus has higher precedence than orr and orr has higher precendence than cat hence the order.
        cat (orr (starPlusPrim (Map regex)))
        |> fun pattern -> Run pattern input
    Map regex


// --------------------------------------- Regex Parser --------------------------------- //

let Parser count =
    // internal mutability is okay
    let mutable count = count
    let atom = 
        fun input ->
            match Seq.tryHead input with
            | Some (Ok (Atom a)) ->
                let a' = regex.Atom(a, count)
                count <- count + 1
                Ok(a', Seq.tail input)
            | Some a -> Error <| $"Parser Error: {a} is not an atom"
            | _ -> Error <| "Parser Error: not an atom"
        |> Map

    let star pattern =
        Expect (fun token -> token = STAR) <&> pattern
        |> (>>) (fun (_, regex) -> Star regex)
        

    let plus pattern =
        Expect (fun token -> token = PLUS) <&> pattern
        |> (>>) (fun (_, regex) -> Plus regex)
        
    let questionmark pattern =
        Expect (fun token -> token = Qustionmark) <&> pattern
        |> (>>) (fun (_, regex) -> Or regex Epsilon)

    let Cat pattern =
        Expect (fun token -> token = CAT) <&> pattern <&> pattern
        |> (>>) (fun ((_, regex1), regex2) -> Cat regex1 regex2)
        

    let Orr pattern =
        Expect (fun token -> token = OR) <&> pattern <&> pattern
        |> (>>) (fun ((_, regex1), regex2) -> Or regex1 regex2)
       
        

    let Regex =
        let rec regex input =
            let reg = Map regex
            Orr reg <|> Cat reg <|> questionmark reg <|> star reg <|> plus reg <|> atom
            |> fun pattern -> Run pattern input
        Map regex

    fun input ->
        match Run Regex input with
        | Ok (regex, iter) -> Ok((regex, count), iter)
        | Error msg -> Error msg
    |> Map

    
