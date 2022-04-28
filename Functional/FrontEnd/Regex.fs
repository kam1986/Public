module Regex

open Lexer
open Regex
open Productions
open Token

type RegexToken =
       | ATOM
       | QUESTIONMARK
       | OR 
       | STAR
       | MINUS
       | PLUS
       | LPAR
       | RPAR
       | LSQRT
       | RSQRT
       | HAT 
       | APPOSTROHPE
       | SPACE
       | EOF

module Lexing =

   

    let atom = "[\x00-\xFF]"

    let regex =
        [|
            $"'{atom}'"     != (fun (str: string) -> byte str.[1])  --> ATOM
            "\?"            := QUESTIONMARK
            "\|"            := OR
            "\*"            := STAR
            "\+"            := PLUS
            "\^"            := HAT
            "\-"            := MINUS
            "'"             := APPOSTROHPE
            "\("            := LPAR
            "\)"            := RPAR
            "\["            := LSQRT
            "\]"            := RSQRT
            " +"            := SPACE
            ""              := EOF
        |]


    let FromFile (path: string) =
        let buf = new Buffer.LexBuffer(path)
        LexFile regex buf

    let FromString str = LexString regex str


module Parsing =
    open Parser

    let mutable count = 0
    let mutable term = 0

    let Run = Parser.Run
    
    type cases =
        | Reg
        | PostFix
        | Interval
        | Atoms
        | Primitives

    let regex =
        Productions [
            Reg => [
                
                [%PostFix; !OR; %PostFix]
                >> fun args -> Or (ValueOf args.[0]) (ValueOf args.[2])

                [%PostFix; %PostFix]
                >> fun args -> Cat (ValueOf args.[0]) (ValueOf args.[1])
                
                [%PostFix]
                >> fun args -> ValueOf args.[0]
            ]
            
            PostFix => [
                [%PostFix; !STAR]
                >> fun args -> Star (ValueOf args.[0])

                [%PostFix; !PLUS]
                >> fun args -> Plus (ValueOf args.[0])

                [%PostFix; !QUESTIONMARK]
                >> fun args -> Maybe (ValueOf args.[0])

                [%Primitives]
                >> fun args -> ValueOf args.[0]

            ]

            Primitives => [
                [%Interval]
                >> fun args ->
                    ValueOf args.[0]
                    |> Set.map 
                        (fun a -> 
                            count <- count + 1
                            regex.Atom(a, count)    
                        )
                    |> Seq.reduce (fun a1 a2 -> Or a1 a2)

                [!APPOSTROHPE; !ATOM; !APPOSTROHPE]
                >> fun args -> 
                    count <- count + 1
                    regex.Atom(ValueOf args.[1], count)

                [!LPAR; %Reg; !RPAR]
                >> fun args -> ValueOf args.[0]
            ]

            Interval => [
                [%Interval; !MINUS; %Interval]
                >> fun args -> ValueOf args.[0] - ValueOf args.[2]
                    
                [!LSQRT; %Atoms; !RSQRT]
                >> fun args -> ValueOf args.[1]

                [!LSQRT; !HAT; %Atoms; !RSQRT]
                >> fun args -> All - ValueOf args.[2]
            ]

            Atoms => [
                [%Atoms; %Atoms]
                >> fun args -> ValueOf args.[0] + ValueOf args.[1]

                [!APPOSTROHPE; !ATOM; !APPOSTROHPE; !MINUS; !APPOSTROHPE; !ATOM; !APPOSTROHPE]
                >> fun args -> set[ValueOf args.[1] .. ValueOf args.[5]]

                [!APPOSTROHPE; !ATOM; !APPOSTROHPE]
                >> fun args -> set[ValueOf args.[1]]
            ]
        ]
        |> SLR


let ParseString (str: string) : regex =
    Lexing.FromString str
    |> Parsing.Run Parsing.regex
    |> fun reg -> 
        Parsing.term <- Parsing.term - 1
        Cat reg (regex.Terminal Parsing.term)