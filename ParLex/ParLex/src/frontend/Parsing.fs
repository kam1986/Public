module Parsing

open Token
open Parser
open Regex
open Lexing
open Productions

let mutable private count = 0
let mutable private term = 0

let c() =
    let c = count
    count <- count + 1
    c


let t() =
    term <- term - 1
    term
    

let all = set[0uy .. 255uy]
let ascii = set[0uy .. 127uy]

type Prod =
    | P_regex
    | P_atom
    | P_value
    | P_cat
    | P_or
    | P_interval
    | P_inner
    | P_postfix

type token = Lexing.token


let syntax =
    Productions [
        P_regex => [
            [%P_or; !EOF]
            >> fun args -> Regex.Cat (ValueOf args.[0]) (regex.Terminal (t()))
        ]
        
        P_cat => [
            [%P_or; %P_cat]
            >> fun args -> Regex.Cat (ValueOf args.[0]) (ValueOf args.[1]) 


            [%P_or]
            >> fun args -> ValueOf args.[0] 
        ]

        P_or => [
            [%P_postfix; !Or; %P_or]
            >> fun args -> Regex.Or (ValueOf args.[0]) (ValueOf args.[2]) 


            [%P_postfix]
            >> fun args -> ValueOf args.[0] 
        ]

        P_postfix => [
            [%P_interval; !Star]
            >> fun args -> Regex.Star (ValueOf args.[0]) 
            
            [%P_interval; !Plus]
            >> fun args -> Regex.Cat (ValueOf args.[0]) (Regex.Star (ValueOf args.[0]))
            
            [%P_interval; !Question]
            >> fun args -> 
                Regex.Or (ValueOf args.[0]) Epsilon 
            

            [%P_value]
            >> fun args -> ValueOf args.[0] 
        
        ]

        // enable interval intersection
        P_value => [
            [%P_interval]
            >> fun args -> 
                let atoms = ValueOf args.[0]
                if Set.isEmpty atoms then
                   Epsilon
                else
                    Set.map (fun a -> Regex.regex.Atom(a, (c()))) atoms
                    |> Seq.reduce (Regex.Or)

            [%P_atom]
            >> fun args -> ValueOf args.[0]
        ]


        P_interval => [
            // set difference
            [!Lsqrt; %P_inner; !Rsqrt; !Bar; %P_interval]
            >> fun args ->
                let first = ValueOf args.[1]  : byte Set
                let second = ValueOf args.[4] : byte Set
                Set.difference (first : byte Set) (second: byte Set) 



            // singleton
            [!Lsqrt; %P_inner; !Rsqrt]
            >> fun args -> ValueOf args.[1] : byte Set
                
            // complement of (0, 255)
            [!Lsqrt; !Hat; %P_inner; !Rsqrt]
            >> fun args -> all - ValueOf args.[2] : byte Set
              
            // complement of (0, 127)
            [!Lsqrt; !Numb; %P_inner; !Rsqrt]
            >> fun args -> ascii - ValueOf args.[2] : byte Set
        ]


        P_inner => [
            [!Atom; !Bar; !Atom; %P_inner]
            >> fun args -> 
                let low = ValueOf args.[0]  : byte
                let high = ValueOf args.[2] : byte
                set[low .. high] + ValueOf args.[3]

            [!Atom; %P_inner]
            >> fun args -> 
                let atom = ValueOf args.[0] : byte
                set[atom] + ValueOf args.[1]

            []
            >> fun _ -> set[] : byte Set
        ]


        P_atom => [
            [!Atom]
            >> fun args -> Regex.regex.Atom(ValueOf args.[0], c())

            [!Lpar; %P_or; !Rpar]
            >> fun args -> ValueOf args.[1]
        ]
        
    
    ]
    |> SLR