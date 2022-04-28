module LexerGenerator
(*
open Return
open Mapping
open Iter
open Regex
open DFA

// TODO need to make codeformat for predefined functions


let MakeStateFunc name transistion actions =
    name + " lexeme context =\n" 
    + "    match Seq.head context as b with\n"
    + (List.fold2 (fun patterns trans action -> patterns + "    | " + trans + " -> " + action + "\n") "" transistion actions)


let BuildLexer (str : string) =
    let mutable count = 0
    let mutable term = 0
    let patterns, actions =
        str.Split('?') // split up to regex token pair
        |> Array.map (fun pair -> pair.Split ([|"->"|],System.StringSplitOptions.None))
        |> Array.filter (fun pair -> pair.Length = 2)
        |> Array.map (fun pair -> pair.[0].Trim(), pair.[1].Trim()) // remove trailing whitespace
        |> Array.unzip


    let regex =
        patterns
        |> Array.map 
            ( fun pattern -> 
                Run Tokenizer (FromString(pattern))
                |> debugReturn
                |> fun (tokens, _) -> lst(tokens)
                |> Run (Parser count)
                |> debugReturn
                |> fun ((regex, count'),_) ->
                        count <- count'
                        term <- term - 1
                        Cat regex (Terminal term)
            )
            |> Array.reduce Or

    let (_, states, transitions) = StateFinder regex
    let states' = List.zip states [0..states.Length-1]

    printf "states: %d\n" states.Length

    let states'' =
        let statemap =
            Map.ofList states'

        states'
        // for each state with number numb
        |> List.map (fun (state, numb) ->
            // find valid transitions and states to jump to
            let transactions =
                Map.filter (fun (s, _) _ -> state = s) transitions
                |> Map.fold (fun acc (s,t) d -> (t,Map.find d statemap) :: acc) []
             
            // check if there is an acceptance
            // Some if true
            // None if false
            let terminal = 
                Set.filter (fun n -> n < 0) state
                |> fun s ->
                    match s.IsEmpty with
                    | true -> None
                    | _ ->
                        s
                        |> Set.maxElement
                        |> (~-) // negation
                        |> fun n -> Some (actions.[n-1])
            // return a triple of state number transitions action pair and terminal handle
            (numb, transactions, terminal)
        )

    states''
    |> List.map (fun (statenumb, pairs, terminal) -> 
        let patterns, actions =
            List.map (fun (pattern, n) -> (string pattern + "uy", "state" + string n + " (lexeme + char b) (Seq.tail context)")) pairs
            |> List.unzip
        // convert each state into a eqivalent function
        MakeStateFunc ("states" + string statenumb) patterns actions
        |> fun statefunc ->
           // add acceptance handle
           match terminal with
           | None -> 
                statefunc +
                "    | _ -> Error \"\"\n"
           | Some action ->
                statefunc +
                sprintf "    | _ -> Ok(%s)\n" action
    )
    |> fun funcs ->
        // first function
        "let rec internal " + funcs.Head + "\n\n"
        + (List.fold (fun funcs func -> funcs + "and internal " + func + "\n\n") "" (funcs.Tail))

        

let testpattern =
    "? (a|b)*ac -> success"
    |> BuildLexer
*)