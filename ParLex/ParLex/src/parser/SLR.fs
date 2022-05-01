module SLR

(*

    OBS!
        - should implement both dfa reduction and left recursion transformation
        - separate the dfa table from the goto table (mayby use Map)
*)


open Position
open Token
open Productions
open TypeAcrobatics
open NFA
#nowarn "25"

[<Struct>]
type Action =
    | Shift     of s: int
    | Reduce    of r: int
    | Goto      of g: int
    | Error     
    | Accept




let internal text input =
    match input with 
    | Shift n -> sprintf "s%02d" n
    | Reduce n -> sprintf "r%02d" n
    | Accept -> "  a"
    | Error -> "   "
    | Goto n -> sprintf "g%02d" n

let print row size (table : _[]) =

    for i in 0 .. row .. size-1 do
        let i' = i/row

        printfn "%2d %s" (i/row) (Array.reduce (fun str s -> str + " " + s)  <| Array.map text table.[i .. row+i - 1])




let internal GetLanguage (Productions productions) =
    let rec gl language productions =
        match productions with
        | [] -> language
        | Production(N, ra) :: productions ->
            let rules, _ = List.unzip ra
            let lang = 
                List.fold (fun lang rule -> 
                    List.fold (fun lang symbol ->
                        Set.add symbol lang
                    ) lang rule
                ) language rules
            gl (Set.add (NonTerminal N) lang) productions

    gl Set.empty productions
   


let rec internal findaction state actions =
    match actions with
    | [] -> failwith "actions missing"
    | (s, p, _, _) :: _ when s = state -> p
    | _ :: actions -> findaction state actions

let rec internal findproduction state actions =
    match actions with
    | [] -> failwith "actions missing"
    | (s, _, p, _) :: _ when s = state -> p
    | _ :: actions -> findproduction state actions

let internal makeTable follow actions language goto dfa =
    // finding all terminals
    let terminals = 
        Set.filter (fun symbol -> match symbol with Terminal _ -> true | _ -> false) language
        |> Set.map (fun (Terminal c) -> c)

    let follow' = Map.map (fun _ item ->  Set.map (fun (Terminal c) -> c) item) follow

    let max' = (Set.maxElement terminals).GetHashCode()
    let symbols = max' + 2 // include both end to the interval by adding one
    let size = language.Count 
    let numberofstates = List.length dfa// removing the added state for acceptance
    // assume error at first
    let table = [| for _ in 1 .. size * numberofstates -> Error |]


    let states = 
        dfa
        |> List.fold (fun (map, state) dstate -> 
            Map.add dstate state map, state+1
            ) (Map.empty, 0)
        |> fst

    printfn "Parser Information"
    printfn "The parser has %d states" dfa.Length
    printfn "For a language of %d tokens" terminals.Count
    printfn "and %d productions" (language.Count - terminals.Count)
    printfn ""
    printfn "There is %d transitions" (goto : Map<_,_>).Count
    printfn "i.e. %f procent of the entries are non errors" (float goto.Count/ float (language.Count * dfa.Length)) 
    printfn ""

    // making accepting state set
    let acceptingstates = 
        List.map (fun (x, _, _, _) -> x) actions 
        |> Set.ofList
        |> fun s -> s - set[1]


    table.[size*1] <- Accept
    // for each transition in the GOTO
    
    for (source, symbol), destination in goto |> Map.toSeq do
        let src, dst = Map.find source states, Map.find destination states
        let accept = Set.intersect source acceptingstates
        let offset = size * src
        match symbol with
        | NonTerminal N ->
            let flw = Map.find N follow'
            match accept.Count with
            | 0 ->
                table.[offset + symbols + N] <- Goto dst
            | 1 ->
                // take the one with highest precedence
                let p = findaction accept.MinimumElement actions
                // set goto table entry
                table.[offset + symbols + N] <- Goto dst
                // set reduction
                for c in flw do
                    let entry = table.[offset + c + 1]
                    match entry with
                    | Shift n ->
                        printfn "shift / reduce conclict"
                    | _ -> ()
                    table.[offset + c + 1] <- Reduce p
            | _ ->
                // take the one with highest precedence
                let p = findaction accept.MinimumElement actions
                // set goto table entry
                table.[offset + symbols + N] <- Goto dst
                // set reduction
                for c in flw do
                    let entry = table.[offset + c + 1]
                    match entry with
                    | Shift n ->
                        printfn "shift / reduce conclict"
                    | _ -> ()
                    table.[offset + c + 1] <- Reduce p
        | Terminal c ->
            let offset = size * src + 1 + c // shifting the table entries by one to the right, this allow the terminal to be represented by -1 
            let entry = table.[offset]
            match entry with
            | Reduce n ->
                printfn "shift / reduce conclict"
            | _ -> ()
            
            table.[offset] <- Shift dst

    dfa
    |> List.map (fun state -> Set.intersect state acceptingstates, Map.find state states)
    |> List.filter (fun (accept, _) -> not accept.IsEmpty)
    |> List.iter(fun (accept, src) ->
            let a = accept.MinimumElement
            let p = findaction a actions
            let flw = findproduction a actions |> fun production -> Map.find production follow'
            for c in flw do
                table.[size * src + c + 1] <- Reduce p // index out of bound here?
            )
    // return the table
    table
  


let internal ItemsToPop (Productions productions) actions =
    List.map (fun (Production(_, (rules : (Symbol<int,int> list * (Token<_>[] -> token))list))) -> List.map (fun (rule, _) -> List.length rule) rules) productions
    |> fun lst -> List.foldBack (fun lst l -> lst @ l) lst []
    |> fun items -> List.zip items actions
    |> List.toArray

 
let internal SLR productions =
    let productions =
        productions
        |> ToCommonRepresentation
        |> addEndOfParse

    let lang = GetLanguage productions
    let size = lang.Count
    let symbols = lang |> Set.filter (fun t -> match t with NonTerminal _ -> false | _ -> true) |> Set.count
    let (NFA n as nfa), actions = MakeNFA productions
    let trans, dfa = MakeDFA nfa lang
    let flw = Follow productions
    // make an array of production number and action function
    let actions' = 
        List.map (fun (_, _, p, a) -> (p, a)) actions 
        |> ItemsToPop productions 
        |> Array.map (fun (pops,(production, action)) -> (pops, production, action))
    let mutable table = makeTable flw actions lang trans dfa

    (table, size, symbols, actions')
    
