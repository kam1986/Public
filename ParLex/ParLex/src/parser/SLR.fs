module SLR

#nowarn "25"

open Position
open Token
open Product
open NFA
open HashTable
open TypeAcrobatics
open Stack

type Action =
    | Shift  of int
    | Reduce of int
    | Goto   of int
    | Error
    | Accept


let private GetLanguage (Productions productions) =
    (Set.empty, productions)
    ||> Array.fold (fun lang (Production(N, ra)) -> 
        let rules, _ = Array.unzip ra
        (lang, rules)
        ||> Array.fold (fun lang rule -> Array.fold (fun lang symbol -> Set.add symbol lang) lang rule) 
        |> Set.add (NonTerminal N)
    )

let private findaction state actions =
    Array.tryFind (fun (s, _, _, _) -> s = state) actions
    |> Option.map (fun (_, p, _, _) -> p)
    |> function Some p -> p | None -> failwith $"action {state} not found"



let private findproduction state actions =
    Array.tryFind (fun (s, _, _, _) -> s = state) actions
    |> Option.map (fun (_, _, p, _) -> p)
    |> function Some p -> p | None -> failwith $"action {state} not found"


let internal MakeTable (productions: Productions<'T,'N>) =
    let hashtable = HashTable.empty
    let lang = GetLanguage productions
    let follow = Follow productions
        
    let nfa, actions = MakeNFA productions

    let trans, dfa = MakeDFA (NFA nfa) lang

 
    for trans in Map.toArray trans  do printfn "%A" trans

    //for action in actions do printfn "%A" action


    let states =
        ((Map.empty, 0), dfa)
        ||> List.fold (fun (map, state) dstate ->
            Map.add dstate state map, state + 1
        )
        |> fst


    let eof =
        Set.filter (fun token -> token.ToString().ToLower() = "eof") lang
        |> fun s -> 
            if s.Count = 0 then
                failwith "No EOF token"
            elif s.Count > 1 then
                failwith "To many EOF tokens"
            else
                s.MaximumElement
                |> fun (Terminal c) -> c


    let acceptancesstates =
        Array.map (fun (x, _,_,_) -> x) actions
        |> Set.ofArray
        |> Set.remove 2


    for (source, symbol), destination in trans |> Map.toArray do
        let src, dst = Map.find source states, Map.find destination states

        match symbol with
        | NonTerminal N ->
            let flw = Map.find N follow
            let accept = Set.intersect source acceptancesstates
            match accept.Count with
            | 0 -> hashtable.[(src, symbol)]  <- Goto dst
            | _ -> 
                let p = findaction accept.MinimumElement actions
                hashtable.[(src, symbol)] <- Goto dst

                for c in flw do hashtable.[(src, c)] <- Reduce p


        | Terminal _ -> hashtable.[(src, symbol)] <- Shift dst

    dfa
    |> List.map (fun state -> Set.intersect state acceptancesstates, Map.find state states)
    |> List.filter (fun (accept, _) -> not accept.IsEmpty)
    |> List.iter (fun (accept, src) ->
            let a = accept.MinimumElement
            let p = findaction a actions
            let flw = findproduction a actions |> fun production -> Map.find production follow
            for c in flw do hashtable.[(src, c)] <- Reduce p
            )

    hashtable.add (1, Terminal eof) Accept

    printfn "%A" hashtable
    hashtable, actions

let ItemsToPop (Productions productions) actions =
    Array.map (fun (Production(_, rules)) -> Array.map (Array.length << fst) rules) productions
    |> Array.concat
    |> fun items -> Array.zip items actions


[<Struct>]
type SLR<'token, 'production when 'token:comparison and 'production:comparison> =
    val table: HashTable<int * Symbol<'token, 'production>, Action>
    val actions: (int * 'production * (Token<Symbol<'token, 'production>>[] -> token))[]
    
    new(productions) =
        let tab, acts = MakeTable productions
        let actions =
            Array.map (fun (_, _, p, a) -> p, a) acts
            |> ItemsToPop productions 
            |> Array.map (fun (pops, (production, action)) -> pops, production, action)
                 

        { table = tab; actions = actions }

    
    member Parser.Run (tokens) =
        let mutable current = Option.map (Token<_>.map Terminal) <| Seq.tryHead tokens
        let mutable next = Seq.tail tokens
        let mutable states = Stack()
        let mutable stack = Stack()

        states.Push 0

        while current.IsSome && not (states.isEmpty()) do
            match Parser.table.[states.Peek(), TypeOf current.Value] with
            | None -> failwith "transition not allowed"
            | Some action ->
                match action with
                | Shift n ->
                    stack.Push current.Value
                    states.Push n
                    current <- Option.map (Token<_>.map Terminal) <| Seq.tryHead next
                    next <- Seq.tail next

                | Reduce p ->
                    
                    let pops, production, action = Parser.actions.[p]
                    let args = stack.Pop pops
                    let value = action args
                    stack.Push (Token(NonTerminal production, value, PosOf current.Value))
                    states.Drop pops

                    match Parser.table.[states.Peek(), NonTerminal production] with
                    | Some (Goto g) -> states.Push g

                    | _ -> failwith "goto not found"

                
                | Accept -> 
                    printfn "accept"
                    states <- Stack()
                | _ ->
                    failwith "parser error"

        ValueOf (stack.Pop())

    