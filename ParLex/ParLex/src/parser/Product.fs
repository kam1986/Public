module Product


open Token
open NFA
open TypeAcrobatics

type Symbol<'T, 'N when 'T:comparison and 'N:comparison> =
    | Terminal of 'T
    | NonTerminal of 'N
with
    override s.ToString() = 
        match s with
        | Terminal t -> string t
        | NonTerminal n -> string n



let (!) t = Terminal t
let (!!) n = NonTerminal n

type Production<'T, 'N when 'T:comparison and 'N:comparison> =
    | Production of ('N * (Symbol<'T,'N>[] * (Token<Symbol<'T,'N>>[] -> token)) [])

type Productions<'T,'N when 'T: comparison and 'N: comparison> =
    | Productions of Production<'T,'N>[]


// we use 'inline' heavily because it shouldn't make a change to the binary size
// but it will force the F# compiler to substitute a function call with the function body
let inline Productions productions = 
    productions
    |> List.toArray
    |> Productions

let inline ( >> ) rule action = rule, Delay action

// uses list syntax to ease coding
// transform to arrrays internally to enhance performance
let inline ( => ) N rules = 
    let rules =
        Array.ofList rules
        |> Array.map (fun (symbols, action) -> Array.ofList symbols, action)
    Production(N, rules)


let inline private MakeNFAOfRule nfa state rule =
    Array.fold (fun (state, nfa) symbol -> 
        let next = state + 1
        next, Map.add (state, Transition symbol) (set[next]) nfa
    ) (state, nfa) rule


let inline private MakeNFAOfProduction rule' nfa state actions (Production(production, rules)) =
    (*
        nfa: the transistion table over states
        rule': number of rules
        state: the starting state of the rule
        statesof: a list of starting states of the production
        actions: 
            a collection of the end state of the rule, which rule in use, 
            corresponding production and action to be taken.
    *)
    ((nfa, rule', state, [], actions), rules)
    ||> Array.fold (fun (nfa, rule', state, statesof, actions) (rule, action) ->
        let state', nfa = MakeNFAOfRule nfa state rule
        let action' = state', rule', production, action
        nfa, rule' + 1, state' + 1, state :: statesof, action' :: actions)
    |> function nfa, count, state, statesof, actions -> nfa, count, state, (production, Array.ofList statesof), actions


let inline internal MakeNFA (Productions productions: Productions<'token,'production>) =
    // make NFA 
    ((Map.empty, 0, 0, [], []), productions)
    ||> Array.fold (fun (nfa, count, state, statesof, actions) production ->
        let nfa, count', accepts, states, actions' = MakeNFAOfProduction count nfa state actions production
        nfa, count', accepts, states :: statesof, actions' 
    )
    |> fun (nfa, _, states, statesof, actions) ->
        let statesof = Array.ofList statesof
        // corrects the nfa by adding epsilon transitions between states where there is an nonterminal transistion
        // to all the starting state of that production. epsilon transition to each rule of that production are already added.
        (nfa, [| 0 .. states |]) 
        ||> Array.fold (fun nfa state -> 
            (nfa, Array.map (function Production(p,_) -> p) productions)
            ||> Array.fold (fun nfa nonterminal ->
                match Map.tryFind (state, Transition (NonTerminal nonterminal)) nfa with
                | Some _ ->                
                    (nfa, snd <| Array.find (fun (p, _) -> p = nonterminal) statesof)
                    ||> Array.fold (fun nfa state' ->
                        match Map.tryFind (state, Epsilon) nfa with
                        | Some states -> Map.add (state, Epsilon) (Set.add state' states) nfa
                        | None -> Map.add (state, Epsilon) (set[state']) nfa
                         
                    )
                    
                | None -> nfa
            )
        )
        |> fun nfa -> nfa, (Array.rev << Array.ofList) actions


let inline IsNonTerminal t = 
    match t with
    | Terminal _ -> false
    | _         -> true


let inline private nullable last symbols =
    let mutable na = true
    let mutable cur = 0

    while na do            
        na <- Array.length symbols < cur || (match symbols.[cur] with Terminal _ -> false | NonTerminal n -> Map.find n last)
        cur <- cur + 1
    
    na
    
        


let inline private NullablesOfRule last lines =
    let mutable n = true
    let mutable line = 0
    
    while Array.length lines < line && n do
        n <- nullable last lines.[line]
        line <- line + 1
    
    n 


let inline private Nullable (Productions productions) =
    let inline Iteration last =
        Array.fold (fun next (Production(n, lines)) -> Map.add n (NullablesOfRule last (Array.map fst lines)) next) Map.empty productions

    let rec Iterate prod last =
        let next = Iteration last
        match next = last with
        | false -> Iterate prod next
        | _ -> last

    (Map.empty, productions)
    ||> Array.fold (fun first (Production(n, _)) -> Map.add n false first) 
    |> Iterate productions


let inline private FirstOfLine nullables firsts line =
    let mutable symbol = 0
    let mutable first = Set.empty
    let mutable run = true

    while run && symbol < Array.length line do
        match line.[symbol] with
        | Terminal _ as symbol -> 
            first <- Set.add symbol first 

        | NonTerminal N ->
            let f = Map.find N firsts
            if Map.find N nullables then
                first <- first + f
            else
                run <- false
        symbol <- symbol + 1

    first


let inline private FirstOfLines nullables firsts lines =
    let mutable line = 0
    let mutable first = Set.empty
    
    while line < Array.length lines do
        first <- FirstOfLine nullables firsts lines.[line]
        line <- line + 1

    first

let inline private First nullables (Productions productions) =
    let mutable last = Map.empty
    let mutable next = Array.fold (fun first (Production(N, _)) -> Map.add N Set.empty first) last productions
    let mutable production = 0
    while next <> last do
        while production < Array.length productions do
            last <- next
            let (Production(N, lines)) = productions.[production]
            next <- Map.add N (FirstOfLines nullables last (Array.map fst lines)) next
            production <- production + 1
        production <- 0
    next


let inline private FollowPerRuleByFirst first (follow: Map<_,_> byref) rule = 
    let mutable symbol = 0
    while symbol < Array.length rule do
        // it will not error on length error
        match rule.[symbol .. symbol + 1] with
        | [| NonTerminal N; Terminal c |] ->
            let fn = Map.find N follow
            follow <- Map.add N (Set.add (Terminal c) fn) follow
            symbol <- symbol + 2

        | [| NonTerminal N; NonTerminal B |] ->
            let fb = Map.find B first
            let fn = Map.find N follow
            follow <- Map.add N (fn + fb) follow
            symbol <- symbol + 1

        | _ -> symbol <- symbol + 1

let inline private FollowPerProductionByFirst first (follow: Map<_,_> byref) (Production(_, rules)) =
    for (rule, _) in rules do FollowPerRuleByFirst first (&follow) rule


let inline private GetFollowConstraintByFirst first (follow: Map<_,_> byref) (Productions productions) =
    for p in productions do FollowPerProductionByFirst first (&follow) p



let inline private PerRule nullable (follow: Map<_,_> byref) M rule =
        let mutable cur = 0
        while cur < Array.length rule do
            match rule.[cur .. cur + 1] with
            | [| NonTerminal N |] -> // will be shorter than 2 if there is less than 2 elements
                let fm = Map.find M follow
                let fn = Map.find N follow
                follow <- Map.add N (fn + fm) follow 
            
            | [| NonTerminal N; NonTerminal B |] ->
                let fm =
                    if Map.find B nullable then
                        Map.find M follow
                    else
                        Set.empty

                let fn = Map.find N follow
                let fb = Map.find B follow

                follow <- Map.add N (fm + fn + fb) follow
                

            | _ -> ()

            cur <- cur + 1
 
let inline private PerProduction nullable (follow: Map<_,_> byref) (Production(M, rules)) = 
    // can't use Array.iter because of semantic rules for 'byref' under closure
    for (rule, _) in rules do PerRule nullable (&follow) M rule

let inline private Find nullable (follow: Map<_,_> byref) (Productions productions) =
    for p in productions do PerProduction nullable &follow p

let inline private GetFollowConstraintByFollow nullable (follow: Map<_,_> byref) productions =
    let mutable last = Map.empty
    while last <> follow do
        last <- follow
        Find nullable (&follow) productions
        

let inline internal Follow (Productions p as productions) =
    let nullable = Nullable productions
    let first = First nullable productions
    let mutable follow = Map.empty

    Array.iter (function Production(M, _) -> follow <- Map.add M Set.empty follow) p
    GetFollowConstraintByFirst first (&follow) productions
    GetFollowConstraintByFollow nullable (&follow) productions

    follow