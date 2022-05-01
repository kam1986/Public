module Productions

open NFA
open Token
open TypeAcrobatics


type Symbol<'T,'N> = Terminal of 'T | NonTerminal of 'N 

let isTerminal t = 
    match t with
    | Terminal _ -> true
    | _         -> false

let (!) item = Terminal item
let (~%) item = NonTerminal item



type Production<'t, 'T,'N> = Production of ('N * (Symbol<'T,'N> list * (Token<'t>[] -> token)) list)

type Productions<'t, 'T,'N> = Productions of Production<'t, 'T,'N> list

let internal empty = NFA Map.empty

let ( >> ) rule (action : Token<_>[] -> _) = rule, Delay action

let ( => ) N rules = Production(N, rules)


// transform each terminal and non terminal into a integer representation of the same productions to enable diviations
let inline internal ToCommonRepresentation (Productions productions) =
    productions 
    |> List.map (fun (Production(N, rules)) -> 
        N.GetHashCode() => 
            List.map (fun (rule, action) ->
                List.map (fun symbol -> 
                    match symbol with
                    | Terminal c -> Terminal (c.GetHashCode())
                    | NonTerminal N -> NonTerminal (N.GetHashCode())
                   
                ) rule, action
            ) rules
        )
    |> Productions


let internal AddDiviation n rules actions (Productions productions) =
    (n => List.zip rules actions) :: productions
    |> Productions

// endpoint are handled as a terminal with the value -1
// this wil make the parse table much more readable since it will alway has the same placement namely furthest left
// need to add another action function
let internal addEndOfParse (Productions productions) =
    match productions with
    | [] -> [ -1 => [[!(-1)] >> fun args -> args.[0]]]
    | Production(N, _) :: _ ->
        (-1 => [[%N] >> fun args -> args.[0]]) :: productions
    |> Productions
                    

let internal GetErrorMessages (Production (p, rules)) =
    List.map (fun (rule, _) -> rule) rules
    |> List.map (fun rule -> p, rule)
    |> List.map (fun (p, rule) -> 
        "In the production " + string p + "\n    rule: " + 
        List.fold (fun str symbol -> 
            match symbol with
            | NonTerminal t | Terminal t -> str + string t + " "
            ) "" rule + 
        "\n"
        )




// TODO need to make list of start state for each rule to make epsilon transitions to them from nonterminal transistions
let rec internal MakeNFAOfRule (NFA nfa) state rule =
    match rule with
    | [] -> state+1, NFA nfa
    | symbol :: rule -> 
        let next = state + 1
        let nfa = Map.add (state, T symbol) (set[next]) nfa |> NFA
        MakeNFAOfRule nfa next rule

let internal MakeNFAOfproduction count nfa state actions (Production (production, rules)) =
    let rec ofRules count nfa actions state statesof rules =
        match rules with
        | []            -> statesof, nfa, state, actions, count
        | (rule, action) :: rules ->
            let state', nfa = MakeNFAOfRule nfa state rule
            // 
            ofRules (count+1) nfa ((state' - 1, count, production, action)::actions) state' (state :: statesof) rules
    
    let statesof, nfa, count, actions, accept = ofRules count nfa state actions [] rules // should include actions as terminals
    (production, statesof), nfa, count, actions, accept


// works now
// SHOULD DEFENTLY be optimized since it is O(n^3) or worse
let internal MakeNFA (Productions productions) =
    // make nfa for each production rule
    List.fold (fun (statesof, nfa, count, actions, accepts) production -> 
        let states, nfa, count', actions', accepts = MakeNFAOfproduction accepts nfa actions count production
        (states :: statesof, nfa, count', actions', accepts) 
    )  ([], empty, 0, [], 0) productions
    |> fun (statesof, nfa, count, actions, _) ->
        List.fold (fun (NFA nfa) state ->
            List.fold (fun (NFA nfa) nonterminal ->
                // check if there is a transition by nonterminal from state
                match Map.tryFind (state, T (NonTerminal nonterminal)) nfa with
                | None -> NFA nfa // no tranistion 
                | Some _ -> // a transition
                    // find all states that should have an epsilon transition
                    let _, startstates = List.find (fun (p, _) -> p = nonterminal) statesof
                    // for each of the above states "state'" make an epsilon transistion from state to stat'
                    List.fold ( fun (NFA nfa) state' -> 
                        match Map.tryFind (state, E) nfa with
                        | None -> 
                            Map.add (state, E) (set[state']) nfa 
                            |> NFA
                        | Some states -> 
                            Map.add (state, E) (Set.add state' states) nfa 
                            |> NFA
                    ) (NFA nfa) startstates
            ) (NFA nfa) (List.map (fun (Production(p, _)) -> p) productions)
        ) nfa [ 0 .. count ]
        |> fun nfa -> nfa, List.rev actions



let rec internal nullable acc last symbols =
    match acc, symbols with
    | _, [] | false, _ -> acc
    | _, (Terminal _) :: symbols -> false
    | _, (NonTerminal n) :: symbols -> nullable (Map.find n last) last symbols


let internal IterateRule last lines =
    let rec iterate pred acc lst =
        match acc, lst with
        | _, [] | true, _ -> acc
        | _, line :: lines -> iterate pred (pred true last line) lines
    iterate nullable false lines


let internal Nullable (Productions productions) =
    let Iteration last = 
        List.fold (fun next (Production(n, lines)) -> Map.add n (IterateRule last (List.map fst lines)) next) Map.empty productions 
    
    let rec Iterate prod last =
        let next = Iteration last
        match next = last with
        | false -> Iterate prod next
        | _ -> last

    // compute the initial map of all nonterminals with state 'false'
    List.fold (fun first (Production(n, _)) -> Map.add n false first) Map.empty productions
    |> Iterate productions // find nullable mapping
    

let rec internal fline nullable fst first line =
    match line with
    | [] -> first
    | (Terminal _ as a) :: line -> Set.add a first
    | (NonTerminal N) :: line ->
        let f = Map.find N fst
        if Map.find N nullable then
            fline nullable fst (first + f) line
        else
            f


let rec internal flines nullable fst first lines =
    match lines with
    | [] -> first
    | line :: lines -> 
        // find the set of all first in given last itereation 'firstOf'
        flines nullable fst (first + fline nullable fst (set[]) line) lines

// nullable are added as an argument to prevent multiple calculations of it
let internal First nullable ((Productions productions) as p) =
    let rec iteration nullable first next productions =
        match productions with
        | [] -> next
        | Production(N, lines) :: productions -> 
            iteration nullable first (Map.add N (flines nullable first (set[]) (List.map fst lines)) next) productions
    
    let first = List.fold (fun first (Production(N, _)) -> Map.add N Set.empty first) Map.empty productions
    
    let rec iterate nullable last productions =
        let next = iteration nullable last first productions
        match next = last with
        | true -> next
        | _ -> iterate nullable next productions
        
    iterate nullable first productions


// adding all first sets to the proper follow sets, this is a one time computation 
// since first set doesn't depend on follow sets
let internal GetFollowConstraintByFirst first follow productions =
    let rec perRule follow rule =
        match rule with
        | [] -> follow

        | (NonTerminal N) :: (Terminal c) :: rule ->
            let fn = Map.find N follow
            perRule (Map.add N (Set.add (Terminal c) fn) follow) rule

        // the Follow(M) in Follow(N) here is not added since it can be errorfull by non calculated set
        | NonTerminal N :: ((NonTerminal B :: _) as rule) ->
            let fb = Map.find B first
            let fn = Map.find N follow
            perRule (Map.add N (fn + fb) follow) rule
        
        | _ :: rule -> perRule follow rule



    let perProduction follow (Production(_, rules)) =
        List.fold (fun follow (rule, _) -> perRule follow rule) follow rules

    let findAll follow (Productions productions) =
        List.fold (fun follow production -> perProduction follow production) follow productions
            
    findAll follow productions   



let internal GetFollowConstraintByFollow nullable follow productions =
    let rec perRule M follow rule =
        match rule with
        | [] -> 
            follow
        
        | NonTerminal N :: [] ->
            let fm = Map.find M follow
            let fn = Map.find N follow
            Map.add N (fn + fm) follow
        
        | NonTerminal N :: ((NonTerminal B :: _) as rule) ->
            match Map.find B nullable with
            | true -> 
                let fm = Map.find M follow
                let fn = Map.find N follow
                let fb = Map.find B follow
                perRule M (Map.add N (Set.unionMany [fm;  fn;  fb]) follow) rule
            
            | _ ->
                let fn = Map.find N follow
                let fb = Map.find B follow
                perRule M (Map.add N (fn + fb) follow) rule
        

        | _ :: rule -> perRule M follow rule

    let perProduction follow (Production(M, rules)) =
        List.fold (fun follow (rule, _) -> perRule M follow rule) follow rules

    let find follow (Productions productions) =
        List.fold (fun follow production -> perProduction follow production) follow productions

    let rec iterate lastFollow =
        let nextFollow = find lastFollow productions

        match nextFollow = lastFollow with
        | true -> nextFollow 
        | _ -> iterate nextFollow

    iterate follow


let internal Follow (Productions productions as p) =
    let nullable = Nullable p
    let first = First nullable p
    let AddEndpoint =
        match productions with
        | [] -> failwith "empty production"
        | Production(M,_) :: _ -> Map.add M (set[Terminal -1])

    List.fold (fun follow (Production(M, _)) -> Map.add M Set.empty follow) Map.empty productions
    |> AddEndpoint
    |> fun follow -> GetFollowConstraintByFirst first follow p
    |> fun follow -> GetFollowConstraintByFollow nullable follow p



