module ISCFG


(*

    OBS!!! ændre Terminal til kun Terminal(terminal, indentation) og find prec og ass ved lookup

    We treat indentation as a 'lookahead' when building the LR parser hence we end up with more states, but as with lookahead it solves some ambiguasy.
*)
// for indentation sensitive language
type Indentation =
    | NoIndent  // indentation insensitive i.e. normal CFG 
    | Geq       // greater or equal the left side
    | Greater   // strictly greater of the left side
    | Eq        // the same indentation than the left side
    | Leq       // less or equal than the left side
    | Less      // stricly less than the left side

// Interpreter: the rule of associative is that NoAss < Left < Right, hence if a conflict is coursed by associativity we we choose right over left and left over noass with transitivity over them.
// OBS for the Generator it will be possible to make user defined order.
type Associativity = NoAss | Left | Right 

// for precedence handling
// %nonprec = 0
type Precedence = uint32
type State = State of int
// typed out to make the type system catch errors
type NonTerminal = NonTerminal of string
type Terminal =  Terminal of string
type Action = Action of string  // string format of the action taken


type Symbol =
    | Terminal of Terminal * Indentation
    | NonTerminal of NonTerminal * Indentation

type Rule = Rule of Symbol list * Action

type Production = Production of NonTerminal * Rule list

type Productions = Productions of Production list

type Transition = Transition of Symbol | Epsilon 

type NFA = NFA of Map<State * Transition, State Set>



(*

    We run through each rule to set associativity and precedence of an action

*)


let GatherPrec precedences =
    List.fold (fun (map, prec : Precedence) symbols -> 
            List.fold (fun map symbol -> Map.add symbol prec map) map symbols, prec + 1u
        ) (Map.empty, 0u) precedences
    |> fst
     

let GatherAss associativity =
    List.fold(fun map (ass : Associativity, symbols : 'symbol list) -> 
        List.fold(fun map symbol -> Map.add symbol ass map) map symbols
    ) Map.empty associativity



let Lookup _default key map =
    match Map.tryFind key map with
    | None -> _default
    | Some item -> item



let rec FindAssPrec prec ass precedences associativities rule =
    List.fold (fun (prec, ass) symbol -> 
        match symbol with
        | Terminal (terminal, _) -> 
            let prec' = precedences |> Lookup 0u terminal 
            let ass'  = associativities |> Lookup NoAss terminal 
            prec', ass'
        | _ -> prec, ass
    ) (prec, ass) rule


let GetAssPrecAction precedences associativities (Productions(productions)) =
    List.fold (fun (map, actionnumb) (Production(_, rules)) -> 
        List.fold (fun (map, actionnumb) (Rule(rule, _)) ->
            let prec, acc = FindAssPrec 0u NoAss precedences associativities rule
            // add the precedence and associativity of action with number 'actionnumb' to the map. 
            (Map.add actionnumb (prec,acc) map, actionnumb+1)
        ) (map, actionnumb) rules
    ) (Map.empty, 0) productions



let Add p i map =
    match Map.tryFind p map with
    | None -> 
        Map.add p (set[i]) map
    | Some s -> 
        Map.add p (Set.add i s) map

let rec MakeNFAOfRule (NFA nfa) state rule =
    List.fold (fun (nfa, state) symbol -> 
        let next = state + 1
        Add (State state, Transition symbol) (State next) nfa, next
    ) (nfa, state) rule
    |> fun (nfa, state) -> NFA nfa, state + 1


let MakeNFAOfProduction nfa action state startstates endstates (Production (production, rules)) =
    List.fold (fun (nfa, action, state, startstates, endstates) (Rule(rule,_)) ->        
        let startstates = Add production state startstates
        let nfa, state = MakeNFAOfRule nfa state rule
        let endstates = (state-1, action) :: endstates
        (nfa, action + 1, state, startstates, endstates)
    ) (nfa, action, state, startstates, endstates) rules
    

let MakeNFA (Productions productions) =
    // make NFA 
    List.fold (fun (nfa, action, state, startstates, endstates) production -> 
        MakeNFAOfProduction nfa action state startstates endstates production
    ) (NFA Map.empty, 0, 0, Map.empty, []) productions
    // add epsilon transitions
    |> fun (NFA nfa, _, _, startstates, endstates) -> 
        Map.fold (fun nfa (src, t) _ ->
            match t with
            | Transition (NonTerminal (nonterminal, _)) ->
                Set.fold (fun nfa dst -> 
                        Add (src, Epsilon) (State dst) nfa
                ) nfa startstates.[nonterminal]
            | _ -> nfa
        // return the complete NFA and all accepting states with their actions
        ) nfa nfa |> NFA, endstates

