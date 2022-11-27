module internal NFA

type 'T Transitions = Transition of 'T | Epsilon

type NFA<'T when 'T : comparison> = NFA of Map<(int * 'T Transitions), int Set>

let empty = NFA Map.empty


let rec F M (NFA n as nfa) s =
    let next = 
        Set.fold (fun next i -> 
            match Map.tryFind (i, Epsilon) n with
            | None -> next
            | Some s -> next + s
        ) (set[]) s + M
    
    if Set.isEmpty next then
        s
    else
        F Set.empty nfa (next-s) + s


let Move (NFA n as nfa) s' c =
    Set.fold (fun next i -> 
        match Map.tryFind (i, Transition c) n with
        | None -> next
        | Some s -> next + s
    ) (set[]) s'
    |> F Set.empty nfa 


let MakeDFA nfa language =
    let iteration trans states =
        Seq.fold (fun (trans, states') letter ->
            Seq.fold (fun (trans, states) state ->
                let move = Move nfa state letter
                if Set.isEmpty state || Set.isEmpty move then
                    
                    trans, states
                else                       
                    Map.add (state, letter) move trans, Set.add move states
                    
            ) (trans, states') states
        ) (trans, Set.empty) language    
    
    let start = F (set[0]) nfa (set[])

    let rec iterate trans states news =

        let oldies = (states + news)
        let trans', next = (iteration trans news)
        
        match Set.isEmpty (next - oldies) with
        | true -> trans', oldies
        | _ -> iterate trans' oldies (next - oldies)

    let trans, dfa = iterate Map.empty Set.empty (set[start])
    trans, start :: Set.toList (dfa - set[start])

