module DFA

#nowarn "25"

open Position
open Regex
open Mapping
open TypeAcrobatics 
open Jumptable


let rec GetLanguage regex =
    match regex with
    | regex.Atom (a, _) -> set[a]
    | regex.Cat (regex1, regex2, _, _, _) | regex.Or (regex1, regex2, _, _, _) ->
        GetLanguage regex1 + GetLanguage regex2
    | regex.Star(regex, _, _) -> GetLanguage regex
    | _ -> set[]


let rec GetCorrespondence regex correspond =
    match regex with
    | regex.Atom (a, i) ->
        match Map.tryFind a correspond with
        | None -> Map.add a (set[i]) correspond
        | Some c -> Map.add a (set[i] + c) correspond
    
    | regex.Cat (regex1, regex2, _, _, _) | regex.Or (regex1, regex2, _, _, _) ->
        GetCorrespondence regex1 correspond
        |> GetCorrespondence regex2

    | Star (regex, _, _) ->
        GetCorrespondence regex correspond

    | _ -> correspond


// this could be made slightly faster by using sets instead of lists
let inline StateFinder regex =
    let mutable DTran = Map.empty
    let language = GetLanguage regex
    let followposOf = fun item -> Map.find item (Followpos regex Map.empty)
    let correspondTo = fun item -> Map.find item (GetCorrespondence regex Map.empty)
    
    let start = FirstPosOf regex
    

    // we uses Sets as they do not allow doublicates
    // and are based on binary tree structures which makes them both fast to update and lookup O(lg(n))
    // in general in this algorithm sets are mighty useful because they fast operations when determening if something is a member of them
    // union of two sets are fast too and the intersection of two sets are also fast
    let mutable unmarked = set[FirstPosOf regex]
    let mutable marked = set[]
   
    // find all states by keeping tracks on which already has been found i.e. marked states
    while not <| Set.isEmpty unmarked do 

        // the invariant of the loop makes this element always exists
        let S = unmarked.MinimumElement
        unmarked <- Set.remove S unmarked // mark first state
        for a in language do
            let U =
                Set.fold 
                    (fun ps p -> ps + followposOf p) 
                    Set.empty
                    (Set.intersect S (correspondTo a))
            // we do only add a new state to unmarked if it does not appear in both stacks
            if (not << Set.isEmpty) U then
                if not (Set.contains U (unmarked + marked)) then 
                    unmarked <- Set.add U unmarked
            
                DTran <- Map.add (S, a) U DTran

        if not <| Set.isEmpty S then
            marked <- Set.add S marked
            
    
    language, start :: Set.toList (marked.Remove start), DTran


(*

    The code below, build a DFA transition table.
    an entry set to -1 simulate a illegal transition, at this point the DFA should stop
    and check for accepting, if not return an error, else return token.
    
    If a state in construction find that it has strictly more than 1 acceptance, it pick the one to return
    that has the earlist pattern in the list of patterns.

*)


// need the numbering of terminal to be in range -1 .. -n + 1 
let inline getAcceptancePrState (states : int Set seq) (acceptance : ('token * ('a -> 'b)) seq) =
    let states = Array.ofSeq states
    let acceptance = Array.ofSeq acceptance
    let acceptance' =
        Array.zip [|0 .. states.Length-1|] states                                                                                                  // number the states, assuming sorted list 
        |> Array.filter (fun (_, state) -> Set.exists (fun x -> x < 0) state)                                                                      // filter out all non acceptance state
        |> Array.map (fun (numb, state) -> numb, state |> Set.filter (fun x -> x < 0) |> Set.maxElement |> fun action -> acceptance.[-action-1])   // extract the acceptance action with hihgest precedence
        |> Map.ofArray                                                                                                                             // return as a map

    // abusing the TryFind ability to give an option type back, hence automatical setting all none terminal states acceptance to None
    // and all terminal state to Some tranformation
    [| for entry in 0 .. 255 -> Map.tryFind entry acceptance' |]
     

let inline makeTable ((language : byte Set), (states : int Set list), transitions) (acceptance : ('token * (string -> token)) option[]) =
    let mutable table = JumpTable(0)
    // label states with numbers from 0 to n-1, where n is the number of states
    let states' = Map.ofSeq <| Seq.zip states [ 0 .. Seq.length states - 1 ]

    // find the minimum and maximum offset of the language
    let min', max' = int <| Set.minElement language, int <| Set.maxElement language
   
    // inserting legal transitions
    // using Seq. to enable change og collection in the input
    Set.iter ( 
        fun letter -> 
            List.iter (
                fun state -> 
                    let src = Map.find state states'
                    // fix here
                    match Map.tryFind (state, letter) transitions with
                    | None -> ()
                    | Some destination -> 
                        let dest = Map.find destination states'
                        // find index of the transtion of the letter from the states offset
                        // size' * stateNumber finds the offset where the state begin, 
                        // and letter - min' is the offset from the starting position of the state
                        // where to be find the correct transition
                        // find the right state ofset to jump to and change the above entry to the state number
                        table.[src, int letter] <- byte <| dest 
            ) states
        ) language

    // insert acceptance states
    Seq.zip [| 0 .. 255 |] acceptance
    |> Seq.iter (fun (state, action) -> table.Accept(state) <- action) 
    
    table


// recursive lexing algorithm
let dfamap eof table pos =
    // capture the starting position of the token
    let start = pos
    let rec loop (table : JumpTable<_>) token state pos bytes =
        match Seq.tryHead bytes with
        | Some c ->
            match table.[int state, int c] with
            | None -> 
                match token with
                | None -> Error ("Transition Error at " + string pos)  // OBS! better error mapping when outer layer of the lexer has been constructed
                | Some (state, tokenstart, tokenend) ->
                    // legal acceptance state and no me transition for that token
                    let toktype, func = table.Accept(int state).Value
                    Ok((toktype, func, tokenstart, tokenend), bytes)

            | Some state -> 
                let token =
                    if table.Accept(int state).IsSome then
                        Some(state, start, pos)
                    else
                        token

                let pos = 
                    if c = 0xAuy then
                        { pos with 
                            Line = pos.Line + 1 
                            Offset = 0
                            Absolut = pos.Absolut + 1
                        }
                    else
                        { pos with 
                            Offset = pos.Offset + 1
                            Absolut = pos.Absolut + 1
                        }

                loop table token state pos (Seq.tail bytes)
        // End Of File/Stream case
        | None -> 
            match token with
            // no token found = eof
            | None -> Ok((eof, (fun str -> Arg str), pos,pos), bytes)
            // return last token found
            | Some (state, tokenstart, tokenend) ->
                // legal acceptance state and no me transition for that token
                let toktype, func = table.Accept(int state).Value
                Ok((toktype, func, tokenstart, tokenend), bytes)
    
    // loop through the byte stream undtil no more legal transistions can be made, 
    // and return the token with the longest prefix if such can be found
    (loop table None 0uy start)
    |> Map
