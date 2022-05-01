module internal DFA

#nowarn "25"
// just a junk function showing that we can make 'maybe equal' 
// let ( ?= ) a b = if a = b then Ok b else sprintf "%A is not equal %A" a b |> Error 
open Position
open Result
open Regex
open Iter
open Mapping
open TypeAcrobatics
open Token
open Jumptable

(* 
    For now we use Maximum value of a byte as error mark.
    This is do to the ASCII encoding uses only the 0 - 127 interval
    And UFT8 can not have a full set byte in a sequence

    i.e.
    ASCII byte 0xxxxxxx.
    UTF8 byte sequence have following format where x is either set ot not
        single byte : 0xxxxxxx
        double byte : 110xxxxx 10xxxxxx
        triple byte : 1110xxxx 10xxxxxx 10xxxxxx
        four   byte : 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx

    This might be change since there is a lot of other bits invalid byte to be used as mark 
*)

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
    let followpos =  Followpos regex Map.empty
    let language = GetLanguage regex
    let followposOf = fun item -> Map.find item followpos
    let correspondTo = fun item -> Map.find item (GetCorrespondence regex Map.empty)
    

    // use single linked list as two stacks.
    let mutable unmarked = [FirstPosOf regex]
    let mutable marked = []
    // the loop below are fine but not peak optimal in performance
    // should be changed to perform better
    while unmarked <> []  do // <> is equal to != in C#
        // the condition of the loop makes sure that this is always true.
        let (S :: unmarked') = unmarked
        unmarked <- unmarked' // mark first state
        for a in language do
            let U =
                Seq.fold 
                    (fun ps p -> ps + followposOf p) 
                    (set[]) 
                    (Set.intersect S (correspondTo a))
            // we do only add a new state to unmarked if it does not appear in both stacks
            if (not << Set.isEmpty) U then
                if not (Seq.contains U unmarked || Seq.contains U marked) then 
                    unmarked <- (U :: unmarked)
            
                DTran <- Map.add (S, a) U DTran
        if not <| Set.isEmpty S then

            marked <- List.distinct (S :: marked)
            
    
    language, List.rev marked, DTran


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
        |> Array.map (fun (numb, state) -> numb, state |> Set.filter (fun x -> x < 0) |> Set.maxElement |> fun action -> acceptance.[-action])   // extract the acceptance action with hihgest precedence
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
    // finde the distance between those two.
    let size' = max' - min' + 1


    // inserting legal transitions
    // using Seq. to enable change og collection in the input
    Seq.iter ( 
        fun letter -> 
            Seq.iter (
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
let inline dfamap eof table pos =
    // capture the starting position of the token
    let start = pos
    let rec loop (table : JumpTable<_>) token state pos bytes =
        match Seq.tryHead bytes with
        | Some c ->
            match table.[int state, int c] with
            | None -> 
                match token with
                | None -> Error ("Transition Error at " + string pos)               // OBS! better error mapping when outer layer of the lexer has been constructed
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
            | None -> Ok((eof, (fun str -> Arg str), pos, pos), bytes)
            // return last token found
            | Some (state, tokenstart, tokenend) ->
                // legal acceptance state and no me transition for that token
                let toktype, func = table.Accept(int state).Value
                Ok((toktype, func, tokenstart, tokenend), bytes)
    
    // loop through the byte stream undtil no more legal transistions can be made, 
    // and return the token with the longest prefix if such can be found
    (loop table None 0uy start)
    |> Map
