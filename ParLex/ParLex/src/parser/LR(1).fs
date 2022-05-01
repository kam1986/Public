module internal LR1
(*

    TODO:
        
        1. In lookahead remember to find all states of nonterminal transition (nested too)
        2. To minimize actual computation cost, make a hashtable of lookahead for each state
           to avoid blow up in performance penalty since this is a heavy computation
        
*)
open Productions
open SLR
open NFA

#nowarn "25"
(*
    TODO: 
        Need to insert the production number into the item to
        tell some items apart
*)

// works


let First nfa =
    let (NFA nfa') = nfa
    let iteration lookahead =
        Map.fold (fun lookahead (srcState, trans) dstState -> 
            match trans with
            // Either we have an epsilon transition and we must look on the
            // destination state to find the next lookahead
            // or we have a nonterminal
            | E | T (NonTerminal _) ->
                // find current lookahead for src state
                let slah = 
                    match Map.tryFind srcState lookahead with
                    | None -> set[]
                    | Some s -> s
                // find the lookahead for dst state
                let dlah = 
                    Set.fold (fun lah state ->
                        (match Map.tryFind state lookahead with
                        | None -> set[]
                        | Some s -> s)
                        |> (+) lah
                        ) Set.empty dstState
                // add them up
                Map.add srcState (slah + dlah) lookahead
            | _ ->
               // here we simple do nothing
               // we assume that all terminal transitions are found
               // minimize doing the same computation again and again
               lookahead     
        ) lookahead nfa'

    // iterate over all lookaheads undtil we find a fixpoint
    let rec iterate lookahead =
        let lookahead' = iteration lookahead
        if lookahead = lookahead' then
            lookahead
        else
            iterate lookahead'
    
    // find all lookahead for terminal transitions
    let lookahead = 
        Map.fold (fun lookahead (src, trans) _ ->
            match trans with
            | T (Terminal c) ->
                // since c is the next nonterminal transition symbol
                // it is clearly in the lookahead set of the source state.
                match Map.tryFind src lookahead with
                | None -> Map.add src (set[c]) lookahead
                | Some lah -> Map.add src (Set.add c lah) lookahead
            | _ -> lookahead
        ) Map.empty nfa'

    iterate lookahead

// works
let Closure (first : Map<_,_ Set>) (NFA nfa) I =
    let iteration I =
        Set.fold (fun acc (state, lookaheads) ->
            // find all epsilon transitions
            Map.filter (fun (src, trans) _ -> src = state && trans = E) nfa
            // add all the states that can be reach with eps to I where we update lookahead
            |> Map.fold (fun acc _ dst ->
                // we which to find the first
                Set.fold (fun acc dst -> 
                        match Map.tryFind (state + 1) first with
                        | None -> Set.add (dst, lookaheads) acc
                        | Some lookaheads' -> Set.add (dst, lookaheads') acc
                    ) acc dst
                ) acc 
        ) I I

    let rec iterate I =
        let I' = iteration I
        if I' = I then
            I
        else iterate I'

    iterate I


let Goto first (NFA nfa as n) I x =
    Set.fold (fun items (state, lookahead) ->
        let dst = 
            match Map.tryFind (state, T x) nfa with
            | None -> set[]
            | Some dst -> dst

        Set.fold (fun items dst ->
            Set.add (dst, lookahead) items 
        ) items dst
    ) Set.empty I
    |> Closure first n

// not working, need to tell the difference between s -> c and c -> c
// need to adapt to this
let Items nfa =
    let (NFA nfa') = nfa
    let first = First nfa
    let language =
        Map.filter (fun (_,trans) _ -> trans <> E) nfa'
        |> Map.toSeq
        // have eleminated case E
        |> Seq.map (fun ((_, T t),_) -> t)
        |> Set.ofSeq
    

    let iteration trans I =
        Set.fold (fun (trans, items) item ->
            Set.fold (fun (trans, items) symbol ->
                let goto = Goto first nfa item symbol
                if not goto.IsEmpty then
                    Map.add (item, symbol) goto trans,
                    Set.add goto items
                else
                    trans, items
            ) (trans, items) language
        ) (trans, I) I

    let rec iterate trans I =
        let trans', I' = iteration trans I
        if I' = I then
            language, trans', I'
        else iterate trans' I'
    
    // return value are the computation of iterating over all items
    // adding new items undtil we meet a fixpoint
    iterate Map.empty (set[Closure first nfa (set[(0, set[])])])



// lookup function for simple compressed table form.
// it is not constant lookup, but the size will be relative small compared to normal form.
// and the ability to minimize cache misses will pay most of the penalty for linear search back
// by using arrays of arrays instead of arrays of (int * int * action) is that we in the state search part
// we can ommit checking every offset for those states since we know they do not fit.
//
// For most parsers the parsing table are inherently sparse populated with shift and reduce.
// for a 'simple' language like C this leads to a LR(1) parser tables with thousinds of states with many entries occupied by Errors
// in general a memory reduction in the area of +60% is to be expected
//
// This will also be used for the goto table 
let inline lookup<'a when 'a: comparison> (table: (int * ('a * Action) []) []) state offset =
    let mutable i = 0
    while i < table.Length && fst table.[i] < state do
        i <- i + 1 

    if i < table.Length || fst table.[i] <> state then
        Error // no legal transitions on that state at all ??
    else 
        // found state
        let row = snd table.[i]
        i <- 0 // reusing the same index variable
        // looking for action
        while i < row.Length && fst row.[i] < offset do
            i <- i + 1

        if i < row.Length || fst row.[i] <> offset then
            Error // no transition of the symbol the offset represent in that state
        else
            snd row.[i]


// compressed form (int * ('a * action)[])[]
// for huge table sparse tables (like most dfa's) this can reduce memory usage
// by a big factor
type CompressedTable<'lookup,'goto when 'lookup:comparison and 'goto: comparison> =
    val table : (int * ('lookup * Action) []) []
    val goto : (int * ('goto * Action) []) []

    internal new(table, goto) =
        {
            table = table
            goto = goto
        }

    member C.LookUp state token = 
        lookup C.table state token

    member C.Goto state token =
        lookup C.goto state token

let rec Insert state offset action lst =
    let rec insert lst = 
        match lst with
        | [] -> [(offset, action)]
        | (x, _) :: _ when x > offset -> (offset, action) :: lst
        | x::xs -> x :: insert xs
        
    match lst with
    | [] -> [state,[offset, action]]
    | (x,_) as x' :: xs when x < state -> x' :: (state, [offset,action]) :: xs
    | (x, lst') :: xs when x = state -> (x, insert lst') :: xs
    | x :: xs -> x :: Insert state offset action xs

let fix lst =
    List.map (fun (i,lst) -> i, List.toArray lst) lst
    |> List.toArray

// OBS need to handle reductions too
// OBS NEED TO BE REWORKED look at construction of the slr table
let MakeTable (follow: Map<_,_>, language : _ Set, goto : Map<_,_>, items : _ Set, actions: _ list) =
    
    let terminals =
        Set.filter (fun symbol -> match symbol with Terminal _ -> true | _ -> false) language
        |> Set.map (fun (Terminal c) -> c)

    let follow' = Map.map (fun _ item -> Set.map (fun (Terminal c) -> c) item) follow

    // check if there are exactly one EOF token type
    Set.filter (fun token -> token.ToString().ToLower() = "eof") language
    |> fun s -> 
        if s.Count = 0 then
            Result.Error "No EOF token"
        elif s.Count > 1 then
            Result.Error "To many EOF tokens"
        else
            // find the integer value of it
            let eof = 
                s.MaximumElement
                |> fun (Terminal c) -> c

            // The nfa state with number 1 will always be the acceptance of the grammar
            // hence we only need to find states that contains that nfa state
            let finalize =
                items
                |> Set.filter (fun state -> (Set.map (fun (state', _) -> state') state).Contains 1)

                

            let symbols = (Set.maxElement terminals).GetHashCode()
            let size = language.Count
            let numberOfStates = items.Count
    
            let mutable table = []
            let mutable goto' = []

            let states =
                items
                |> Set.fold (fun (map, count) item -> 
                    Map.add item count map, count+1
                ) (Map.empty, 0)
                |> fst

            let acceptingStates =
                List.map (fun (x,_,_,_) -> x) actions
                |> Set.ofList
                |> fun s -> s - set[1] // removing the state which should hold the accept action
            

            for state in finalize do
                let src = Map.find state states
                table <- Insert src eof Accept table


            for ((source, _), symbol), destionation in goto |> Map.toSeq do
                let src, dst = Map.find source states, Map.find destionation states
                let accept = 
                    source
                    |> Set.map (fun (i, _) -> i)
                    |> Set.intersect acceptingStates
        
                match symbol with
                | Terminal c ->
                    table <- Insert src (c) (Shift dst) table

                | NonTerminal N ->
                    let flw = Map.find N follow'
                    let p = findaction accept.MinimumElement actions
                    match accept.Count with
                    | 0 ->
                        goto' <- Insert src N (Action.Goto dst) goto'
                    | _ ->
                        goto' <- Insert src N (Action.Goto dst) goto'
                        for c in flw do
                            let offset = c
                            table <- Insert src offset (Reduce p) table

            for item in items do
                let accept = Set.intersect (Set.map (fun (i,_) -> i) item) acceptingStates
                if not accept.IsEmpty then
                    let src = Map.find item states
                    let a = accept.MinimumElement
                    let p = findaction a actions
                    let flw = findproduction a actions |> fun production -> Map.find production follow'
                    for c in flw do
                        table <- Insert src (c) (Reduce p) table
            
            CompressedTable(fix table, fix goto') |> Ok
        



