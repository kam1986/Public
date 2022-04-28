// indentation sensitive parser
// A SLR parser combined with indentation sensitive parsing should be expressiv enough
// the same expressiveness as a language with enclosings symbols as brackets
module ISP

open NFA
open Token
open TypeAcrobatics
open Position

type indentationMark = Set | Stay
type indentationRequest = Less | Leq | Eq | Geq | Greater | All    

// a way to represent terminals an nonterminals with indentation sensitivity
type Symbol<'terminal, 'nonterminal> =
    | Terminal  of 'terminal * indentationMark * indentationRequest
    | NonTerminal  of 'nonterminal * indentationRequest
  
   

// prefix operators for the above type instances
// the Set mark will be automatically set under the transformation to the parsetable for all first elements of a rule
let (!>) item  = Terminal (item, Stay, Less)
let (!>=) item = Terminal (item, Stay, Leq)
let (!<=) item = Terminal (item, Stay, Geq)
let (!<) item  = Terminal (item, Stay, Greater)
let (!) item   = Terminal (item, Stay, Geq)

let (!!>) item  = NonTerminal (item, Less)
let (!!>=) item = NonTerminal (item, Leq)
let (!!=) item  = NonTerminal (item, Eq)
let (!!<=) item = NonTerminal (item, Geq)
let (!!<) item  = NonTerminal (item, Greater)
let (!!) item   = NonTerminal (item, Eq)

// used to build nfa and dfa states for the table
type Indentation = LESS | EQ | GREATER


type Action = Error | Shift of int * indentationMark | Reduce of int | Accept
type Entry = (Action * Action * Action)

let less ((l, e, g) : Entry) = l
let eg ((l, e, g) : Entry) = e
let greater ((l, e, g) : Entry) = g


type Production<'t, 'T,'N> = Production of ('N * (Symbol<'T,'N> list * (Token<'t> []-> token)) list)

type Productions<'t, 'T,'N> = Productions of Production<'t, 'T,'N> list

let internal empty = NFA Map.empty

let ( >> ) rule (action : Token<_> [] -> _) = rule, Delay action

let ( => ) N rules = Production(N, rules)


let inline internal ToCommonRepresentation (Productions productions) =
    productions
    |> List.map (fun (Production(N, rules)) ->
            N.GetHashCode() => 
                List.map(fun (rule, action) ->
                    List.map (fun symbol ->
                        match symbol with
                        | Terminal (c,im,ir)    -> Terminal(c.GetHashCode(), im, ir)
                        | NonTerminal(N,ir) -> NonTerminal(N.GetHashCode(), ir)  
                    ) rule, action
                ) rules
            )
    |> Productions

let AddEndOfParse endmark (Productions productions) =
    match productions with
    | [] -> [ -1 => [[!(endmark)] >> fun args -> args.[0]]]
    | Production(N, _) :: _ ->
        (-1 => [[!!N] >> fun args -> args.[0]]) :: productions
    |> Productions
         
let AddStartOfRuleTag (Productions(productions)) =
    productions
    |> List.map (fun (Production(N, rules)) ->
            Production(
                N, 
                rules
                |> List.map 
                    (fun (rule, action) ->
                        match rule with
                        | (Terminal(c,_, i)) :: rule -> (Terminal(c, Set, i) :: rule, action) // add start tag to all rules
                        | (NonTerminal(N, i)) :: rule -> (NonTerminal(N, i) :: rule, action)
                        | _ -> (rule, action)
                    ) 
            )
    )
    |> Productions

// transformation to NFA
let rec internal MakeNFAOfRule (NFA nfa) state rule =
    match rule with
    | [] -> state + 1, NFA nfa
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
                List.fold (fun (NFA nfa) indentation ->
                    // check if there is a transition by nonterminal from state
                    match Map.tryFind (state, T (NonTerminal (nonterminal, indentation))) nfa with
                    | None -> NFA nfa // no tranistion 
                    | Some _ -> // a transition
                        // find all states that should have an epsilon transition
                        let _, startstates = List.find (fun (p, _) -> p = nonterminal) statesof
                        // for each of the above states "state'" make an epsilon transition from state to stat'
                        List.fold (fun (NFA nfa) state' -> 
                            match Map.tryFind (state, E) nfa with
                            | None -> 
                                Map.add (state, E) (set[state']) nfa 
                                |> NFA
                            | Some states -> 
                                Map.add (state, E) (Set.add state' states) nfa 
                                |> NFA
                        ) (NFA nfa) startstates
                ) (NFA nfa) [Less; Leq; Eq; Geq; Greater; All]
            ) (NFA nfa) (List.map (fun (Production(p, _)) -> p) productions)
        ) nfa [ 0 .. count ]
        |> fun nfa -> nfa, List.rev actions


    
let rec internal nullable acc last symbols =
    match acc, symbols with
    | _, [] | false, _ -> acc
    | _, Terminal _ :: _ -> false
    | _, NonTerminal (n, _) :: symbols -> nullable (Map.find n last) last symbols
    


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
    | (Terminal _ as a) :: _ -> Set.add a first
    | NonTerminal(n, _) :: line ->
        let f = Map.find n fst
        if Map.find n nullable then
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
let  internal GetFollowConstraintByFirst first follow productions =
    let rec perRule follow rule =
        match rule with
        | [] -> follow

        | (NonTerminal(N, _)) :: (Terminal(c, im, ir)) :: rule ->
            let fn = Map.find N follow
            perRule (Map.add N (Set.add (Terminal (c, im, ir)) fn) follow) rule


        

        // the Follow(M) in Follow(N) here is not added since it can be errorfull by non calculated set
        | NonTerminal (N, _) :: (NonTerminal (B, _) :: _ as rule) ->
            let fb = Map.find B first
            let fn = Map.find N follow
            perRule (Map.add N (fn + fb) follow) rule

        | _ :: rule -> perRule follow rule



    let perProduction follow (Production(_, rules)) =
        List.fold (fun follow (rule, _) -> perRule follow rule) follow rules

    let findAll follow (Productions productions) =
        List.fold (fun follow production -> perProduction follow production) follow productions
            
    findAll follow productions   


    
let  internal GetFollowConstraintByFollow nullable follow productions =
    let rec perRule M follow rule =
        match rule with
        | [] -> 
            follow
            
        | NonTerminal(N, _) :: [] ->
            let fm = Map.find M follow
            let fn = Map.find N follow
            Map.add N (fn + fm) follow
    

        | NonTerminal(N, _) :: ((NonTerminal(B, _) :: _) as rule) ->
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
    
    
let internal Follow endmark (Productions productions as p) =
    let nullable = Nullable p
    let first = First nullable p
    let AddEndpoint =
        match productions with
        | [] -> failwith "empty production"
        | Production(M,_) :: _ -> Map.add M (set[!(endmark)])
    
    List.fold (fun follow (Production(M, _)) -> Map.add M Set.empty follow) Map.empty productions
    |> AddEndpoint
    |> fun follow -> GetFollowConstraintByFirst first follow p
    |> fun follow -> GetFollowConstraintByFollow nullable follow p
    
   
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

            // the !! operator is might the wrong one.
            gl (Set.add (!!N) lang) productions
  
    gl Set.empty productions
    

let search (state, offset) lst =
    let rec innerloop lst = 
        match lst with
        | [] -> None
        | (x, entry) :: lst when x = offset -> Some entry
        | _ :: lst -> innerloop lst
        
    let rec outerloop lst =
        match lst with
        | [] -> None
        | (x, s) :: lst when x = state -> innerloop s
        | _ :: lst -> outerloop lst

    outerloop lst


let insert (state,offset) entry lst =
    let rec innerloop lst =
        match lst with
        | [] -> [offset, entry]
        | (x, _) as t :: lst when x < offset -> t :: innerloop lst
        | (x, _) :: _ when x > offset -> (offset, entry) :: lst
        | _ :: lst -> (offset, entry) :: lst

    let rec outerloop lst =
        match lst with
        | [] -> [state, [offset, entry]]
        | (x, _) as t :: lst when x < state -> t :: outerloop lst
        | (x, _) :: _ when x > state -> (state, [offset, entry]) :: lst
        | (x, s) :: lst -> (x, innerloop s) :: lst

    outerloop lst


type 't table =
    val mutable table : (int * (int * 't) list) list
    
    new(table) = { table = table }

    member P.Item
        with get(state, letter) = search (state, letter) P.table
        and set(state, letter) entry = P.table <- insert(state,letter) entry P.table

// both goto and table are O(n) lookup instead of O(1),
// this is do to the very sparse tables that most often are in parsers
// it is not uncommen to safe 50 - 95% of the space which makes the lookup faster
type ParseTable =
        val goto : int table
        val actions: (int * int * (int Token [] -> token)) []
        val table: Entry table // this safe a lot of space
        val endmark : int
        new(goto, actions, tab, endmark) = { goto = goto; actions = actions; table = tab; endmark = endmark }
        new(goto, actions, tab, endmark) = ParseTable(table goto, actions, table tab, endmark = endmark)
        
                
    
     
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


let internal ItemsToPop (Productions productions) actions =
    List.map (fun (Production(_, (rules : (Symbol<int,int> list * (Token<_> [] -> token))list))) -> List.map (fun (rule, _) -> List.length rule) rules) productions
    |> fun lst -> List.foldBack (fun lst l -> lst @ l) lst []
    |> fun items -> List.zip items actions
    |> List.toArray


let internal makeTable follow actions language goto dfa endmark =
    let terminals = 
        Set.filter (function Terminal _ -> true | _ -> false) language
        |> Set.map (function (Terminal(c, _, _)) -> c)

    for t in terminals do
        printfn "%d" t

    // assume error at first
    let table = ParseTable([], [||], [], endmark)
    let follow' = Map.map (fun _ item ->  Set.map (function Terminal(c, _, _) -> c) item) follow

    // map each state to an integer
    let states =
        dfa
        |> List.fold (fun (map, state) dstate -> 
            Map.add dstate state map, state+1
            ) (Map.empty, 0)
        |> fst

    // making accepting state set
    let acceptingstates = 
        List.map (fun (x, _, _, _) -> x) actions 
        |> Set.ofList
        |> fun s -> s - set[1]

    // will accept 
    table.table.[1,endmark] <- (Accept, Accept, Accept) // end of content symbol are declared as -1 at the moment
    
    let reduce p =
        function
        | Reduce m when m < p -> Reduce m
        | Accept -> Accept
        | _ -> Reduce p
   
  
    let SetIndentation i p (l,e,g) =
        match i with
        | Less      -> reduce p l, e, g
        | Leq       -> reduce p l, reduce p e, g
        | Eq        -> l, reduce p e, g
        | Geq       -> l, reduce p e, reduce p g
        | Greater   -> l, e, reduce p g
        | All       -> reduce p l, reduce p e, reduce p g

    let shiftreduce o n =
        match o with 
        | Reduce _ | Accept -> o
        | _ -> n


    for (source, symbol), destination in goto |> Map.toArray do
        let src, dst = Map.find source states, Map.find destination states
        let accept = Set.intersect source acceptingstates
        match symbol with
        | Terminal (c, i, ir) ->
            table.table.[src, c] <- 
                match table.table.[src, c] with
                | None -> 
                    match ir with
                    | Less -> Shift (dst, i), Error, Error
                    | Leq -> Shift (dst,i), Shift (dst,i), Error
                    | Eq -> Error, Shift (dst,i), Error
                    | Geq -> Error, Shift (dst,i), Shift (dst,i)
                    | Greater -> Error, Error, Shift (dst,i)
                    | All -> Shift (dst,i), Shift (dst,i), Shift (dst,i)

                | Some (l, e, g) -> 
                    match ir with
                    | Less -> shiftreduce l (Shift(dst, i)), e, g
                    | Leq -> shiftreduce l (Shift(dst,i)), shiftreduce e (Shift(dst,i)), g
                    | Eq -> l, shiftreduce e (Shift(dst,i)), g
                    | Geq -> l, shiftreduce e (Shift(dst,i)), shiftreduce g (Shift(dst,i))
                    | Greater -> l, e, shiftreduce g (Shift(dst,i))
                    | All -> shiftreduce l (Shift(dst,i)), shiftreduce e (Shift(dst,i)), shiftreduce g (Shift(dst,i))

        | NonTerminal(N, ir) ->
            let flw = Map.find N follow'
            match accept.Count with
            | 0 ->
                table.goto.[src, N] <- dst
            | _ ->
                table.goto.[src, N] <- dst

                let p = findaction accept.MaximumElement actions

                for c in flw do
                    match table.table.[src, c] with
                    | None -> table.table.[src, c] <- SetIndentation ir p (Error, Error, Error)
                    | Some entry -> table.table.[src, c] <- SetIndentation ir p entry



    dfa
    |> List.map (fun state -> Set.intersect state acceptingstates, Map.find state states)
    |> List.filter (fun (accept, _) -> not accept.IsEmpty)
    |> List.iter(fun (accept, src) ->
            let a = accept.MinimumElement 
            let p = findaction a actions
            let flw = findproduction a actions |> fun production -> Map.find production follow'
            for c in flw do
                match table.table.[src, c] with
                | None -> table.table.[src, c] <- (Reduce p, Reduce p, Reduce p)
                | Some entry -> 
                    table.table.[src, c] <- 
                        match table.table.[src,c] with
                        | None -> Reduce p, Reduce p, Reduce p
                        | Some entry -> SetIndentation All p entry
            )

    // return table
    table


let inline internal ISSLR (Productions productions as p) =       
    let endmark = 
        GetLanguage p 
        |> Set.filter (function Terminal _ -> true | _ -> false)
        |> Set.map (function (Terminal(c, _, _)) -> c)
        |> Set.filter (fun t -> t.ToString().ToLower() = "eof")
        |> Set.minElement
        |> fun e -> e.GetHashCode()

    let productions =
        p
        |> AddStartOfRuleTag
        |> ToCommonRepresentation
        //|> AddEndOfParse endmark

    let lang = GetLanguage productions
    let nfa, actions = MakeNFA productions
    let trans, dfa = MakeDFA nfa lang 
    let flw = Follow endmark productions
    let actions' = 
        List.map (fun (_, _, p, a) -> (p, a)) actions 
        |> ItemsToPop productions
        |> Array.map (fun (pops,(production, action)) -> (pops, production, action))

    let table = makeTable flw actions lang trans dfa endmark

    (table.goto, actions', table.table, endmark)


type Parser = 
    val private table: ParseTable

    new(table) = { table = table}

    member private P.Action state input = P.table.table.[state, input.GetHashCode()]
    member private P.Goto state input = P.table.goto.[state, input.GetHashCode()]

    member P.Parse input =
        
        for (state, trans) in P.table.table.table do
            printf "state %d:  " state
            for t in trans do
                printf "%A    " t
            printfn "\n"
        
        for (state, gotos) in P.table.goto.table do
            printf "state %d:  " state
            for t in gotos do
                printf "%A    " t
            printfn "\n"
        


        let input =
            Seq.map (fun (token : _ Token) -> Token(token.tp.GetHashCode(), token.value, token.pos)) input
            |> fun s -> seq { yield! s; Token(P.table.endmark, Arg null, start()) }
            |> List.ofSeq

        // pick right action dependent on the current indentation level
        let indent last next token =
            match token with
            | None -> None
            | Some (l,e,g) when last < next -> Some g
            | Some (l,e,g) when last > next -> Some l
            | Some (l,e,g) -> Some e
            
        let mutable stack = []
        let mutable states = [0]
        let mutable indentation = [0]
        let mutable current = input
        let mutable input = current.Head
        let mutable NoError = true
        let mutable err = ""

        while not states.IsEmpty && NoError do
            printfn "Token Type: %d" (TypeOf input)
            printf "Indentation: ["
            for i in indentation do
                printf " %2d," i
            printfn "]\n"
            printf "stack: ["
            for i in stack do
                printf " %2d," (TypeOf i)
            printfn "]\n"
            printf "states: ["
            for i in states do
                printf " %2d," i
            printfn "]\n"

            let i = ((PosOf input).Offset >>> 2)
            match indent indentation.Head i (P.Action states.Head (TypeOf input)) with
            // we move one symbol down the stream, but keep base indentation level
            | Some (Shift(n, Stay)) -> 
                stack <- input :: stack
                states <- n :: states 

                current <- List.tail current
                input <- Seq.head current
              
            // we move one symbol down the stream and increase the base indentation level since we have started a new case
            | Some (Shift(n, Set)) ->
                indentation <- i :: indentation // increase indentation to the level of the current token i.e. the start of a reduction.
                stack <- input :: stack  // push the token to on the stack
                states <- n :: states      // push the state on the stack

                // move one token along the stream.
                current <- List.tail current 
                input <- Seq.head current
             

            | Some(Reduce p) ->
                // fetch number element to pop, production number and reduction function 
                let pops, production, reduce = P.table.actions.[p]
                // get arguments in the right order
                let args = List.take pops stack |> List.rev |> List.toArray
                let value = reduce args

                
                stack  <- List.skip pops stack 
                
                states <- List.skip pops states

                indentation <- List.tail indentation

                match P.Goto states.Head production with
                | None -> 
                    err <- "ParseError no fitting reduction"
                    NoError <- false
                | Some state -> 
                    states <- state :: states
                
                let pos = 
                    match List.tryHead stack with
                    | None -> start()
                    | Some p -> PosOf p

                stack <- (Token(production, value, pos)) :: stack

            | Some Accept -> 
                // empty the stack of states
                states <- []

            | Some Error | _ ->
                let pos = PosOf input
                err <- "Parser Error at " + (TypeOf input).ToString() + " position " + string (pos.Line, pos.Offset)
                NoError <- false


        if NoError then
            try
                Ok(ValueOf stack.Head) // success
            with
                err ->
                    sprintf "Parsing error:\n%s at %s" err.Message (string (PosOf stack.[0]))
                    |> Result.Error    
        else 
            sprintf "Parsing error:\n%s at %s" err (string (PosOf stack.[0]))
            |> Result.Error
        
    static member ISS prod =
        ISSLR prod
        |> ParseTable
        |> Parser



