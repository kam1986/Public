module Jumptable

open TypeAcrobatics

(*
    This is an updated version of the lexer jumptable
    it should enhance the performance by slightly
    it is meant for bytewise parsing, it is fix sized and it is the maximum size
    a lexer parse table can be when it parsing at the byte level.

    We choose byte level lexing, because it is capable of lexing the same as any greater bit length data point, but in contrast it
    will be it can be held in cache

    We could choose to minimize space usage, but this will come with a performance hit and with modern computers the size reduction is neglible.
*)

[<Struct>]
type internal 'token JumpTable =
    val private table: byte []               // 256 * 256   = 65536
    val private acceptanceStates: ('token * (string -> token)) option[]  // 8 * 4       = 36
    val private transitionGraph: uint64 []   // 256 * 4 * 8 = 8192
    
    new(_) =
        {
            table            = [| for _ in 1 .. 256 * 256 -> 0uy |]
            acceptanceStates = [| for _ in 1 .. 256       -> None |]
            transitionGraph  = [| for _ in 1 .. 4 * 256   -> 0UL |]
        }
    
    // each state has 256 (4 * 64) possible transistion, hence each states legal transitions can be represented by 256 bits 
    member private JT.Transition(state, offset) =
        let i = offset >>> 6 // find the correct int64 offset 
        let j = offset % 64  // find the correct bit offset
        let t = JT.transitionGraph.[4 * state + i] &&& (1UL <<< j)
        t > 0UL

    member JT.Accept  
        with get(state) = JT.acceptanceStates.[state] 

        // the value is irrelevant
        and set state f = JT.acceptanceStates.[state] <- f
            

    member JT.Item
        with get(state, offset) = 
            if JT.Transition(state, offset) then
                Some JT.table.[256 * state + offset]
            else
                None

        and set (state, offset) value = 
            let i = offset >>> 6 // find the correct int64 offset 
            let j = offset % 64  // find the correct bit offset
            // make the transition legal
            JT.transitionGraph.[4 * state + i] <- JT.transitionGraph.[4 * state + i] ||| (1UL <<< j) 
            // set transition
            JT.table.[256 * state + offset] <- value


