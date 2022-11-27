module Regex

#nowarn "25"

// 
let ASCII = set[0uy .. 127uy]
let All = set[0uy .. 254uy]
type regex =
    | Epsilon
    | Terminal of int
    | Atom of byte * int
    | Cat of regex * regex * int Set * int Set * bool
    | Or of regex * regex * int Set * int Set * bool 
    | Star of regex * int Set * int Set

// Construction functions

let IsNullAble reg =
    match reg with
    | Epsilon | Star _ -> true
    | Atom _ | Terminal _ -> false
    | Cat (_, _, _, _, n) | Or (_, _, _, _, n) -> n


let FirstPosOf reg =
    match reg with
    | Epsilon -> set[]
    | Atom (_, i) | Terminal i -> set[i]
    | Cat (_, _, fp, _, _) | Or (_, _, fp, _, _) | Star (_, fp, _) -> fp


let LastPosOf reg =
    match reg with
    | Epsilon -> set[]
    | Atom (_, i) | Terminal i -> set[i]
    | Cat (_, _, _, lp, _) | Or (_ , _, _, lp, _) | Star (_, _, lp) -> lp

let rec GetTerminals regex =
    match regex with
    | regex.Terminal i -> Set[i]
    | regex.Atom _ |Epsilon -> set[]
    | regex.Star(regex, _, _) -> GetTerminals regex
    | regex.Or(regex1, regex2, _ ,_, _) | regex.Cat(regex1, regex2, _ ,_, _) ->
        GetTerminals regex1 + GetTerminals regex2


let rec Followpos regex fp =
    match regex with
    | Cat(c1, c2, _, _, _) -> 
        Followpos c1 fp // compute followpos for the left side subtree
        |> Followpos c2 // compute followpos for the right side subtree
        |> Set.foldBack ( // compute followpos for this node
            fun i fp ->
                match Map.tryFind i fp with
                | None -> Map.add i (FirstPosOf c2) fp
                | Some f -> Map.add i (f + FirstPosOf c2) fp
        ) (LastPosOf c1)

    | Star(n, f, l) ->
        Followpos n fp // compute followpos for the subtree
        |> Set.foldBack ( // compute followpos for this node
            fun i fp ->
                match Map.tryFind i fp with
                | None -> 
                   Map.add i f fp
                | Some f' -> Map.add i (f' + f) fp
        ) l
    | Or(c1, c2, _, _, _) ->
        Followpos c1 fp // compute followpos for the left side subtree
        |> Followpos c2 // compute followpos for the right side subtree

    | _ -> fp // leaf cases 





// easy wrappers to hide the ugly stuff
let Cat reg1 reg2 =
    let fp = 
        if reg1 |> IsNullAble 
        then FirstPosOf reg1 + FirstPosOf reg2
        else FirstPosOf reg1
    
    let lp =
        if reg2 |> IsNullAble 
        then LastPosOf reg1 + LastPosOf reg2
        else LastPosOf reg2

    let n = IsNullAble reg1 && IsNullAble reg2

    Cat(reg1, reg2, fp, lp, n)

let Or reg1 reg2 =
    let fp = FirstPosOf reg1 + FirstPosOf reg2
    let lp = LastPosOf reg1 + LastPosOf reg2
    let n = IsNullAble reg1 || IsNullAble reg2

    Or(reg1, reg2, fp, lp, n)



let Star reg = Star(reg, FirstPosOf reg, LastPosOf reg) 

// do not need to rename the atoms ID
let Plus reg = Cat reg <| Star reg
