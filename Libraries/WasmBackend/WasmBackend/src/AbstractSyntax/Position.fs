module Position

[<Struct>]
type Position =
    val line: uint 
    val col: uint 
    val abs: uint
    new(?line, ?col, ?abs) = 
        let l = defaultArg line 0u
        let c = defaultArg col 0u
        let a = defaultArg abs 0u
        { line = l; col = c; abs = a }
    
    static member start = Position()

    override pos.ToString() = $"({pos.line}, {pos.col})" 


let Move (pos: Position) = Position(pos.line, pos.col + 1u, pos.abs + 1u)
let NewLine (pos: Position) = Position(pos.line + 1u, 0u, pos.abs + 1u)

let Update pos b =
    if b = byte '\n' then 
        NewLine pos
    else
        Move pos
    