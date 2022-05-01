module Position



type Position = 
    {
        mutable Line : int 
        mutable Offset : int 
        mutable Absolut : int
    }
with
        override P.ToString() = "(" + string P.Line + ", " + string P.Offset + ")"
        
let Position(line, offset, absolut) = {
    Line = line
    Offset = offset
    Absolut = absolut
    }

let start() = Position(0,0,0)

let move pos steps = 
        pos.Offset <- pos.Offset + steps 
        pos.Absolut <- pos.Absolut + steps

       
let next pos = move pos 1


let newline (pos : byref<Position>) = 
    pos.Absolut <- pos.Absolut + 1
    pos.Line <- pos.Line + 1
    pos.Offset <- 0
 
    
let Line (pos : Position) = pos.Line
let Offset (pos : Position) = pos.Offset
let Indentation (pos : Position) = pos.Offset >>> 2 // low overhead computation, most likely only computed once pr. position
let Absolut (pos : Position) = pos.Absolut

let Copy (pos: Position) = Position(Line pos, Offset pos, Absolut pos)
