module CompilerInfo

type Location =
    | Accummulator // stack
    | Local
    | Global
    | Memory




type Information<'Position, 'Location, 'Type> =
    {
        Position: 'Position         // location in the file
        Location: 'Location         // location in runtime
        LastUse: bool               // Compiler info for last usage
        Type: 'Type option          // Type information
    }

with
    static member Info pos = 
        {
            Position = pos
            Location = ()
            LastUse = false
            Type = None
        }
