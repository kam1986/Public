module Unique


// a type that implement uniqueness
type unique private (id) =
    // static variable that increment every time the Unique are instanciated
    static let mutable count = 0UL


    let id = id
    
    member private _.ID = id

    new () =
        let id = count
        count <- count + 1UL
        unique(id)

    static member op_Equality (u1 : unique, u2: unique) = u1.ID = u2.ID

    override _.ToString() = $"ID:{id}"