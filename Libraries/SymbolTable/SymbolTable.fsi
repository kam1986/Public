module SymbolTable

/// This is a simple library defining an interface for a general symboltable.
/// All implementations here are immutable, but there are no restriction on
/// behavour for user defined types using this interface.
type ISymbolic<'id,'item when 'id: equality and 'id: comparison> =
    abstract member Empty: unit -> ISymbolic<'id,'item> 
    abstract member Bind: 'id   -> 'item -> ISymbolic<'id,'item>
    abstract member LookUp: 'id -> Result<'item, string>
    abstract member Delete: 'id -> ISymbolic<'id,'item>


/// A function that bind the id to the item and return the updated symbol table
val Bind : ISymbolic<'id, 'item> -> 'id -> 'item -> ISymbolic<'id,'item>

/// A function that removes the last item binded to the id 
/// No error will occur if there are no binding of id
val Delete : ISymbolic<'id,'item> -> 'id -> ISymbolic<'id,'item>

// A function that returns the last item binded to the id
// or and error message
val LookUp : ISymbolic<'id,'item> -> 'id -> Result<'item, string>


/// A simple but effecient implementation of a symbol table when
/// the table are small and needs to be updated often.
[<Sealed>]
type SymbolList<'id,'item when 'id: equality and 'id: comparison> =
    interface ISymbolic<'id,'item>

    static member Empty: unit -> ISymbolic<'id,'item>


/// A simple but effecient implementation of a symbol table when
/// the table are big and need to favor lookups 
[<Sealed>]
type SymbolMap<'id,'item when 'id: equality and 'id: comparison> =
    interface ISymbolic<'id,'item>

    static member Empty: unit -> ISymbolic<'id,'item>

/// An effecient implementation of a symbol table which
/// changes underlying representation depending on size
/// to encounter changing needs.
[<Sealed>]
type DynamicTable<'id,'item when 'id: equality and 'id: comparison> =
    interface ISymbolic<'id,'item>

    static member Empty: unit -> ISymbolic<'id,'item>