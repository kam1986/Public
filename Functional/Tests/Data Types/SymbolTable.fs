(*

    This is the test class for the data type SymbolTable

    There is no mutable state in it and therefor we
    only need to test base cases (whitebox testing), 
    which simplifies the testing quit a lot.

    The map, filter, filterByID, filterByValue, fold and foldBack 
    are all implemented with simular function for the underlying type
    and we assume that they work correctly since the map, filter, fold and foldBack
    functions for lists are well tested them self
*)

module SymbolTableTests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

open SymTab

[<TestClass>]
type SymbolTable () as Test =
    
    let empty() = DynamicTable<_,_>.Empty() :> ISymbolic<_,_>

    [<TestMethod>]
    member _.Creation() =
        let expected = Table(0, SymTable []) :> ISymbolic<_,_>

        let actual = empty()
        
        expected = actual
        |> Assert.IsTrue


    [<TestMethod>]
    member _.Bind() =
        let actual = Bind (empty()) "x" 1

        let expected = Table(1, SymTable ["x", 1]) :> ISymbolic<_,_>

        expected = actual
        |> Assert.IsTrue


    [<TestMethod>]
    member _.Delete() =
        let bindings = 
            // row: (name, value) 
            [
                "y", 5
                "x", 1
                "r", 3
                "x", 42
            ]

        // stay in same order as above
        let tab = List.foldBack (fun (name, value) tab -> Bind tab name value) bindings (empty())

        let expected1 = 
            let (Table(n, tab)) = tab :?> _
            let (SymTable lst) = tab :?> _
            (Table(n-1, SymTable (lst.Head :: lst.Tail.Tail))) :> ISymbolic<_,_>

        let actual1 = Delete tab "x" // remove first x in the table
        let actual2 = Delete tab "i" // should not remove anything
        
        expected1 = actual1 
        |> Assert.IsTrue

        tab = actual2
        |> Assert.IsTrue
    
    [<TestMethod>]
    member _.TestLookUp() =
        let bindings = 
            [
                "y", 5
                "x", 1
                "r", 3
                "x", 42
            ]

        let tab = List.fold (fun tab (name, value) -> Bind tab name value) (empty()) bindings

        let actual1 = LookUp tab "r"
        let expected1 = Ok(3)

        let actual2 = LookUp tab "u"
        let expected2 = Error "Look up error:\n\t the id u is not defined\n"
       
        expected1 = actual1
        |> Assert.IsTrue

        expected2 = actual2
        |> Assert.IsTrue


    [<TestMethod>]
    member _.Test () =
        
        Test.Creation()
        Test.Bind()
        Test.Delete()
        Test.TestLookUp()
