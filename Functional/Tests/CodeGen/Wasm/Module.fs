module Wasm.Test.Module

open Microsoft.VisualStudio.TestTools.UnitTesting

open AbstractSyntax.Wasm
open Wasm.Values
open Wasm.Types
open Wasm.Encode

[<TestClass>]
type ModuleToBinary() as test = 
    
    [<TestMethod>]
    member _.Expression() = 
        let actual = 
            Expr [Nop[]]
            |> GenExpr

        let expected = GenInstruction (Nop[]) => End

        expected = actual
        |> Assert.IsTrue


    [<TestMethod>]
    member _.FuncType() =
        let actual = 
            [{ ty = 1; locals = [0, NumType I32_t]; body = Expr[Nop[]] }]
            |> GenVec (fun f -> Idx f.ty) 

        let expected = u32 1 => Idx 1

        expected = actual
        |> Assert.IsTrue


    [<TestMethod>]
    member _.Element() = 
        let actual = 
            [
                { ty = FuncRef; init = [Expr [RefFunc(0,[])]]; mode = elemmode.Active {|index = 0; offset = Expr[]|} }
                { ty = FuncRef; init = [Expr [RefFunc(0,[])]]; mode = elemmode.Passive }
                { ty = FuncRef; init = [Expr [RefFunc(0,[])]]; mode = elemmode.Active {|index = 5; offset = Expr[]|} }
                { ty = FuncRef; init = [Expr [RefFunc(0,[])]]; mode = elemmode.Declarative }
                { ty = FuncRef; init = [Expr [RefFunc(0,[]); Nop[]]]; mode = elemmode.Active {|index = 0; offset = Expr[Nop[]]|} }
                { ty = ExternRef; init = [Expr [RefFunc(0,[]); Nop[]]]; mode = elemmode.Passive }
                { ty = ExternRef; init = [Expr [RefFunc(0,[]); Nop[]]]; mode = elemmode.Active {|index = 2; offset = Expr[]|}}
                { ty = ExternRef; init = [Expr [RefFunc(0,[]); Nop[]]]; mode = elemmode.Declarative }
            ]
            |> List.map GenElement

        let expected = 
            [
                &0x00 => GenExpr (Expr[]) => GenVec Idx [0]
                &0x01 => GenRefType FuncRef => elmkind => GenVec Idx [0]
                &0x02 => Idx 5 => GenExpr (Expr[]) => GenRefType FuncRef => elmkind => GenVec Idx [0]
                &0x03 => GenRefType FuncRef => elmkind => GenVec Idx [0]
                &0x04 => GenExpr (Expr[Nop[]]) => GenVec GenExpr [Expr [RefFunc(0,[]); Nop[]]]
                &0x05 => GenRefType ExternRef => GenVec GenExpr [Expr [RefFunc(0,[]); Nop[]]]
                &0x06 => Idx 2 => GenExpr (Expr[]) => GenRefType ExternRef => GenVec GenExpr [Expr [RefFunc(0,[]); Nop[]]]
                &0x07 => GenRefType ExternRef => GenVec GenExpr [Expr [RefFunc(0,[]); Nop[]]]
            ]

        expected = actual
        |> Assert.IsTrue


    [<TestMethod>]
    member _.Data() =
        let b = [|0uy;1uy;3uy;4uy;5uy|]
        let actual = 
            [
                { init = b; mode = Active {| memory = 0; offset = Expr [Nop[]]|} }
                { init = b; mode = Passive }
                { init = b; mode = Active {| memory = 2; offset = Expr [Nop[]]|} }
            ]
            |> List.map GenData

        let expected = 
            [
                &0x00 => GenExpr (Expr [Nop[]]) => u32 b.Length => b
                &0x01 => u32 b.Length => b
                &0x02 => Idx 2 => GenExpr (Expr [Nop[]]) => u32 b.Length => b
            ]


        expected = actual
        |> Assert.IsTrue


    [<TestMethod>]
    member _.Start() =
        let actual = 
            [
                None
                Some 2
            ]
            |> List.map (function None -> seq[] | Some idx -> Idx idx)

        let expected = 
            [
                seq[]
                Idx 2
            ]

        expected = actual
        |> Assert.IsTrue


    [<TestMethod>]
    member _.Code() =
        let actual = 
            { ty = 1; locals = [0, NumType I32_t]; body = Expr[Nop[]] }
            |> GenFunc

        let expected = 
            let func = GenVec (fun (loc, ty) -> u32 loc => GenValueType ty) [0, NumType I32_t; 1, NumType F32_t] => GenExpr (Expr[Nop[]])
            
            u32 (Seq.length func) => func

        expected = actual
        |> Assert.IsTrue


    [<TestMethod>]
    member _.Import() =
        let actual = []
        let expected = []

        expected = actual
        |> Assert.IsTrue


    [<TestMethod>]
    member _.Export() = 
        let actual = 
            [
                { modulename = "module"; name = "name"; desc = ImFunc 2}
                { modulename = "module"; name = "name"; desc = ImTable (TableType({min = 50; max = None; share = Shared}, FuncRef))}
                { modulename = "module"; name = "name"; desc = ImMemory (MemoryType {min = 50; max = None; share = Shared})}
                { modulename = "module"; name = "name"; desc = ImGlobal (GlobalType(NumType I64_t, Immutable))}
            ]
            |> List.map GenImport

        let expected = 
            [
                GenName "module" => GenName "name" => &0x00 => Idx 2
                GenName "module" => GenName "name" => &0x01 => GenTableType (TableType({min = 50; max = None; share = Shared}, FuncRef))
                GenName "module" => GenName "name" => &0x02 => GenMemType (MemoryType {min = 50; max = None; share = Shared})
                GenName "module" => GenName "name" => &0x03 => GenGlobalType (GlobalType(NumType I64_t, Immutable))
            ]

        expected = actual
        |> Assert.IsTrue

    [<TestMethod>]
    member _.Section() = 
        let actual = 
            [
                [
                    { modulename = "module"; name = "name"; desc = ImFunc 2}
                    { modulename = "module"; name = "name"; desc = ImTable (TableType({min = 50; max = None; share = Shared}, FuncRef))}
                    { modulename = "module"; name = "name"; desc = ImMemory (MemoryType {min = 50; max = None; share = Shared})}
                    { modulename = "module"; name = "name"; desc = ImGlobal (GlobalType(NumType I64_t, Immutable))}
                ]
                []
            ]
            |> List.map ImportSection

        let expected =
            let content =
                u32 4 
                => GenName "module" => GenName "name" => &0x00 => Idx 2
                => GenName "module" => GenName "name" => &0x01 => GenTableType (TableType({min = 50; max = None; share = Shared}, FuncRef))
                => GenName "module" => GenName "name" => &0x02 => GenMemType (MemoryType {min = 50; max = None; share = Shared})
                => GenName "module" => GenName "name" => &0x03 => GenGlobalType (GlobalType(NumType I64_t, Immutable))
            [
                &0x02 => u32 (Seq.length content) => content
                seq[]
            ]

        expected = actual
        |> Assert.IsTrue


    [<TestMethod>]
    member _.Module() = 
        let actual = (Module<_,_,_,_>.empty(): Module<int, int, Location<int, int>, unit>) |> GenModule
        let expected = magic => version
       

        actual = expected
        |> Assert.IsTrue


    [<TestMethod>]
    member _.Test() = 
        test.Expression()
        test.FuncType()
        test.Element()
        test.Data()
        test.Start()
        test.Import()
        test.Export()
        test.Section()
        test.Module()