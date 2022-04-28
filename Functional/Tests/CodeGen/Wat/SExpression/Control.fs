namespace Wat

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

open AbstractSyntax.Wasm
open Wasm.Values
open Wasm.Types

open Wat.Encoding



module Control =

    [<TestClass>]
    type Control() as test =
        [<TestMethod>]
        member _.Block() = 
            let actual = 
                [Block(ValBlockType (Some (NumType I32_t)),[Const(I32 1); Const(I32 1); Binary(I32 IntOp.Add,[])],[]) ]
                |> PrintAsSExpression

            let expected = "(block (result i32)\n  (i32.add\n    (i32.const 1)\n    (i32.const 1)\n  )\n)"
                
            expected = actual
            |> Assert.IsTrue
    

        [<TestMethod>]
        member _.Loop() = 
            let actual = 
                [Loop(ValBlockType (Some (NumType I32_t)),[Const(I32 1); Const(I32 1); Binary(I32 IntOp.Sub,[])],[]) ]
                |> PrintAsSExpression

            let expected = "(loop (result i32)\n  (i32.sub\n    (i32.const 1)\n    (i32.const 1)\n  )\n)"
                           
            expected = actual
            |> Assert.IsTrue
                
    
            expected = actual
            |> Assert.IsTrue
    

        [<TestMethod>]
        member _.If() =
            let actual = 
                [
                    [Const(I32 1); If(ValBlockType (Some (NumType F32_t)), [Const(F32 1f)],[], [])]
                    [Const(I32 0); If(ValBlockType (Some (NumType F32_t)), [Const(F32 1f); Const(F32 1f); Binary(F32 FloatOp.Sub,[])],[Const(F32 1f); Const(F32 1f); Binary(F32 FloatOp.Add,[])], [])]
                ]
                |> List.map PrintAsSExpression

            let expected = 
                [
                    "(i32.const 1)\n(if (result f32)\n  (then\n    (f32.const 1)\n  )\n)"
                    "(i32.const 0)\n(if (result f32)\n  (then\n    (f32.sub\n      (f32.const 1)\n      (f32.const 1)\n    )\n  )\n  (else\n    (f32.add\n      (f32.const 1)\n      (f32.const 1)\n    )\n  )\n)"
                    
                ]

    
            expected = actual
            |> Assert.IsTrue


        [<TestMethod>]
        member _.Select() =
            let actual = 
                [Const(F64 2.);Const(F64 3.);Const(I32 1); Select([],[])]
                |> PrintAsSExpression

            let expected = "(select \n  (i32.const 1)\n  (f64.const 2)\n  (f64.const 3)\n)"
            
            expected = actual
            |> Assert.IsTrue


        [<TestMethod>]
        member _.Test() = 
            test.Block()
            test.Loop()
            test.If()
            test.Select()