namespace C.Test

module Operation =


    open System
    open Microsoft.VisualStudio.TestTools.UnitTesting

    open Helpers
    open SymTab
    open AbstractSyntax.C
    open AbstractSyntax.Wasm
    open Wasm.Types
    open Wasm.Values
    open CodeGen.C

    [<TestClass>]
    type COpToWasmOp() as Test = 
    
        [<TestMethod>]
        member private _.GetIntOperand() =
            // since the GetInOperand generates functions
            // we need to give each an argument to test
            let actual = 
                [
                    Int8_t   
                    Int16_t  
                    Int32_t  
                    Int64_t  
                    UInt8_t  
                    UInt16_t 
                    UInt32_t 
                    UInt64_t 
                    Float32_t
                    Float64_t
                ]
                |> List.map GetOperand
                |> List.map (function Integer op | Float op -> op 1)

            let expected =
                [
                    I32 1
                    I32 1
                    I32 1
                    I64 1
                    U32 1
                    U32 1
                    U32 1
                    U64 1
                    F32 1
                    F64 1
                ]

            expected = actual
            |> Assert.IsTrue

        [<TestMethod>]
        member private _.GenIntBinOp() =
            let actual = 
                [
                    binOp.Add        
                    binOp.Sub        
                    binOp.Div        
                    binOp.Mul        
                    binOp.Rem        
                    binOp.And        
                    binOp.Or         
                    binOp.Xor        
                    binOp.Rotl       
                    binOp.Rotr       
                    binOp.LeftShift  
                    binOp.RightShift 
                ]
                |> List.map GenIntBinOp

            let expected = 
                [
                  IntOp.binop.Add
                  IntOp.binop.Sub
                  IntOp.binop.Div
                  IntOp.binop.Mul
                  IntOp.binop.Rem
                  IntOp.binop.And
                  IntOp.binop.Or
                  IntOp.binop.Xor
                  IntOp.binop.Rotl
                  IntOp.binop.Rotr
                  IntOp.binop.Shl 
                  IntOp.binop.Shr
                ]
                |> List.map Ok

            expected = actual
            |> Assert.IsTrue


            GenIntBinOp binOp.Max
            |> function Error _ -> true | Ok _ -> false
            |> Assert.IsTrue

        [<TestMethod>]
        member _.GenFloatBinOp() =
            let actual =
                [
                    binOp.Add      
                    binOp.Sub      
                    binOp.Div      
                    binOp.Mul      
                    binOp.Min      
                    binOp.Max      
                    binOp.CopySign 
                ]
                |> List.map GenFloatBinOp

            let expected =
                [
                    FloatOp.binop.Add
                    FloatOp.binop.Sub
                    FloatOp.binop.Div
                    FloatOp.binop.Mul
                    FloatOp.binop.Min
                    FloatOp.binop.Max
                    FloatOp.binop.CopySign
                ]
                |> List.map Ok
        
            expected = actual
            |> Assert.IsTrue


            GenFloatBinOp binOp.LeftShift
            |> function Error _ -> true | Ok _ -> false
            |> Assert.IsTrue

        [<TestMethod>]
        member private _.GenIntRelOp() =
            let actual = 
                [
                    Eq      
                    Nq      
                    Greater 
                    Geq     
                    Less    
                    Leq     
                ]
                |> List.map GenIntRelOp

            let expected = 
                [
                    IntOp.relop.Eq
                    IntOp.relop.Ne
                    IntOp.relop.Gt
                    IntOp.relop.Ge
                    IntOp.relop.Lt
                    IntOp.relop.Le
                ]
                |> List.map Ok

            expected = actual
            |> Assert.IsTrue

        [<TestMethod>]
        member private _.GenFloatRelOp() =
            let actual = 
                [
                    Eq      
                    Nq      
                    Greater 
                    Geq     
                    Less    
                    Leq     
                ]
                |> List.map GenFloatRelOp

            let expected = 
                [
                    FloatOp.relop.Eq
                    FloatOp.relop.Ne
                    FloatOp.relop.Gt
                    FloatOp.relop.Ge
                    FloatOp.relop.Lt
                    FloatOp.relop.Le
                ]
                |> List.map Ok

            expected = actual
            |> Assert.IsTrue

        [<TestMethod>]
        member private _.GenFloatUnOp() =
            let actual = 
                [
                    Neg    
                    Abs    
                    Ceil   
                    Floor  
                    Trunc  
                    Nearest
                    Sqrt     
                ]
                |> List.map GenFloatUnOp

            let expected = 
                [
                    FloatOp.unop.Neg
                    FloatOp.unop.Abs
                    FloatOp.unop.Ceil
                    FloatOp.unop.Floor
                    FloatOp.unop.Trunc
                    FloatOp.unop.Nearest
                    FloatOp.unop.Sqrt
                ]
                |> List.map Ok

            expected = actual
            |> Assert.IsTrue


        [<TestMethod>]
        member _.Test() = 
            Test.GetIntOperand()
            Test.GenIntBinOp()
            Test.GenFloatBinOp()
            Test.GenIntRelOp()
            Test.GenFloatRelOp()
            Test.GenFloatUnOp()

