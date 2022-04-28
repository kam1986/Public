module Wasm.Test.ValueOperation

open Microsoft.VisualStudio.TestTools.UnitTesting

open AbstractSyntax.Wasm
open Wasm.Values
open Wasm.Types
open Wasm.Encode



[<TestClass>]
type ValueInstructionToBinary() as test =
    
    let genbinop operand lst =
        List.map (fun op -> Binary(operand op, ())) lst
        |> List.map GenInstruction
    
    let genunop operand lst =
        List.map (fun op -> Unary(operand op, ())) lst
        |> List.map GenInstruction

    let genrelop operand lst =
        List.map (fun op -> Compare(operand op, ())) lst
        |> List.map GenInstruction

    let gencvtop operand lst =
        List.map (fun op -> Convert(operand op, ())) lst
        |> List.map GenInstruction

    let genspcop operand lst =
        List.map (fun op -> Spec(operand op, ())) lst
        |> List.map GenInstruction


    let gentestop operand lst =
        List.map (fun op -> Test(operand op, ())) lst
        |> List.map GenInstruction

    [<TestMethod>]
    member _.IntBinary() = 

        let operations =
            [
                IntOp.binop.Add
                IntOp.binop.Sub
                IntOp.binop.Mul
                IntOp.binop.Div
                IntOp.binop.Rem
                IntOp.binop.And
                IntOp.binop.Or
                IntOp.binop.Xor
                IntOp.binop.Shl
                IntOp.binop.Shr
                IntOp.binop.Rotl
                IntOp.binop.Rotr 
            ]
         
        let actualI32 = genbinop I32 operations
        let actualI64 = genbinop I64 operations
        let actualU32 = genbinop U32 operations
        let actualU64 = genbinop U64 operations

        let expectedI32 = 
            [
                &0x6A
                &0x6B
                &0x6C
                &0x6D
                &0x6F
                &0x71
                &0x72
                &0x73
                &0x74
                &0x75
                &0x77
                &0x78
            ]

        let expectedI64 =
            [
               &0x7C
               &0x7D
               &0x7E
               &0x7F
               &0x81
               &0x83
               &0x84
               &0x85
               &0x86
               &0x87
               &0x89
               &0x8A 
            ]

        let expectedU32 = 
            [
                &0x6A
                &0x6B
                &0x6C
                &0x6E
                &0x70
                &0x71
                &0x72
                &0x73
                &0x74
                &0x76
                &0x77
                &0x78
            ]
        
        let expectedU64 = 
            [
                &0x7C
                &0x7D
                &0x7E
                &0x80
                &0x82
                &0x83
                &0x84
                &0x85
                &0x86
                &0x88
                &0x89
                &0x8A
            ]

        expectedI32 = actualI32
        |> Assert.IsTrue

        expectedI64 = actualI64
        |> Assert.IsTrue
        
        expectedU32 = actualU32
        |> Assert.IsTrue

        expectedU64 = actualU64
        |> Assert.IsTrue
    

    [<TestMethod>]
    member _.FloatBinary() =
        let operations =
            [ 
                FloatOp.binop.Add 
                FloatOp.binop.Sub
                FloatOp.binop.Mul
                FloatOp.binop.Div
                FloatOp.binop.Min
                FloatOp.binop.Max
                FloatOp.binop.CopySign 
            ]
        
        let actualF32 = genbinop F32 operations
        let actualF64 = genbinop F64 operations


        let expectedF32 = 
            [
                &0x92
                &0x93
                &0x94
                &0x95
                &0x96
                &0x97
                &0x98
            ]

        let expectedF64 =
            [
                &0xA0
                &0xA1
                &0xA2
                &0xA3
                &0xA4
                &0xA5
                &0xA6  
            ]

        expectedF32 = actualF32
        |> Assert.IsTrue

        expectedF64 = actualF64
        |> Assert.IsTrue


    [<TestMethod>]
    member _.IntUnary() = 
        let operations =
            [
                IntOp.unop.Clz   
                IntOp.unop.Ctz   
                IntOp.unop.Popcnt
            ]
        
        let actualI32 = genunop I32 operations
        let actualI64 = genunop I64 operations
        let actualU32 = genunop U32 operations
        let actualU64 = genunop U64 operations
        
        let expected32 = 
            [
                &0x67 
                &0x68
                &0x69
            ]

        let expected64 =
            [
                &0x79 
                &0x7A
                &0x7B
            ]

        expected32 = actualI32
        |> Assert.IsTrue

        expected32 = actualU32
        |> Assert.IsTrue

        expected64 = actualI64
        |> Assert.IsTrue

        expected64 = actualU64
        |> Assert.IsTrue


    [<TestMethod>]
    member _.FloatUnary() =
        let operations =
            [
                FloatOp.unop.Abs
                FloatOp.unop.Neg
                FloatOp.unop.Ceil
                FloatOp.unop.Floor
                FloatOp.unop.Trunc
                FloatOp.unop.Nearest
                FloatOp.unop.Sqrt
            ]
        
        let actualF32 = genunop F32 operations
        let actualF64 = genunop F64 operations

        let expectedF32 =
            [
                &0x8B
                &0x8C
                &0x8D
                &0x8E
                &0x8F
                &0x90
                &0x91
            ]

        let expectedF64 =
            [
                &0x99
                &0x9A
                &0x9B
                &0x9C
                &0x9D
                &0x9E
                &0x9F
            ]
        

        expectedF32 = actualF32
        |> Assert.IsTrue

        expectedF64 = actualF64
        |> Assert.IsTrue

    [<TestMethod>]
    member _.IntRelation() =
        let operations =
            [
                IntOp.relop.Eq
                IntOp.relop.Ne
                IntOp.relop.Lt
                IntOp.relop.Gt
                IntOp.relop.Le
                IntOp.relop.Ge
            ]
        
        let actuali32 = genrelop I32 operations
        let actualu32 = genrelop U32 operations
        let actuali64 = genrelop I64 operations
        let actualu64 = genrelop U64 operations

        let expectedi32 = 
            [
                &0x46
                &0x47
                &0x48
                &0x4A
                &0x4C
                &0x4E
            ]

        let expectedu32 =
            [
                &0x46
                &0x47
                &0x49
                &0x4B
                &0x4D
                &0x4F
            ]

        let expectedi64 =
            [
                &0x51
                &0x52
                &0x53
                &0x55
                &0x57
                &0x59 
            ]

        let expectedu64 =
            [
                &0x51
                &0x52
                &0x54
                &0x56
                &0x58
                &0x5A  
            ]

        expectedi32 = actuali32
        |> Assert.IsTrue

        expectedu32 = actualu32
        |> Assert.IsTrue

        expectedi64 = actuali64
        |> Assert.IsTrue

        expectedu64 = actualu64
        |> Assert.IsTrue

    [<TestMethod>]
    member _.FloatRelation() =

        let operations =
            [
                FloatOp.relop.Eq
                FloatOp.relop.Ne
                FloatOp.relop.Lt
                FloatOp.relop.Gt
                FloatOp.relop.Le
                FloatOp.relop.Ge
            ]

        let actualf32 = genrelop F32 operations
        let actualf64 = genrelop F64 operations

        let expectedf32 =
            [
                &0x5B
                &0x5C
                &0x5D
                &0x5E
                &0x5F
                &0x60
            ]

        let expectedf64 =
            [
                &0x61
                &0x62
                &0x63
                &0x64
                &0x65
                &0x66
            ]
        
        expectedf32 = actualf32
        |> Assert.IsTrue

        expectedf64 = actualf64
        |> Assert.IsTrue

    [<TestMethod>]
    member _.IntConvert() = 
        let operations32 =
            [
                IntOp.cvtop.WrapI64 
                IntOp.cvtop.TruncF32
                IntOp.cvtop.TruncF64
            ]

        let operations64 =
            [
                IntOp.cvtop.Extend  
                IntOp.cvtop.TruncF32
                IntOp.cvtop.TruncF64
            ]

        let actuali32 = gencvtop I32 operations32
        let actualu32 = gencvtop U32 operations32
        let actuali64 = gencvtop I64 operations64
        let actualu64 = gencvtop U64 operations64

        let expectedi32 =
            [
                &0xA7
                &0xA8
                &0xAA
            ]


        let expectedu32 =
            [
                &0xA7
                &0xA9
                &0xAB
            ]


        let expectedi64 =
            [
                &0xAC
                &0xAE
                &0xB0
            ]


        let expectedu64 =
            [
                &0xAD
                &0xAF
                &0xB1
            ]

        expectedi32 = actuali32
        |> Assert.IsTrue

        expectedu32 = actualu32
        |> Assert.IsTrue

        expectedi64 = actuali64
        |> Assert.IsTrue

        expectedu64 = actualu64
        |> Assert.IsTrue

    [<TestMethod>]
    member _.FloatConvert() =
        let operations32 =
            [ 
                FloatOp.cvtop.ConvertSI32 
                FloatOp.cvtop.ConvertUI32 
                FloatOp.cvtop.ConvertSI64 
                FloatOp.cvtop.ConvertUI64 
                FloatOp.cvtop.DemoteF64   
            ]

        let operations64 =
            [
                FloatOp.cvtop.ConvertSI32 
                FloatOp.cvtop.ConvertUI32 
                FloatOp.cvtop.ConvertSI64 
                FloatOp.cvtop.ConvertUI64 
                FloatOp.cvtop.PromoteF32  
            ]

        let actualf32 = gencvtop F32 operations32
        let actualf64 = gencvtop F64 operations64

        let expectedf32 =
            [
                &0XB2
                &0XB3
                &0XB4
                &0XB5
                &0XB6
            ]

        let expectedf64 =
            [
                &0XB7
                &0XB8
                &0XB9
                &0XBA
                &0XBB
            ]
        
        expectedf32 = actualf32
        |> Assert.IsTrue

        expectedf64 = actualf64
        |> Assert.IsTrue

    [<TestMethod>]
    member _.IntSpecial() =
        let operations32 =
            [
                IntOp.spec.ExtendS Pack8
                IntOp.spec.ExtendS Pack16
                IntOp.spec.TruncSatF32
                IntOp.spec.TruncSatF64
                IntOp.spec.Reinterpret
            ]

        let operations64 =
            [
                IntOp.spec.ExtendS Pack8
                IntOp.spec.ExtendS Pack16
                IntOp.spec.ExtendS Pack32
                IntOp.spec.TruncSatF32
                IntOp.spec.TruncSatF64
                IntOp.spec.Reinterpret
            ]

        let actuali32 = genspcop I32 operations32
        let actualu32 = genspcop U32 operations32
        let actuali64 = genspcop I64 operations64
        let actualu64 = genspcop U64 operations64
        
        let expectedi32 =
            [
                &0xC0
                &0xC1
                &0xFC => u32 0u
                &0xFC => u32 2u
                &0xBC
            ]

        let expectedu32 =
            [
                &0xC0
                &0xC1
                &0xFC => u32 1u
                &0xFC => u32 3u
                &0xBC
            ]

        let expectedi64 =
            [
                &0xC2
                &0xC3
                &0xC4
                &0xFC => u32 4u
                &0xFC => u32 6u
                &0xBD
            ]

        let expectedu64 =
            [
                &0xC2
                &0xC3
                &0xC4
                &0xFC => u32 5u
                &0xFC => u32 7u
                &0xBD
            ]


        expectedi32 = actuali32
        |> Assert.IsTrue

        expectedu32 = actualu32
        |> Assert.IsTrue

        expectedi64 = actuali64
        |> Assert.IsTrue

        expectedu64 = actualu64
        |> Assert.IsTrue
        
    [<TestMethod>]
    member _.FloatSpecial() =
        let operations =
            [
                FloatOp.spec.Reinterpret
            ]

        let actualf32 = genspcop F32 operations
        let actualf64 = genspcop F64 operations

        let expectedf32 = [&0xBE]
        let expectedf64 = [&0xBF]

        expectedf32 = actualf32
        |> Assert.IsTrue

        expectedf64 = actualf64
        |> Assert.IsTrue

    [<TestMethod>]
    member _.IntTest() =
        let operations =
            [
                IntOp.testop.Eqz
            ]

        let actuali32 = gentestop I32 operations
        let actualu32 = gentestop U32 operations
        let actuali64 = gentestop I64 operations
        let actualu64 = gentestop U64 operations

        let expected32 = [&0x45]
        let expected64 = [&0x50]
        
        expected32 = actuali32
        |> Assert.IsTrue

        expected32 = actualu32
        |> Assert.IsTrue

        expected64 = actuali64
        |> Assert.IsTrue

        expected64 = actualu64
        |> Assert.IsTrue


    // The method call for the class for the main wasm testclass
    [<TestMethod>]
    member _.Test() = 
        test.IntBinary()
        test.FloatBinary()
        test.IntUnary()
        test.FloatUnary()
        test.IntRelation()
        test.FloatRelation()
        test.IntConvert()
        test.FloatConvert()
        test.IntSpecial()
        test.FloatSpecial()
        test.IntTest()