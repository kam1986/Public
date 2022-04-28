module Wasm.Test.Value

open Microsoft.VisualStudio.TestTools.UnitTesting

open Wasm
open Values
open Types
open Wasm.Encode

(*

    Since I32 are converted through the I64 function it is satisfying to just test that

*)

[<TestClass>]
type ValueToBinary() as test =
    

    [<TestMethod>]
    member _.I64() = 
        let actual0 = i64 0
        let actual1 = i64 -123456
        
        let expected0 = [0uy]
        let expected1 = [0xc0uy; 0xbbuy; 0x78uy]

        actual0 = expected0
        |> Assert.IsTrue

        actual1 = expected1
        |> Assert.IsTrue



    
    [<TestMethod>]
    member _.U64() = 
        let actual0 = u64 0
        let actual1 = u64 624485
        
        let expected0 = [0uy]
        let expected1 = [0xe5uy; 0x8euy; 0x26uy]

        actual0 = expected0
        |> Assert.IsTrue

        actual1 = expected1
        |> Assert.IsTrue
    


    [<TestMethod>]
    member _.F32() =
        (*
            the test here is not to show correctly byte translation, but only
            to test for correct indianess, since the System.BitConverter.GetBytes
            only exposes the underlying bytes of the floating point
        *)
        let actual = f32 -0f


        let expected = [|0uy; 0uy; 0uy; 1uy <<< 7|]

        actual = expected
        |> Assert.IsTrue
        
    
    [<TestMethod>]
    member _.F64() = 
        (*
            as above we are this is merely to test for byte ordering.
        *)
        let actual = f64 -0.
 
        let expected = [|0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 1uy <<< 7|]

        
        actual = expected
        |> Assert.IsTrue



    member _.Test() =
        test.F32()
        test.F64()
        test.I64()
        test.U64()