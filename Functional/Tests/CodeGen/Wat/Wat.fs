module WasmToWat

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

open Wat
open Value
open Type
open Operand
open Refop
open Binop
open Relop
open Convert
open SExpression

[<TestClass>] 
type WasmToWat() =
    



    [<TestMethod>]
    member _.Test() =
        Value().Test()
        Type().Test() 
        Operand().Test()
        Refop().Test()
        Binop().Test()
        Relop().Test()
        Convert().Test()
        SExpression().Test()