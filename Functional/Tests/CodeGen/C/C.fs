module CToWasm

open SymbolTableTests
open C.Test.Expression
open C.Test.Condition
open C.Test.Statement
open C.Test.Type
open C.Test.Value
open C.Test.Operation

type CToWasm() =
    
    member _.Test() =
        SymbolTable().Test()
        
        CTypeToWasmType().Test()
        
        CValueToWasmValue().Test()
        
        COpToWasmOp().Test()
                
        CExprToWasm().Test()
        
        CCondToWasm().Test()
        
        CStatementToWasm().Test()