module Program = 
    
    open SymbolTableTests
    open CToWasm
    open WasmToBinary
    open WasmToWat
    (*
        This is the main test function
        it calls all main test programs for each test section
    *)
    let [<EntryPoint>] main _ = 
        
        CToWasm().Test()
        WasmToBinary().Test() 
        WasmToWat().Test()
        0 

