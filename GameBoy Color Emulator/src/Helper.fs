module Helper

open Instructions
open CPU

type Result<'a,'e> with
    
    static member map2 f ret1 ret2 =
        match ret1, ret2 with
        | Error e, _ | _, Error e -> Error e
        | Ok r1, Ok r2 -> Ok(f r1 r2)

    static member mapWithError f ret =
        match ret with 
        | Error e -> Error e
        | Ok r -> f r



let Get8BitLocation opcode =
    match opcode &&& 0b111y with
    | 0b111y -> Ok(Reg A )
    | 0b000y -> Ok(Reg B )
    | 0b001y -> Ok(Reg Register.C )
    | 0b010y -> Ok(Reg D )
    | 0b011y -> Ok(Reg E )
    | 0b100y -> Ok(Reg H )
    | 0b101y -> Ok(Reg L )
    | 0b110y -> Ok(Mem (HL, 0us))
    | _     -> Error ""


let Get16BitLocation opcode =
    match opcode &&& 0b111y with
    //| 0b111y -> Ok 
    | 0b000y -> Ok(BC)
    //| 0b001y -> Ok 
    | 0b010y -> Ok(DE)
    //| 0b011uy -> Ok
    | 0b100y -> Ok(HL)
    //| 0b101uy -> Ok
    | 0b110y -> Ok(SP)
    | _ -> Error "16bit register encoding error"



let inline GetBits opcode = byte (opcode >>> 3) &&& 0b111uy



let printbits i =
    printf "0b"
    for s in 31 .. -1 .. 0 do
        printf "%i" ((i >>> s) &&& 1)
    printfn ""
    