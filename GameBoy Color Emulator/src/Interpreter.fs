module Interpreter

open Types
open Memory
open Instructions
open CPU
open Fetch
open Helper


//#nowarn "25"

let reg8bit (cpu: CPU) = function
    | A -> &cpu.A
    | B -> &cpu.B
    | C -> &cpu.C
    | D -> &cpu.D
    | E -> &cpu.E
    | H -> &cpu.H
    | L -> &cpu.L
    | F -> &cpu.F

let reg16bit (cpu: CPU) = function
    | AF -> &cpu.AF
    | BC -> &cpu.BC
    | DE -> &cpu.DE
    | HL -> &cpu.HL
    | SP -> &cpu.SP

// TODO
let ei() = ()
let di() = ()

let Step (cpu: CPU) (memory : Memory) =
    memory.Read cpu.SP
    |> Fetch
    |> function
    | CB opcode -> DecodeCB.Decode opcode
    | Normal opcode -> Decode.Decode opcode          
    |> Result.mapWithError 
        (fun (instr, incr) ->
            // used for error tracing will be set to 0 when error occurs
            let mutable incr = incr
            let mutable err = "" // used to check error and ease code
            match instr with
            | NOP -> ()

            | DAA ->
                let r = &cpu.A
                let c = cpu.CarryFlag 
                let h = cpu.HalfCarryFlag 
                let low =  r &&& 0x0Fuy 
                let high = r &&& 0xF0uy 
                (*
                    decimal adjustment
                    transform binary format to decimal format
                    decimal format for a byte is a two digit decimal number
                        the lower 4 bits is used to represent the least significant digit (0-9)
                        the lower 4 bits is used to represent the most significant digit (0-9)
                        
                *)
                
                let correct_low  = 
                    if h = 1uy || low  >= 0x0Auy then 
                        low + 6uy
                    else 
                        cpu.HalfCarryFlag <- 0uy
                        low

                let correct_high = 
                    if c = 1uy || high >= 0xA0uy then 
                        low + 0x60uy 
                    else 
                        cpu.CarryFlag <- 0uy
                        high

                r <- correct_high ||| correct_low
                
                cpu.ZeroFlag <- if r = 0uy then 1uy else 0uy
                cpu.HalfCarryFlag <- 0uy
                
            | CPL ->
                // xor a byte with 0xFF will flip all bits set to unset and all unset to set
                cpu.A <- cpu.A ^^^ 0xFFuy
                cpu.SubtractFlag <- 1uy
                cpu.HalfCarryFlag <- 1uy

            | CCF ->
                cpu.CarryFlag <- cpu.CarryFlag ^^^ 1uy
                cpu.SubtractFlag <- 0uy
                cpu.HalfCarryFlag <- 0uy


            | SCF ->
                cpu.CarryFlag <- 1uy
                cpu.SubtractFlag <- 0uy
                cpu.HalfCarryFlag <- 0uy

            | HALT ->
                // should power down ??
                ()

            | STOP ->
                // halt cpu 
                // stop display until button pressed
                ()

            | DI ->
                // disables interrups
                // TODO
                di()

            | EI ->
                // enable interrupts
                // TODO
                ei()

            | RLCA ->
                let r = cpu.A
                
                cpu.A <- (r <<< 1) ||| (r >>> 7)
                cpu.ZeroFlag <- if cpu.A = 0uy then 1uy else 0uy
                cpu.SubtractFlag <- 0uy
                cpu.HalfCarryFlag <- 0uy
                cpu.CarryFlag <- (r >>> 7)

            | RLA ->
                let r = cpu.A
                
                cpu.A <- (r <<< 1) ||| cpu.CarryFlag
                cpu.ZeroFlag <- if cpu.A = 0uy then 1uy else 0uy
                cpu.SubtractFlag <- 0uy
                cpu.HalfCarryFlag <- 0uy
                cpu.CarryFlag <- (r >>> 7)


            | RRCA ->
                let r = cpu.A
                               
                cpu.A <- (r >>> 1) ||| (r <<< 7)
                cpu.ZeroFlag <- if cpu.A = 0uy then 1uy else 0uy
                cpu.SubtractFlag <- 0uy
                cpu.HalfCarryFlag <- 0uy
                cpu.CarryFlag <- (r &&& 1uy)

            | RRA ->
                let r = cpu.A
                               
                cpu.A <- (r >>> 1) ||| (cpu.CarryFlag <<< 7)
                cpu.ZeroFlag <- if cpu.A = 0uy then 1uy else 0uy
                cpu.SubtractFlag <- 0uy
                cpu.HalfCarryFlag <- 0uy
                cpu.CarryFlag <- (r &&& 1uy)

            | JP(flag, loc) ->
                let nn =
                    match loc with
                    | Imm16 nn -> nn
                    | Mem(HL, _) -> memory.Read cpu.HL
                    | _ -> 
                        err <- "illegal jump target"
                        cpu.PC // 

                let cond =
                    match flag with
                    | None -> true
                    | Some flag ->
                        match flag with
                        | Flag.NZ -> cpu.ZeroFlag = 0uy
                        | Flag.Z  -> cpu.ZeroFlag > 0uy
                        | Flag.NC -> cpu.CarryFlag = 0uy
                        | Flag.C  -> cpu.CarryFlag > 0uy

                if cond then
                    incr <- 0us // jump to a fixed adr no pc correction needed
                    cpu.PC <- nn  

            | JR(flag, n) -> 
                let cond =
                    match flag with
                    | None -> true
                    | Some flag ->
                        match flag with
                        | Flag.NZ -> cpu.ZeroFlag = 0uy
                        | Flag.Z  -> cpu.ZeroFlag > 0uy
                        | Flag.NC -> cpu.CarryFlag = 0uy
                        | Flag.C  -> cpu.CarryFlag > 0uy

                if cond then
                    incr <- 0us
                    cpu.PC <- uint16 n

            | CALL(flag, nn) ->
                let cond =
                    match flag with
                    | None -> true
                    | Some flag ->
                        match flag with
                        | Flag.NZ -> cpu.ZeroFlag = 0uy
                        | Flag.Z  -> cpu.ZeroFlag > 0uy
                        | Flag.NC -> cpu.CarryFlag = 0uy
                        | Flag.C  -> cpu.CarryFlag > 0uy

                if cond then
                    // push next instruction to the stack
                    memory.Write cpu.SP (cpu.PC + incr)
                    incr <- 0us // no correction needed
                    cpu.SP <- cpu.SP - 2us // set new stack pointer
                    cpu.PC <- nn

            | RST nn ->
                memory.Write cpu.SP cpu.PC
                incr <- 0us
                cpu.PC <- uint16 nn

            | RET flag ->
                let cond =
                    match flag with
                    | None -> true
                    | Some flag ->
                        match flag with
                        | Flag.NZ -> cpu.ZeroFlag = 0uy
                        | Flag.Z  -> cpu.ZeroFlag > 0uy
                        | Flag.NC -> cpu.CarryFlag = 0uy
                        | Flag.C  -> cpu.CarryFlag > 0uy

                if cond then
                    cpu.PC <- memory.Read cpu.SP
                    cpu.SP <- cpu.SP + 2us
                    incr <- 0us

            | RETI ->
                ei()
                cpu.PC <- memory.Read cpu.SP
                cpu.SP <- cpu.SP + 2us
                incr <- 0us

            | LD(Reg r, Imm8 n) -> reg8bit cpu r <- n
            
            // Same as LDH(n + 0xFF00, A)
            | LD(IMM(nn), Reg A) -> memory.Write nn cpu.A

            // Same as LDH(A, n + 0xFF00)
            | LD(Reg A, IMM(nn)) -> cpu.A <- memory.Read nn

            | LD(Reg HL, Mem(SP, nn)) -> 
                // check half carry 
                let h = byte ((0xFFus &&& cpu.SP) + (0xFFus &&& nn)) >>> 8
                // check carry
                let c = byte ((0xFFus &&& (cpu.SP >>> 8)) + (0xFFus &&& (nn >>> 8))) >>> 8

                memory.Write cpu.HL (cpu.SP + nn)
                cpu.ZeroFlag <- 0uy // reset Z flag
                cpu.SubtractFlag <- 0uy
                cpu.HalfCarryFlag <- h
                cpu.CarryFlag <- c

            | LD(Reg r, Imm16 nn) -> reg16bit cpu r <- nn

            | LD(Reg A, Mem(C, nn)) -> cpu.A <- memory.Read (uint16 cpu.C + nn)

            | LD(Mem(C, nn), Reg A) ->  memory.Write (uint16 cpu.C + nn) cpu.A

            | LD(Mem(r1, nn), Reg r2) -> memory.Write (reg16bit cpu r1 + nn) (reg8bit cpu r2)

            // There is only legal instructions that makes this posibble
            | LD(Reg r1, Mem(r2, nn)) -> reg8bit cpu r1 <- memory.Read (reg16bit cpu r2 + nn)
            
            
            | LD(IMM nn, Reg SP) -> memory.Write nn cpu.SP 

            | LD(Reg SP, Reg HL) -> cpu.SP <- cpu.HL

            | LD(Reg r1, Reg r2) -> // this will always be a pair of two 8bit registers 
                reg8bit cpu r1 <- reg8bit cpu r2
            
            // offset is always 0
            | LDD(Reg A, Mem(HL, _)) -> 
                cpu.A <- memory.Read cpu.HL
                cpu.HL <- cpu.HL - 1us
                cpu.SubtractFlag <- 1uy
                
            // offset is always 0
            | LDD(Mem(HL, _), Reg A) -> 
                memory.Write cpu.HL cpu.A
                cpu.HL <- cpu.HL - 1us
                cpu.SubtractFlag <- 1uy

            | LDI(Reg A, Mem(HL, _)) -> 
                cpu.A <- memory.Read cpu.HL
                cpu.HL <- cpu.HL + 1us
                
            // offset is always 0
            | LDI(Mem(HL, _), Reg A) -> 
                memory.Write cpu.HL cpu.A
                cpu.HL <- cpu.HL + 1us

            | Push r -> 
                memory.Write cpu.SP (reg16bit cpu r)
                cpu.SP <- cpu.SP - 2us

            | Pop r ->
                reg16bit cpu r <- memory.Read cpu.SP
                cpu.SP <- cpu.SP + 2us

    

            | Unary(I8 op, loc) ->

                match op with
                | I8.INC  ->
                    match loc with
                    | Reg r ->
                        let r = &reg8bit cpu r
                    
                        let h = ((r &&& 0b1111uy) + 1uy) >>> 4

                        r <- r + 1uy
                    
                        cpu.ZeroFlag <- if r = 0uy then 1uy else 0uy
                        cpu.SubtractFlag <- 0uy
                        cpu.HalfCarryFlag <- h

                    | Mem(HL, _) ->
                        let dst = memory.Read cpu.HL

                        let h = ((dst &&& 0b1111uy) + 1uy) >>> 4

                        memory.Write cpu.HL  (dst + 1uy)
                                            
                        cpu.ZeroFlag <- if dst + 1uy = 0uy then 1uy else 0uy
                        cpu.SubtractFlag <- 0uy
                        cpu.HalfCarryFlag <- h
                    | _ -> 
                        incr <- 0us
                        err <- "Illegal location for inc instruction"

                | I8.DEC ->
                    match loc with
                    | Reg r ->
                        let r = &reg8bit cpu r
                    
                        let h = ((r &&& 0b1111uy) - 1uy &&& 0b10000uy) >>> 4

                        r <- r - 1uy
                    
                        cpu.ZeroFlag <- if r = 0uy then 1uy else 0uy
                        cpu.SubtractFlag <- 0uy
                        cpu.HalfCarryFlag <- h

                    | Mem(HL, _) ->
                        let dst = memory.Read cpu.HL

                        let h = ((dst &&& 0b1111uy) - 1uy &&& 0b10000uy) >>> 4

                        memory.Write cpu.HL  (dst - 1uy)
                                            
                        cpu.ZeroFlag <- if dst + 1uy = 0uy then 1uy else 0uy
                        cpu.SubtractFlag <- 0uy
                        cpu.HalfCarryFlag <- h

                    | _ -> 
                        incr <- 0us
                        err <- "Illegal location for dec instruction"

                | I8.SWAP ->
                    match loc with
                    | Reg r ->
                        let r = &reg8bit cpu r
                        let low = r &&& 0xFuy
                        let high = r &&& 0xF0uy
                        r <- ((high >>> 4) ||| (low <<< 4))
                        
                        cpu.ZeroFlag <- if r = 0uy then 1uy else 0uy
                        cpu.SubtractFlag <- 0uy
                        cpu.HalfCarryFlag <- 0uy
                        cpu.CarryFlag <- 0uy


                    | Mem(HL, _) -> 
                        let r = memory.Read cpu.HL
                        let low = r &&& 0xFuy
                        let high = r &&& 0xF0uy
                        memory.Write cpu.HL ((high >>> 4) ||| (low <<< 4))
                        
                        cpu.ZeroFlag <- if r = 0uy then 1uy else 0uy
                        cpu.SubtractFlag <- 0uy
                        cpu.HalfCarryFlag <- 0uy
                        cpu.CarryFlag <- 0uy
                    
                    | _ -> 
                        incr <- 0us
                        err <- "Illegal location for swap instruction"

                | I8.RLC -> 
                    match loc with
                    | Reg r -> 
                        // order of instructions are important here
                        let r = &reg8bit cpu r

                        // set carry flag to highest bit value
                        cpu.CarryFlag <- (r >>> 7)
                        // compute and store result
                        r <- (r <<< 1) ||| (r >>> 7)
                        // check result and set zero flag if 0
                        cpu.ZeroFlag <- if r = 0uy then 1uy else 0uy
                        // reset rest
                        cpu.SubtractFlag <- 0uy
                        cpu.HalfCarryFlag <- 0uy

                    | Mem(HL, _) ->
                        let r = memory.Read cpu.HL
                        let res = ((r <<< 1) ||| (r >>> 7))
                        cpu.CarryFlag <- (r >>> 7)
                        memory.Write cpu.HL res
                        cpu.ZeroFlag <- if res = 0uy then 1uy else 0uy
                        // reset rest
                        cpu.SubtractFlag <- 0uy
                        cpu.HalfCarryFlag <- 0uy

                    | _ -> err <- "illegal argument for rlc instruction"
                    
                | I8.RL  -> 
                    match loc with
                    | Reg r -> 
                        // order of instructions are important here
                        let r = &reg8bit cpu r
                        let c = cpu.CarryFlag
                        // set carry flag to highest bit value
                        cpu.CarryFlag <- (r >>> 7)
                        // compute and store result
                        r <- (r <<< 1) ||| c
                        // check result and set zero flag if 0
                        cpu.ZeroFlag <- if r = 0uy then 1uy else 0uy
                        // reset rest
                        cpu.SubtractFlag <- 0uy
                        cpu.HalfCarryFlag <- 0uy

                    | Mem(HL, _) ->
                        let r = memory.Read cpu.HL
                        let res = (r <<< 1) ||| cpu.CarryFlag

                        cpu.CarryFlag <- (r >>> 7)
                        memory.Write cpu.HL res
                        cpu.ZeroFlag <- if res = 0uy then 1uy else 0uy
                        // reset rest
                        cpu.SubtractFlag <- 0uy
                        cpu.HalfCarryFlag <- 0uy

                    | _ -> err <- "illegal argument for rl instruction"

                | I8.RRC -> 
                    match loc with
                    | Reg r -> 
                        // order of instructions are important here
                        let r = &reg8bit cpu r

                        // set carry flag to highest bit value
                        cpu.CarryFlag <- (r &&& 1uy)
                        // compute and store result
                        r <- (r >>> 1) ||| (r <<< 7)
                        // check result and set zero flag if 0
                        cpu.ZeroFlag <- if r = 0uy then 1uy else 0uy
                        // reset rest
                        cpu.SubtractFlag <- 0uy
                        cpu.HalfCarryFlag <- 0uy

                    | Mem(HL, _) ->
                        let r = memory.Read cpu.HL
                        let res = (r >>> 1) ||| (r <<< 7)
                        cpu.CarryFlag <- (r &&& 1uy)
                        memory.Write cpu.HL res
                        cpu.ZeroFlag <- if res = 0uy then 1uy else 0uy
                        // reset rest
                        cpu.SubtractFlag <- 0uy
                        cpu.HalfCarryFlag <- 0uy

                    | _ -> err <- "illegal argument for rrc instruction"

                | I8.RR  ->
                    match loc with
                    | Reg r -> 
                        // order of instructions are important here
                        let r = &reg8bit cpu r

                        let c = cpu.CarryFlag
                        // set carry flag to highest bit value
                        cpu.CarryFlag <- (r &&& 1uy)
                        // compute and store result
                        r <- (r >>> 1) ||| (c <<< 7)
                        // check result and set zero flag if 0
                        cpu.ZeroFlag <- if r = 0uy then 1uy else 0uy
                        // reset rest
                        cpu.SubtractFlag <- 0uy
                        cpu.HalfCarryFlag <- 0uy

                    | Mem(HL, _) ->
                        let r = memory.Read cpu.HL
                        let c = cpu.CarryFlag
                        let res = (r >>> 1) ||| (c <<< 7)
                        cpu.CarryFlag <- (r &&& 1uy)
                        memory.Write cpu.HL res
                        cpu.ZeroFlag <- if res = 0uy then 1uy else 0uy
                        // reset rest
                        cpu.SubtractFlag <- 0uy
                        cpu.HalfCarryFlag <- 0uy

                    | _ -> err <- "illegal argument for rr instruction"

                | I8.SLA ->
                    match loc with
                    | Reg r -> 
                        let r = &reg8bit cpu r
                        cpu.SubtractFlag <- 0uy
                        cpu.HalfCarryFlag <- 0uy
                        cpu.CarryFlag <- (r &&& 1uy)
                        r <- (r <<< 1)
                        cpu.ZeroFlag <- if r = 0uy then 1uy else 0uy 

                    | Mem(HL, _) ->
                        let r = memory.Read cpu.HL
                        let res = (r <<< 1) 
                        cpu.CarryFlag <- (r &&& 1uy)
                        memory.Write cpu.HL res
                        cpu.ZeroFlag <- if res = 0uy then 1uy else 0uy
                        // reset rest
                        cpu.SubtractFlag <- 0uy
                        cpu.HalfCarryFlag <- 0uy
                        
                    | _ -> err <- "illegal argument for sla instruction"

                | I8.SRA -> 
                    match loc with
                    | Reg r -> 
                        let r = &reg8bit cpu r
                        cpu.SubtractFlag <- 0uy
                        cpu.HalfCarryFlag <- 0uy
                        cpu.CarryFlag <- (r &&& 1uy)
                        r <- (r >>> 1) ||| (r &&& 0x80uy)
                        cpu.ZeroFlag <- if r = 0uy then 1uy else 0uy 

                    | Mem(HL, _) ->
                        let r = memory.Read cpu.HL
                        let res = (r >>> 1) ||| (r &&& 0x80uy)
                        cpu.CarryFlag <- (r &&& 1uy)
                        memory.Write cpu.HL res
                        cpu.ZeroFlag <- if res = 0uy then 1uy else 0uy
                        // reset rest
                        cpu.SubtractFlag <- 0uy
                        cpu.HalfCarryFlag <- 0uy
                        
                    | _ -> err <- "illegal argument for sra instruction"

                | I8.SRL -> 
                    match loc with
                    | Reg r -> 
                        let r = &reg8bit cpu r
                        cpu.SubtractFlag <- 0uy
                        cpu.HalfCarryFlag <- 0uy
                        cpu.CarryFlag <- (r >>> 7)
                        r <- (r >>> 1) 
                        cpu.ZeroFlag <- if r = 0uy then 1uy else 0uy 

                    | Mem(HL, _) ->
                        let r = memory.Read cpu.HL
                        let res = (r >>> 1) 
                        cpu.CarryFlag <- (r >>> 7)
                        memory.Write cpu.HL res
                        cpu.ZeroFlag <- if res = 0uy then 1uy else 0uy
                        // reset rest
                        cpu.SubtractFlag <- 0uy
                        cpu.HalfCarryFlag <- 0uy
                        
                    | _ -> err <- "illegal argument for sra instruction"


            | Unary(I16 op, Reg r) ->
                let r = &reg16bit cpu r
                match op with
                | I16.INC  -> r <- r + 1us
                | I16.DEC  -> r <- r - 1us
               


            | Binary(I16 I16.ADD, Reg HL, Reg r) ->
                    let (dst: _ byref) = &reg16bit cpu HL
                    let (src: _ inref) = &reg16bit cpu r

                    let h = byte ((dst &&& 0xFFFus) + (src &&& 0xFFFus) >>> 11)
                    let c = byte ((uint dst + uint src) >>> 15) 
                    
                    dst <- dst + src

                    
                    cpu.SubtractFlag <- 0uy
                    cpu.HalfCarryFlag <- h
                    cpu.CarryFlag <- c

            | Binary(I16 I16.ADD, Reg SP, Imm8 n) ->
                let (dst: _ byref) = &reg16bit cpu HL
                let src = uint16 n

                let h = byte ((dst &&& 0xFFFus) + src >>> 11)
                let c = byte ((uint dst + uint src) >>> 15) 
                                   
                dst <- dst + src

                cpu.ZeroFlag <- 0uy                   
                cpu.SubtractFlag <- 0uy
                cpu.HalfCarryFlag <- h
                cpu.CarryFlag <- c

            // all 8bit ALU instructions takes a register as first argument
            | Binary(I8 op, Reg r, src) ->
                    let src =
                        match src with
                        | Imm8 n -> n
                        | Mem(r, _) -> memory.Read (reg16bit cpu r)
                        | Reg r -> reg8bit cpu r
                        | _ -> 
                            incr <- 0us
                            err <- "illegal argument"
                            0uy
                    
                    let r = &reg8bit cpu r

                    match op with
                    | I8.ADD ->
                        let z = if r + src = 0uy then 1uy else 0uy
                        let n = 0uy
                        let h = ((r &&& 0b1111uy) + (src &&& 0b1111uy)) >>> 4
                        let c = byte ((uint16 r + uint16 src) >>> 8)
                        r <- r + src
                        cpu.ZeroFlag <- z
                        cpu.SubtractFlag <- n
                        cpu.HalfCarryFlag <- h
                        cpu.CarryFlag <- c

                    | I8.ADC -> 
                        let z = if r + src + cpu.CarryFlag = 0uy then 1uy else 0uy
                        let n = 0uy
                        let h = ((r &&& 0b1111uy) + (src &&& 0b1111uy) + cpu.CarryFlag) >>> 4
                        let c = byte ((uint16 r + uint16 src) >>> 8)
                        r <- r + src + cpu.CarryFlag

                        cpu.ZeroFlag <- z
                        cpu.SubtractFlag <- n
                        cpu.HalfCarryFlag <- h
                        cpu.CarryFlag <- c
                    
                    | I8.SUB ->
                        let z = if r - src = 0uy then 1uy else 0uy
                        let n = 1uy
                        // we need to check use bitwise and since we check by wrap around
                        let h = ((r &&& 0b1111uy) - (src &&& 0b1111uy) &&& 0b10000uy) >>> 4
                        let c = byte (((uint16 r - uint16 src) &&& 0b1_0000_0000us) >>> 8)
                        r <- r - src

                        cpu.ZeroFlag <- z
                        cpu.SubtractFlag <- n
                        cpu.HalfCarryFlag <- h
                        cpu.CarryFlag <- c

                    | I8.SBC ->
                        let z = if r - src - cpu.CarryFlag = 0uy then 1uy else 0uy
                        let n = 1uy
                        // we need to check use bitwise and since we check by wrap around
                        let h = (((r &&& 0b1111uy) - ((src + cpu.CarryFlag) &&& 0b1111uy) - cpu.CarryFlag) &&& 0b10000uy) >>> 4
                        let c = byte (((uint16 r - uint16 src - uint16 cpu.CarryFlag) &&& 0b1_0000_0000us) >>> 8)
                        r <- r - src

                        cpu.ZeroFlag <- z
                        cpu.SubtractFlag <- n
                        cpu.HalfCarryFlag <- h
                        cpu.CarryFlag <- c

                        
                    | I8.AND ->
                        r <- r &&& src

                        cpu.ZeroFlag <- if r > 0uy then 0uy else 1uy
                        cpu.SubtractFlag <- 0uy
                        cpu.HalfCarryFlag <- 1uy
                        cpu.CarryFlag <- 0uy


                    | I8.OR -> 
                        r <- r &&& src

                        cpu.ZeroFlag <- if r > 0uy then 0uy else 1uy
                        cpu.SubtractFlag <- 0uy
                        cpu.HalfCarryFlag <- 0uy
                        cpu.CarryFlag <- 0uy

                    | I8.XOR ->
                        r <- r ^^^ src

                        cpu.ZeroFlag <- if r > 0uy then 0uy else 1uy
                        cpu.SubtractFlag <- 0uy
                        cpu.HalfCarryFlag <- 0uy
                        cpu.CarryFlag <- 0uy


                    | I8.CP -> 
                        let res = r - src
                        
                        let z = if res = 0uy then 1uy else 0uy
                        let n = 1uy
                        // we need to check use bitwise and since we check by wrap around
                        let h = ((r &&& 0b1111uy) - (src &&& 0b1111uy) &&& 0b10000uy) >>> 4
                        let c = byte (((uint16 r - uint16 src) &&& 0b1_0000_0000us) >>> 8)
                        
                        cpu.ZeroFlag <- z
                        cpu.SubtractFlag <- n
                        cpu.HalfCarryFlag <- h
                        cpu.CarryFlag <- c

                    | I8.BIT -> cpu.ZeroFlag <- r &&& (1uy <<< int src)
                        
                    | I8.SET -> cpu.ZeroFlag <- r ||| (1uy <<< int src)

                    | I8.RES -> cpu.ZeroFlag <- r &&& (0xFFuy ^^^ (1uy <<< int src))

            | _ -> err <- "illegal binary operation argument"

            cpu.PC <- cpu.PC + incr

            if err = "" then
                Ok()
            else
                Error err
        )


