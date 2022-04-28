module Decode

open Types
open Instructions
open Helper
open CPU

(*
    The opcode are fetch by the Fetch function in Fetch.fs
    This Decode only take into respect non CB prefixed encoding
    opcode is an 32bit integer with the format
    
    b1      b2     b3  b4
    0x00__opcode__imm8_00

    or

    b1      b2  b3-b4
    0x00_opcode_imm16

*)

let DecodeError (msg: string)  =
    Error $"Decoding Error:\n  {msg}"

let Decode opcode =
    let upper = int8 (opcode >>> 16)
    match upper with
    | 0x00y -> Ok (NOP, 1)
    | 0x10y -> Ok (STOP, 1)
    | 0x76y -> Ok (HALT, 1)
    | 0xF3y -> Ok (DI,  1)
    | 0xFBy -> Ok (EI,  1)
    | 0x20y -> Ok(JR(Some NZ, uint8 (opcode >>> 8)), 2) 
    | 0x30y -> Ok(JR(Some NC, uint8 (opcode >>> 8)), 2)
        
    | 0x01y | 0x11y | 0x21y | 0x31y ->
        Get16BitLocation (upper >>> 3) 
        |> Result.map (fun loc -> LD(Reg loc, Imm16(uint16 opcode)), 3)

    | 0x02y | 0x12y ->
        Get16BitLocation (upper >>> 3) 
        |> Result.map (fun loc -> LD(Mem(loc, 0us), Reg A), 1)
        
    
    | 0x22y -> Ok (LDI(Mem(HL, 0us), Reg A), 1)

    | 0x32y -> Ok (LDD(Mem(HL, 0us), Reg A), 1)

    | 0x03y | 0x13y | 0x23y | 0x33y ->
        Get16BitLocation (upper >>> 3) 
        |> Result.map (fun loc -> Unary(I16 I16.INC, Mem(loc, 0us)), 1)

    | 0x04y | 0x14y | 0x24y | 0x34y ->
        Get8BitLocation (upper >>> 3) 
        |> Result.map (fun loc -> Unary(I8 I8.INC, loc), 1)

    | 0x05y | 0x15y | 0x25y | 0x35y ->
        Get8BitLocation (upper >>> 3) 
        |> Result.map (fun loc -> Unary(I8 I8.DEC, loc), 1)

    | 0x07y -> Ok(RLCA, 1)
    
    | 0x17y -> Ok(RLA, 1)
    
    | 0x27y -> Ok(DAA, 1)
    
    | 0x37y -> Ok(SCF, 1)
    
    | 0x08y -> Ok (LD(IMM(uint16 opcode), Reg SP), 3)

    | 0x18y -> Ok (JR(None, uint8 (opcode >>> 8)), 2)
    
    | 0x28y -> Ok (JR(Some Z, uint8 (opcode >>> 8)), 2)
    
    | 0x38y -> Ok (JR(Some Flag.C, uint8 (opcode >>> 8)), 2)

    | 0x09y | 0x19y | 0x29y | 0x39y ->
        Get16BitLocation (upper >>> 3)
        |> Result.map (fun loc -> Binary(I16 I16.ADD, Reg HL, Reg loc), 1)

    | 0x0Ay | 0x1Ay ->
        Get16BitLocation (upper >>> 3)
        |> Result.map (fun loc -> LD(Reg A, Mem(loc, 0us)), 1)
     
    | 0x2Ay -> Ok(LDI(Reg A, Mem(HL, 0us)), 1)

    | 0x3Ay -> Ok(LDD(Reg A, Mem(HL, 0us)), 1)

    | 0x0By | 0x1By | 0x2By | 0x3By ->
        Get16BitLocation (upper >>> 3)
        |> Result.map (fun loc -> Unary(I16 I16.DEC, Reg loc), 1)

    | 0x0Cy | 0x1Cy | 0x2Cy | 0x3Cy ->
        Get8BitLocation (upper >>> 3)
        |> Result.map (fun loc -> Unary(I8 I8.INC, loc), 1)

    | 0x0Dy | 0x1Dy | 0x2Dy | 0x3Dy ->
        Get8BitLocation (upper >>> 3)
        |> Result.map (fun loc -> Unary(I8 I8.DEC, loc), 1)

    | 0x06y | 0x16y | 0x26y | 0x36y | 0x0Ey | 0x1Ey | 0x2Ey | 0x3Ey ->
        Get8BitLocation (upper >>> 3)
        |> Result.map (fun loc -> LD(loc, Imm8 (uint8 (opcode >>> 8))), 2)

    | 0x0Fy -> Ok (RRCA, 1)
    | 0x1Fy -> Ok (RRA, 1)
                  
    | 0x3Fy -> Ok (CCF, 1)
    | 0x2Fy -> Ok (CPL, 1)

    | _ when upper < 0x80y ->
        Result.map2 (fun loc1 loc2 -> LD(loc1, loc2), 1) (Get8BitLocation (upper >>> 3)) (Get8BitLocation upper)
        
    | _ when upper < 0x88y ->
        Get8BitLocation upper
        |> Result.map (fun loc -> Binary(I8 I8.ADD, Reg A, loc), 1) 
         
    
    | _ when upper < 0x90y ->
       Get8BitLocation upper
       |> Result.map (fun loc -> Binary(I8 I8.ADC, Reg A, loc), 1) 

    | _ when upper < 0x98y ->
        Get8BitLocation upper
        |> Result.map (fun loc -> Binary(I8 I8.SUB, Reg A, loc), 1) 

    | _ when upper < 0xA0y ->
        Get8BitLocation upper
        |> Result.map (fun loc -> Binary(I8 I8.SBC, Reg A, loc), 1) 
        
    | _ when upper < 0xA8y ->
        Get8BitLocation upper
        |> Result.map (fun loc -> Binary(I8 I8.AND, Reg A, loc), 1) 

    | _ when upper < 0xB0y ->
        Get8BitLocation upper
        |> Result.map (fun loc -> Binary(I8 I8.XOR, Reg A, loc), 1) 

    | _ when upper < 0xB8y ->
        Get8BitLocation upper
        |> Result.map (fun loc -> Binary(I8 I8.OR, Reg A, loc), 1) 

    | _ when upper < 0xC0y ->
        Get8BitLocation upper
        |> Result.map (fun loc -> Binary(I8 I8.CP, Reg A, loc), 1)

    | 0xC0y -> Ok(RET(Some NZ), 1)
    
    | 0XD0y -> Ok(RET(Some NC), 1)
    
    | 0xE0y -> Ok(LDH(IMM (uint16 (opcode >>> 8) + 0xFF00us), Reg A), 2)
        
    | 0XF0y -> Ok(LDH(Reg A, IMM (uint16 (opcode >>> 8) + 0xFF00us)), 2)
    
    
    | 0xC1y | 0xD1y | 0xE1y | 0xF1y ->
        Get16BitLocation (upper >>> 3)
        // need to correct the SP to AF
        |> Result.map (function SP -> Pop AF, 1 | loc -> Pop loc, 1)

    | 0xC2y -> Ok(JP(Some NZ, Imm16 (uint16 opcode)), 3)
    
    | 0xD2y -> Ok(JP(Some NC, Imm16 (uint16 opcode)), 3)

    | 0xE2y -> Ok(LD(Mem(C, 0xFF00us), Reg A), 2)

    | 0xF2y -> Ok(LD(Reg A, Mem(C, 0xFF00us)), 2)

    | 0xC3y -> Ok(JP(None, Imm16 (uint16 opcode)), 3)
    
    | 0xC4y -> Ok(CALL(Some NZ, uint16 opcode), 3)

    | 0xD4y -> Ok(CALL(Some NC, uint16 opcode), 3)

    | 0xC5y | 0xD5y | 0xE5y | 0xF5y ->
        Get16BitLocation (upper >>> 3)
        |> Result.map (fun loc -> Push(loc), 1)

    | 0xC6y -> Ok(Binary(I8 I8.ADD, Reg A, Imm8 (uint8 (opcode >>> 8))), 2)
    
    | 0xD6y -> Ok(Binary(I8 I8.SUB, Reg A, Imm8 (uint8 (opcode >>> 8))), 2)
    
    | 0xE6y -> Ok(Binary(I8 I8.AND, Reg A, Imm8 (uint8 (opcode >>> 8))), 2)
    
    | 0xF6y -> Ok(Binary(I8 I8.OR,  Reg A, Imm8 (uint8 (opcode >>> 8))), 2)

    | 0xCEy -> Ok(Binary(I8 I8.ADC, Reg A, Imm8 (uint8 (opcode >>> 8))), 2)

    | 0xDEy -> Ok(Binary(I8 I8.SBC, Reg A, Imm8 (uint8 (opcode >>> 8))), 2)

    | 0xEEy -> Ok(Binary(I8 I8.XOR, Reg A, Imm8 (uint8 (opcode >>> 8))), 2)

    | 0xFEy -> Ok(Binary(I8 I8.CP,  Reg A, Imm8 (uint8 (opcode >>> 8))), 2)
    
    | 0xC7y -> Ok(RST 0x00s, 1)        
    | 0xD7y -> Ok(RST 0x08s, 1)        
    | 0xE7y -> Ok(RST 0x10s, 1)
    | 0xF7y -> Ok(RST 0x18s, 1) 
    | 0xCFy -> Ok(RST 0x20s, 1)        
    | 0xDFy -> Ok(RST 0x28s, 1)        
    | 0xEFy -> Ok(RST 0x30s, 1)
    | 0xFFy -> Ok(RST 0x38s, 1)

    | 0xC8y -> Ok(RET(Some Z), 1)
    | 0xD8y -> Ok(RET(Some Flag.C), 1)
    | 0xE8y -> Ok(Binary(I16 I16.ADD, Reg SP, Imm8 (uint8 (opcode >>> 8))), 2)
    | 0xF8y -> Ok(LD(Reg HL, Mem(SP, uint16 (byte (opcode >>> 8)))), 2)

    | 0xC9y -> Ok(RET(None), 1)
    | 0xD9y -> Ok(RETI, 1)
    | 0xE9y -> Ok(JP(None, Mem(HL, 0us)), 1)
    | 0xF9y -> Ok(LD(Reg SP, Reg HL), 1)

    | 0xCAy -> Ok(JP(Some Z, Imm16 (uint16 opcode)), 3)
    | 0xDAy -> Ok(JP(Some Flag.C, Imm16 (uint16 opcode)), 3)
    | 0xEAy -> Ok(LD(Imm16 (uint16 opcode), Reg A) ,1)
    | 0xFAy -> Ok(LD(Reg A, IMM (uint16 opcode)),1)

    | 0xCCy -> Ok(CALL(Some Z, uint16 opcode), 3)
    | 0xDCy -> Ok(CALL(Some Flag.C, uint16 opcode), 3)
    | 0xCDy -> Ok(CALL(None, uint16 opcode), 3)

    | _ -> DecodeError (sprintf "Invalid opcode 0x%02x " upper)

    |> Result.map (fun (instr, sz) -> instr, uint16 sz)
    
