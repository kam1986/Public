module Instructions

open Types
open CPU


module I8 =
    type unary = 
        | INC
        | DEC
        | SWAP
        | RLC
        | RRC
        | RL
        | RR
        | SLA
        | SRA
        | SRL

    type binop =
        | ADD
        | ADC
        | SUB
        | SBC
        | AND
        | OR
        | XOR
        | CP
        | BIT
        | SET
        | RES
        
    

module I16 =
    type unary = 
        | INC
        | DEC
        

    type binop =
        | ADD

        

type Location =
    | Reg   of Register
    | Mem   of Register * uint16
    | IMM   of uint16
    | Imm8  of uint8
    | Imm16 of uint16

type Flag =
    | NZ
    | Z
    | NC
    | C


type Instruction =
    | NOP
    | HALT
    | STOP
    | SCF
    | DI
    | EI
    | RLCA
    | DAA
    | RRCA
    | RLA
    | RRA
    | CPL
    | CCF
    | RETI
    | Push   of Register
    | Pop    of Register
    | Unary  of Op<I8.unary, I16.unary> * Location
    | Binary of Op<I8.binop, I16.binop> * Location * Location
    | LD     of Location * Location
    | LDD    of Location * Location
    | LDI    of Location * Location
    | LDH    of Location * Location
    | JP     of Flag option * Location 
    | JR     of Flag option * uint8
    | CALL   of Flag option * uint16
    | RET    of Flag option
    | RST    of int16
