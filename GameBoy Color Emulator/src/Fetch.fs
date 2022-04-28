module Fetch

open Types


// check mask for extended instruction encoding
let [<Literal>] CB_ = 0xCB_00_00_00

let [<Literal>] CBOP_ = 0x00_FF_00_00

 
let Fetch opcode =
    match opcode &&& CB_ with
    | CB_ -> 
        // remove CB tag and set opcode to the lower 8 bits since no imm follows any of the instructions
        CB (int8 ((opcode &&& CBOP_) >>> 16))

    | _   -> 
        // set the lower 24 bits to opcode + possible 8 bit imm or 16 imm
        Normal (opcode >>> 8)
    