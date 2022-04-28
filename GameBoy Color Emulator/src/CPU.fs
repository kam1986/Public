module CPU

// unsafe code: structual layout added
#nowarn "9"
open System.Runtime.InteropServices


type Register =
    | A
    | F
    | B
    | C
    | D
    | E
    | H
    | L 
    | AF  
    | BC  
    | DE  
    | HL  
    | SP  
    

[<StructLayout(LayoutKind.Explicit)>]
type CPU =
    
    // fieldOffset are given becuase we need
    // to relay on manipulating two regs in one op
    [<FieldOffset(0)>]  val mutable A: byte
    [<FieldOffset(1)>]  val mutable F: byte
    [<FieldOffset(2)>]  val mutable B: byte
    [<FieldOffset(3)>]  val mutable C: byte
    [<FieldOffset(4)>]  val mutable D: byte
    [<FieldOffset(5)>]  val mutable E: byte
    [<FieldOffset(6)>]  val mutable H: byte
    [<FieldOffset(7)>]  val mutable L: byte

    // This will treat two registers as one big one
    [<FieldOffset(0)>] val mutable AF: uint16
    [<FieldOffset(2)>] val mutable BC: uint16
    [<FieldOffset(4)>] val mutable DE: uint16
    [<FieldOffset(6)>] val mutable HL: uint16


    [<FieldOffset(8)>]  val mutable internal SP: uint16
    [<FieldOffset(10)>] val mutable internal PC: uint16

    [<FieldOffset(11)>] val mutable private FLAGS: byte

    new() = 
        {
            A  = 0uy
            F  = 0uy
            B  = 0uy
            C  = 0uy
            D  = 0uy
            E  = 0uy
            H  = 0uy
            L  = 0uy
            AF = 0us
            BC = 0us
            DE = 0us
            HL = 0us
            SP = 0us
            PC = 0x100us // the program counter always starts at this location
            FLAGS = 0uy
        }
with
    member cpu.ZeroFlag 
        with get() = (cpu.FLAGS &&& 0b10000000uy) >>> 7
        and set flag = cpu.FLAGS <- (flag <<< 7) ||| (cpu.FLAGS &&& 0b01110000uy)
        
    member cpu.SubtractFlag  
        with get() = (cpu.FLAGS &&& 0b01000000uy) >>> 6
        and set flag = cpu.FLAGS <- (flag <<< 6) ||| (cpu.FLAGS &&& 0b10110000uy)

    member cpu.HalfCarryFlag 
        with get() = (cpu.FLAGS &&& 0b10000000uy) >>> 5
        and set flag = cpu.FLAGS <- (flag <<< 5) ||| (cpu.FLAGS &&& 0b11010000uy)

    member cpu.CarryFlag 
        with get() = (cpu.FLAGS &&& 0b00010000uy) >>> 4
        and set flag = cpu.FLAGS <- (flag <<< 4) ||| (cpu.FLAGS &&& 0b11100000uy)


