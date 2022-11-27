module UInt128

open Checked
open Unchecked
open System.Runtime.InteropServices

type ('a, 'b) Pair = P of struct('a * 'b)

exception Overflow = System.OverflowException

(*
    This is the uint128 type
    it usage exception implementation of carry and borrow flags
*)
[<Struct; StructLayout(LayoutKind.Explicit)>]
type UInt128 =

    [<FieldOffset(0)>] val low  : uint64
    [<FieldOffset(8)>] val high : uint64

    new(high,low) = {
        high = high
        low = low
    } 

with 
    override u.ToString() =
        ((bigint u.high) <<< 64) + (bigint u.low)
        |> string

    static member Zero = UInt128(0UL, 0UL)
    static member One  = UInt128(0UL, 1UL)

    static member ( + ) (x: UInt128, y: UInt128) =
        let carry, low =
            try
                0UL, Checked.add x.low y.low
            with Overflow ->
                1UL, x.low + y.low

        let carry', high = 
            try
                0UL, Checked.add x.high (Checked.add y.high carry)
            with Overflow -> 
                1UL, 0UL

        UInt128(high, low + uint64 carry')

    static member ( - ) (x: UInt128, y: UInt128) =
        let borrow' = 1UL <<< 63
        let borrow, high =
            try
                0UL, (Checked.sub x.high y.high)
            with Overflow -> 
                borrow', 0UL

        let borrow'', low =
            try
                0UL, Checked.sub x.low (Checked.add y.low borrow)
            with Overflow ->
                borrow', 0UL

        UInt128(high + borrow'', low)


    static member ( * ) (x: UInt128, y: UInt128) =
        (*
        
            using karatsuba algorithm

            x = x1*B + x0
            y = y1*B + y0     
       
            we here assume B to be 2^64 for conviniency since this makes x1 be the upper 64bit integer
            and x0 the lower and the same for y1 and y0

            z2 = x1 * y1
            z1 = x1 * y0 + x0 * y1
            z0 = x0 * y0


            we get that z = x * y = z2 * B^2 + z1 * B^1 + z0 * B^0
            
            since B = 2^64 and we are dealing with unsigned numbers the z2 is basicly a wrap around value
            hence it will be placed as the same value in the lower 64bit integer part.

       *)

       let z2 = x.high * y.high
       let z1 = x.high * y.low + x.low * y.high
       let z0 = x.low * y.low

       // if just one of the high integer are none zero it will be more than zero
       // the conversion will 
       let correcting = System.Convert.ToUInt64((x.high ||| y.high) > 0UL)
              
       UInt128(z1, z0 + z2 - correcting)


    static member ( >>> ) (x: UInt128, y) =
        let bits = (128 - (y % 128))
        let carry = (x.high <<< bits) 

        UInt128(x.high >>> bits, (x.low >>> bits) + carry)


    static member ( <<< ) (x: UInt128, y) =
        let bits = (128 - (y % 128))
        let carry = (x.low >>> bits)

        UInt128((x.high <<< bits) + carry, x.low <<< bits)


    static member ( ||| ) (x: UInt128, y: UInt128) = UInt128(x.high ||| y.high, x.low ||| y.low)

    
    static member ( &&& ) (x: UInt128, y: UInt128) = UInt128(x.high &&& y.high, x.low &&& y.low)

    
    static member ( ^^^ ) (x: UInt128, y: UInt128) = UInt128(x.high ^^^ y.high, x.low ^^^ y.low)

    static member ( < ) (x: UInt128, y: UInt128) = 
        x.high < y.high || (x.high  = y.high && x.low < y.low)

    static member ( > ) (x: UInt128, y: UInt128) = 
        x.high > y.high || (x.high = y.high && x.low > y.low)

    static member ( = ) (x: UInt128, y: UInt128) = x.high = y.high && x.low = y.low

    static member ( <= ) (x: UInt128, y: UInt128) = 
        x.high <= y.high || (x.high  = y.high && x.low <= y.low)

    static member ( >= ) (x: UInt128, y: UInt128) = 
        x.high >= y.high || (x.high = y.high && x.low >= y.low)
