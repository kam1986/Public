module Types

type Op<'i8,'i16> =
    | I8 of 'i8
    | I16 of 'i16

type Value = Op<int8,int16>

type Type = Op<unit,unit>


type Format =
    | CB     of int8
    | Normal of int
