module Token
open System
open System.Text
open Position
open TypeAcrobatics
open Interfaces
(*
    This is a generic token type, used both in lexing and the parsing fase
    the integer is meant to enhance pattern matching by converting it to a DFA
    
    delay is a delayed transformation it simply delay the transformation of the string until we put a specific value into the parse tree

    this makes it possible to both pattern match specific tokens and get a generic type for any type feed into the lexer and parser

    ID might be used to track last state in the parser DFA

    The underlying stream are remade at a later point and will course the token type to change
    internally but not at the interface level

    OBS: the new token type uses span indexing for high performance computational lexering of subset of the content.
*)




[<Struct>]
type 'Type Token =
    val tp : 'Type
    val value : token
    val pos : Position
    new(tp, data, pos) = { tp=tp; value=data; pos=pos}


let inline TypeOf (token : _ Token) = token.tp

let inline ValueOf (token : _ Token) = 
    let t = Take token.value
    id t 

let inline PosOf (token : _ Token) = token.pos
  
  (*
/// This is the new and improved version of a token. Not yet implemented into the lexer and parser,
/// The implementation into the lexer and parser combied with the Pos, Value and Type interfaces should make useful error insertion possible.
/// This interface should also be used for reimplementation of the parser stack. 
[<Struct>]
type 't token =
    val mutable content: byte []
    val tp: 't
    val mutable pos: Position
    val length : int

    new(content, t, pos : Position byref, length) = {
        content = content
        tp = t
        pos = Copy &pos
        length = length
    } 

    interface Pos with
        member T.Pos = T.pos

    interface 't Type with
        member T.typeof = T.tp
        
    interface int Value with
        member T.Cast =
            try
                let start = Absolut &(T.pos)
                // this should eleminate 99% of the copy past nature of the array slice i.e. performance gain
                let value = Encoding.UTF8.GetString(ReadOnlySpan(T.content, start, T.length))
                int value
                |> Ok
            with
            | err -> 
                err.Message
                |> Error

    interface int8 Value with
        member T.Cast =
            try
                let start = Absolut &(T.pos)
                let value = Encoding.UTF8.GetString(ReadOnlySpan(T.content, start, T.length))
                int8 value
                |> Ok
            with
            | err -> 
                err.Message
                |> Error

    interface int16 Value with
        member T.Cast =
            try
                let start = Absolut &(T.pos)
                let value = Encoding.UTF8.GetString(ReadOnlySpan(T.content, start, T.length))
                int16 value
                |> Ok
            with
            | err -> 
                err.Message
                |> Error

    interface int64 Value with
        member T.Cast =
            try
                let start = Absolut &(T.pos)
                let value = Encoding.UTF8.GetString(ReadOnlySpan(T.content, start, T.length))
                int64 value
                |> Ok
            with
            | err -> 
                err.Message
                |> Error

    interface uint Value with
        member T.Cast =
            try
                let start = Absolut &(T.pos)
                let value = Encoding.UTF8.GetString(ReadOnlySpan(T.content, start, T.length))
                uint value
                |> Ok
            with
            | err -> 
                err.Message
                |> Error

    interface uint8 Value with
        member T.Cast =
            try
                let start = Absolut &(T.pos)
                let value = Encoding.UTF8.GetString(ReadOnlySpan(T.content, start, T.length))
                uint8 value
                |> Ok
            with
            | err -> 
                err.Message
                |> Error

    interface uint16 Value with
        member T.Cast =
            try
                let start = Absolut &(T.pos)
                let value = Encoding.UTF8.GetString(ReadOnlySpan(T.content, start, T.length))
                uint16 value
                |> Ok
            with
            | err -> 
                err.Message
                |> Error

    
    interface uint64 Value with
        member T.Cast =
            try
                let start = Absolut &(T.pos)
                let value = Encoding.UTF8.GetString(ReadOnlySpan(T.content, start, T.length))
                uint64 value
                |> Ok
            with
            | err -> 
                err.Message
                |> Error

    interface float Value with
        member T.Cast =
            try
                let start = Absolut &(T.pos)
                let value = Encoding.UTF8.GetString(ReadOnlySpan(T.content, start, T.length))
                float value
                |> Ok
            with
            | err -> 
                err.Message
                |> Error


    interface float32 Value with
        member T.Cast =
            try
                let start = Absolut &(T.pos)
                let value = Encoding.UTF8.GetString(ReadOnlySpan(T.content, start, T.length))
                float32 value
                |> Ok
            with
            | err -> 
                err.Message
                |> Error

    interface decimal Value with
        member T.Cast =
            try
                let start = Absolut &(T.pos)
                let value = Encoding.UTF8.GetString(ReadOnlySpan(T.content, start, T.length))
                decimal value
                |> Ok
            with
            | err -> 
                err.Message
                |> Error


    interface string Value with
        member T.Cast = 
            try 
                let start = Absolut &(T.pos)
                Encoding.UTF8.GetString(ReadOnlySpan(T.content, start, T.length)) // through error when wrong encoding.
                |> Ok
            with
            | err -> Error err.Message


    interface bool Value with
        member T.Cast =
            try
                let start = Absolut &(T.pos)
                let value = Encoding.UTF8.GetString(ReadOnlySpan(T.content, start, T.length))
                bool.Parse value
                |> Ok
            with
            | err -> Error err.Message

    interface char Value with
        member T.Cast =
          try
                let start = Absolut &(T.pos)
                let value = Encoding.UTF8.GetString(ReadOnlySpan(T.content, start, T.length))
                char value
                |> Ok
          with
          | err -> Error err.Message

    interface unit Value with
        member T.Cast = Ok ()

    // primarily used to handle non utf8 code 
    interface byte [] Value with
        member T.Cast = Ok T.content.[Absolut &T.pos .. T.length-1]
*)