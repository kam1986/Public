namespace Decoding



open Wasm
open Values
open Types
module Wat =
    // TODO u and i must be fixed
    let u N bytes =
        let rec loop numb shift bytes  =
            match bytes with
            | b :: bytes when b > 127uy -> Ok(numb  + (uint64 (b &&& 63uy) <<< shift) , bytes)  
            | b :: bytes when shift < N -> loop (numb + (uint64 b <<< shift)) (shift+7) bytes
            
            | _ -> Error $"encoding error of u{N}" 
        loop 0UL 0 bytes

    let i N bytes = 
        let rec loop numb shift bytes =
            match bytes with
            | b :: _ when b < 127uy     -> Ok(numb + (int64 b <<< shift), bytes)
            | b :: bytes when shift < N -> loop (numb + (int64 b <<< shift)) (shift + 7) bytes
            | _                         -> Error $"encoding error of i{N}" 
        loop 0L 0 bytes

    let u32 bytes: Result<num * byte list,string> = 
        u 32 bytes 
        |> Result.map (fun (u,bytes) -> U32(uint u), bytes)

    let u64 bytes: Result<num * byte list,string>  =
        u 64 bytes 
        |> Result.map (fun (u, bytes) -> U64 u, bytes)

    let i32 bytes: Result<num * byte list,string>  =
        i 32 bytes
        |> Result.map (fun (i, bytes) -> I32(int i), bytes)

    let i64 bytes: Result<num * byte list,string>  =
        i 64 bytes
        |> Result.map (fun (i, bytes) -> I64 i, bytes)

    let testsigned = [0xC0uy; 0xBBuy; 0x78uy]
    let testunsigend = [0xE5uy; 0x8Euy; 0x26uy]

    let ii = i32 testsigned
    let uu = u32 testunsigend