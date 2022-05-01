module Buffer

exception IndexOutOfBound

open System
open System.IO
open System.Collections


let inline private ( %% ) i p =
    let p' = (1 <<< p) - 1
    let x = (i &&& p') + (i >>> p)
    if x < p' then
        x
    else
        x - p'

type LexBuffer =
    val dbuf : byte []
    val mutable count : int
    val mutable cur: int
    val mutable page: int
    val mutable lastPage: int
    val file: FileStream // unbuffered stream

    new(stream: FileStream) =
        let db = [| for _ in 1 .. 2048 -> 0uy |]

        let c = stream.Read(db, 0, 1024) // make sure that the first half of the buffer ar filled on startup 
        { dbuf = db; count = c; cur = 0; page = 0; lastPage = 0; file = stream }
    
    new(path: string) = new LexBuffer(File.OpenRead(path))
        
        

    interface IDisposable with
        member LB.Dispose() = 
                Array.Clear(LB.dbuf, 0, 2048) // remove data leakage
                LB.file.Dispose()


    interface IEnumerable with
        member LB.GetEnumerator() =
            seq { 0 .. int LB.file.Length } // immutable sequence of positions in the file
            |> Seq.map (fun i -> LB.Read i) // mapped to the position in the file
            |> Seq.takeWhile // run while no error occures
                (fun e -> 
                    match e with
                    | Ok _ -> true
                    | _ -> false
                )
            |> Seq.map (function Ok b -> b | Error e -> raise e)
            |> fun s -> s.GetEnumerator() :> IEnumerator



    // This conform to the F# standard of not casting an exception
    // when index is negative or the start surpasses the end index
    member LB.GetSlice(?startindex, ?endindex) =
        let startindex = Option.defaultValue 0 startindex
        let endindex = Option.defaultValue (int LB.file.Length) endindex
        let k = endindex - startindex + 1
        let mutable arr = Array.create k 0uy 
        let mutable j = 0
        let mutable i = startindex
        if startindex >= 0 && endindex > 0 then
            while i <= endindex do
                match LB.Read i with
                | Error _ -> i <- endindex + 1
                | Ok b -> 
                    arr.[j] <- b
                    i <- i + 1
                    j <- j + 1        
        
        if j < k then
            Array.Resize(&arr, j)
        arr
        |> System.Text.Encoding.UTF8.GetString
      
  

    member LB.Dispose() = (LB :> IDisposable).Dispose()

    member LB.Next() = 
        match LB.Read LB.cur with
        | Ok(byte) -> Some byte
        | _ -> None


    member LB.Read i =
        let lowbound = i > LB.page
        if (lowbound && i < LB.page + 1024) || (LB.page - LB.lastPage = 1024 && i >= LB.page - 1024 && not lowbound) then
            if i > LB.count then
                Error (IndexOutOfRangeException("LexBuffer") :> exn)
            else
                Ok(LB.dbuf.[i %% 11])
        else 
            try
                let target = (i/1024)* 1024
                // can through an error here
                LB.cur <- int <| LB.file.Seek(int64 target, SeekOrigin.Begin)
                let p = (i %% 11) >>> 10
                LB.lastPage <- LB.page
                LB.page <- target
                // and here. then offset will either write into the lower or upper
                // half of the array
                LB.count <- LB.page + LB.file.Read(LB.dbuf, 1024 * p, 1024)
                if i > LB.count then
                    printfn "error"
                    Error (IndexOutOfRangeException("LexBuffer") :> exn)
                else
                    Ok(LB.dbuf.[i %% 11])
            with
                | e -> Error e

    member LB.Seek pos =
        LB.cur = pos


