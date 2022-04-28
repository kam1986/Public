// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System

open Screen
open Decode

[<EntryPoint>]
let main argv =
    
    using (new Screen()) (fun screen -> 
        screen.Run()
    )

    0 // return an integer exit code