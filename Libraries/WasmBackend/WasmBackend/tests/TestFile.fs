module TestFile

open System
open System.IO

let private dir = __SOURCE_DIRECTORY__

[<AbstractClass>]
type TestFile(fullpath) =
    let mutable file: FileStream = null
    member _.fullpath = fullpath 

    member f.File 
        with get() = file
        and set stream = file <- stream

    abstract member Dispose: unit -> unit

    
    // this is the default implementation
    // it handle removing of the temporary content
    default f.Dispose() =
        f.File.Dispose()
        File.Delete f.fullpath

    interface IDisposable with
        member f.Dispose() = f.Dispose()
        

// a file object that is ment to create and remove
// a temporary file
type TestUTF8File(path, content) as file =
    inherit TestFile(Path.Combine(dir, path))
    do
        file.File <- File.CreateText(file.fullpath).BaseStream() :?> FileStream
        

    let file = File.CreateText(Path.Combine(dir, path))
    
type TestASCIIFILE(path, content) =
    let file = File.Create(Path.Combine(dir, path))
    