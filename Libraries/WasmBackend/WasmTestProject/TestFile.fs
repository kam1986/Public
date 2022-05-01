module TestFile

open System.IO

let private dir = __SOURCE_DIRECTORY__

// a file object that is ment to create and remove
// a temporary file
type TestFile(path, content) =
    let file = new System.IO.File.CreateText(Path.Combine(dir, path))
    