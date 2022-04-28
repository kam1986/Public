module Print

open System.IO


type Channel = StreamWriter

// print formatted text to the channel without linebreak
let cprintf (channel : Channel) =
    fun str -> channel.Write(sprintf str : string)
    
// print formatted text to the channel withlinebreak
let cprintfn (channel : Channel) =
    fun str -> channel.WriteLine(sprintf str : string)


// print formatted text to the channel starting at some indentation without linebreak 
let iprintf channel indentations format =
    for i in 1 .. indentations do
        cprintf channel "    " 
    cprintf channel format

// print formatted text to the channel starting at some indentation with linebreak 
let iprintfn channel indentations format =
    for i in 1 .. indentations do
        cprintf channel "    " 
    cprintfn channel format