module Iter

open Return
open Decoding
open Position

open System
open System.IO
open System.Text

(*

/// This is an interface that allow us to iterate and create iterators
/// over diferent types of collections, with added informations.
/// 
/// It uses Result type over option since it might has an underlying
/// stream or so that can report some error in a textual manor
/// which are to be carried to the top and handled
type 'I Iter =
    abstract member Next : Result<'I * 'I Iter, string>
    abstract member IsEmpty : bool
    abstract member CurrentPos : Position with get, set
    abstract member GetSlice : int * int -> string

let Next (iter : 'I Iter) = iter.Next
let IsEmpty (iter: 'I Iter) = iter.IsEmpty
let CurrentPos (iter: 'I Iter) = iter.CurrentPos

[<Struct>]
type FromString =
    val private buf : byte []
    val private currentpos : Position
    private new(xs, pos) = {buf = xs; currentpos = pos}
    new(str : string) = 
        let bytes = GetBytes str |> List.toArray

        { buf = bytes ; currentpos = start()} 

   
    interface byte Iter with
        member L.Next =
            match Array.tryItem L.currentpos.Absolut L.buf with
            | None -> Error "EOF"
            | Some x when char x = '\n' -> Ok(x, FromString(L.buf, Position(L.currentpos.Line + 1, 0, L.currentpos.Absolut + 1)) :> _)

            | Some x ->  Ok(x, FromString(L.buf, Position(L.currentpos.Line, L.currentpos.Offset + 1, L.currentpos.Absolut + 1)) :> _)
 

        member L.GetSlice(i,j) = 
            L.buf.[i .. j]
            |> System.Text.Encoding.Default.GetString
        
        member L.IsEmpty = Array.isEmpty L.buf



        member L.CurrentPos 
            with get() = L.currentpos
            and set pos = ()

[<Struct>]
type 'I lst =
    val private buf : 'I list
        new(cs) = { buf = Seq.toList cs }
    interface 'I Iter with
    
        member L.GetSlice(i, j) = ""
         
        member L.Next =
            match L.buf with
            | []      -> Error "EOF"
            | x :: xs -> Ok(x, lst(xs) :> _)
    
    
        member L.IsEmpty = List.isEmpty L.buf

 

        member I.CurrentPos
         with get() = start()
         and set pos = ()

/// buffer all content from a file to be iterated over by the lexer
/// not intented for big files.
///
/// This iterator iterate immutably over a filecontent instance
/// Here the immutability is meant as the iterator hold on location i in
/// the content will still be reading location i every time, even though
/// another the iterator has read past that point.
/// the iterator is buffered over bounderies such that it minimize
/// the need for IO calls to the kernel
///
/// The type is a struct type, to optimize memory usage and placement
[<Struct>]
type FromFile =
    val content: Buffer.LexBuffer
    val mutable currentpos: Position
    val mutable startpos : Position
    new(path :string) = 
        {
            content = new Buffer.LexBuffer(path)
            currentpos = start()
            startpos = start()
        }

    interface System.IDisposable with
        member F.Dispose() = F.content.Dispose()

    interface byte Iter with
        member F.CurrentPos
            with get() = F.currentpos
            and set pos = F.currentpos <- pos


        member F.IsEmpty = 
            match F.content.Read(Absolut F.currentpos) with
            | Error _ -> true // any error will result in an empty iterator
            | _ -> false

        member F.Next =
            // it is importance to notice that we use
            // the absolute position to index
            // hence we only need to update position to
            // step forth or back in the file
            match F.content.Read(Absolut F.currentpos) with
            | Error _ -> Error "EOF"
            | Ok b ->  
                if char b = '\n' then
                    newline &F.currentpos
                else
                    next &F.currentpos
                Ok(b, F :> _)

        // since the lexbuffer are buffered
        // this is nearly never an IO operation
        member F.GetSlice(i, j) = F.content.GetSlice(i, j)               
            
*)