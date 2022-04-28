module Memory

#nowarn "9"
open System.Runtime.InteropServices
open FSharp.NativeInterop


let scrollingGraphic =
    [|
        0xCE; 0xED; 0x66; 0x66; 0xCC; 0x0D; 0x00; 0x0B; 0x03; 0x73; 0x00; 0x83; 0x00; 0x0C; 0x00; 0x0D
        0x00; 0x08; 0x11; 0x1F; 0x88; 0x89; 0x00; 0x0E; 0xDC; 0xCC; 0x6E; 0xE6; 0xDD; 0xDD; 0xD9; 0x99
        0xBB; 0xBB; 0x67; 0x63; 0x6E; 0x0E; 0xEC; 0xCC; 0xDD; 0xDC; 0x99; 0x9F; 0xBB; 0xB9; 0x33; 0x3E
    |]
    |> Array.map byte


type Memory =
    {
        size: int
        bytes: nativeint // unmanaged memory pointer
    }
with
    member inline M.Write i value = 
            NativePtr.set (NativePtr.ofNativeInt M.bytes) (int i) value
      

    member inline M.Read i = 
            NativePtr.get (NativePtr.ofNativeInt M.bytes) (int i)

    member M.Free = Marshal.FreeHGlobal M.bytes

let empty = 
    let mem =
        { 
            size =  32 * pown 10 3 
            bytes = Marshal.AllocHGlobal (32 * pown 10 3)
        }
    // not needed ?
    //for i in 0x104us .. 0x133us do
    //    // set scrolling Nintendo graphics
    //    mem.Write i scrollingGraphic.[int i - 0x104]

    mem