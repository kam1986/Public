module WasmInstance

open System
open System.IO
open System.Collections.Generic
open System.Reflection
open System.Diagnostics

type Executable() = 
    abstract exitcode: int
    abstract stdout: string[]
    abstract stderr: string[]
    
with
    static let t() = 0

let StartUp workingDirectory exe cmdline =
    try
        let encodingToTranslateToolOutput = System.Text.Encoding.UTF8

        let psi = new ProcessStartInfo(exe,cmdline)

        psi.WorkingDirectory <- workingDirectory
        psi.UseShellExecute <- false
        psi.RedirectStandardOutput <- true
        psi.RedirectStandardError <- true
        psi.CreateNoWindow <- true
        psi.StandardOutputEncoding <- encodingToTranslateToolOutput
        psi.StandardErrorEncoding <- encodingToTranslateToolOutput

        let p = Process.Start psi
        let output = ResizeArray()
        let error = ResizeArray()

        p.OutputDataReceived.Add(fun args -> if args.Data <> null then output.Add(args.Data))
        p.ErrorDataReceived.Add(fun args -> if args.Data <> null then error.Add(args.Data))
        p.BeginErrorReadLine()
        p.BeginOutputReadLine()
        p.WaitForExit()
        { exitcode = p.ExitCode; stdout = output.ToArray(); stderr = error.ToArray() }
    with 
    | :? System.IO.FileNotFoundException
    | :? System.ComponentModel.Win32Exception -> failwith($"The executable: {exe} was not found")
    |_ -> reraise()