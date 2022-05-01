module Validate
(*

   This is an imperative implementation of the validator of wasm modules

*)


open Wasm
open Types
open Values
open Helpers

// we use arrays rather than list for effecientcy reasons
// there is little to no adding of objects to the context
// but many lookups
type funcidx = int

let [<Literal>] tabmax = 4294967295u
let [<Literal>] memmax = 65536u


type Context = 
    {
        types: funcType list
        funcs: funcType list
        tables: tableType list
        memories: memoryType list
        globals: globalType list
        elements: refType list
        data: unit list
        refs: funcidx list
    }
with
    static member empty : Context =
        {
            types = []
            funcs = []
            tables = []
            memories = []
            globals = []
            elements = []
            data = []
            refs = []
        }

let Lookup category (lst: _ list) x =
    try
        lst.[x]
    with _ -> error $"unknown {category} {x}"


let Type context x = Lookup "type" context.types x
let Func context x = Lookup "function" context.funcs x
let Table context x = Lookup "table" context.tables x
let Memory context x = Lookup "memory" context.memories x
let Global context x = Lookup "global" context.globals x
let Element context x = Lookup "elem segment" context.elements x
let Data context x = Lookup "data segment" context.data x

type Type = valueType option

let IsNum = 
    function
    | None 
    | Some (NumType _) -> true
    | _ -> false


let IsRef =
    function
    | None 
    | Some (RefType _) -> true
    | _ -> false

type Op<'i,'f> =
    | Integer of 'i
    | Float of 'f

let GetOp =
    function
    | I32 op | I64 op | U32 op | U64 op -> Integer op
    | F32 op | F64 op -> Float op


let GetIntCvtOpType = function
    | IntOp.Extend   -> I32()
    | IntOp.WrapI64  -> I64()
    | IntOp.TruncF32 -> F32()
    | IntOp.TruncF64 -> F64()


let GetFloatCvtOpType = function
    | FloatOp.ConvertSI32 
    | FloatOp.ConvertUI32 -> I32()
    | FloatOp.ConvertSI64 
    | FloatOp.ConvertUI64 -> I64()
    | FloatOp.PromoteF32 -> F32()
    | FloatOp.DemoteF64 -> F64()



let GetCvtOpType (op: cvtop) : Type =
    match GetOp op with
    | Integer op -> GetIntCvtOpType op
    | Float op -> GetFloatCvtOpType op
    |> NumType
    |> Some

let GetIntSpcOp op: Type = 
    match op with
    | IntOp.spec.TruncSatF32 -> F32()
    | IntOp.spec.TruncSatF64 -> F64()
    | _ -> I32()
    |> NumType
    |> Some
    
type 'a Stack =
    val mutable data: 'a option[]
    val mutable top: int

    new(_) = { data = [| for _ in 1 .. 8 -> None |]; top = 0 }

    member s.Push item =
        s.top <- s.top + 1

        if s.top = s.data.Length then
            let tmp = [| for _ in 1 .. (s.data.Length <<< 1) -> None |]
            for i in 0 .. s.data.Length - 1 do
                tmp.[i] <- s.data.[i]
            s.data <- tmp

        s.data.[s.top] <- Some item

    member s.Pop() =
        // check if the stack is out of bound
        Require (s.top >= 0) "Empty stack"
        
        // return value
        let ret = s.data.[s.top]

        // since the size are a power of 2, a size/4 is just a 2  x shift to the right
        let sz = s.data.Length >>> 2
        if s.top < sz then
            let tmp = [| for _ in 1 .. sz -> None |]
            for i in 0 .. tmp.Length - 1 do
                s.data.[i] <- tmp.[i]
            s.data <- tmp

        s.top <- s.top - 1
        ret

    member s.Size = s.top

    member s.Item
        with get i =
            Require (s.top >= 0 && i <= s.top) "stack index out of bound"
            s.data.[s.top - i].Value
        
    member s.IsEmty() = s.top < 0

    member s.IsNotEmpty() = s.top >= 0

    member s.Resize i = s.top <- s.top - i

// type instance
type stack = Type Stack

let mutable vals: stack = Stack()

type CtrlFrame =
    val opcode: instr<int,int, Location<int,int>, unit>
    val starttypes: valueType list
    val endtypes: valueType list
    val mutable unreachable: bool
    val height: int

    new(opcode, input, output, unreachable) = 
        {
            opcode = opcode
            starttypes = input
            endtypes = output
            unreachable = unreachable
            height = vals.Size
        }

type CtrlStack = CtrlFrame Stack


// global variables
let mutable ctrls: CtrlStack = Stack()
let mutable callframes: CtrlStack = Stack()

let ResetStack() = vals <- Stack()
let ResetControlStack() = ctrls <- Stack()
let ResetCallStack() = callframes <- Stack()


let Known ts = List.map Some ts

let PushVal t = vals.Push t


let PopVal() : Type = 
    if vals.Size = ctrls.[0].height && ctrls.[0].unreachable then
        None
    else
        Require (vals.Size <> ctrls.[0].height) "the size of the stack does not coenside with the height"
        Option.flatten (vals.Pop()) // flatten to simplify code below


let PopExpectedVal (expect: Type) =
    let actual = PopVal()
    if actual.IsNone then
        expect
    elif expect.IsNone then
        actual
    else
        Require 
            (actual = expect) 
            $"expected {ppValueType expect.Value} {ppValueType actual.Value}"
        actual


let PushVals types = List.iter PushVal types


let PopExpectedVals types =
    let mutable popped = []
    for t in List.rev types do
        popped <- PopExpectedVal t :: popped
    popped

// shorthand for popping and pushing specific types'
// [t1; t2] --> [t3]
// means that we check that t2 is on the top of the stack then check it for t1
// if that is correct then push t3 to the stack
let (-->) input output =
    PopExpectedVals input |> ignore
    PushVals output

let PushCtrl opcode input output =
    let frame = CtrlFrame(opcode, input, output, false)
    ctrls.Push frame
    Known input --> []


let PopCtrl() =
    let frame = ctrls.[0]
    Known frame.endtypes --> []
    Require (vals.Size = frame.height) "stack underflow"
    ctrls.Pop().Value // if ctrls.[0] not through an error it will always be a 


let Call opcode input output =
    let frame = CtrlFrame(opcode, input, output, false)
    callframes.Push frame
    Known input --> []


let Return() =
    let frame = callframes.[0]
    Known frame.endtypes --> []
    Require(vals.Size = frame.height) "stack underflow"
    callframes.Pop().Value
    

let LabelTypes (frame: CtrlFrame) =
    match frame.opcode with
    | Loop _ -> Known frame.starttypes
    | _ -> Known frame.endtypes

let ReturnType (frame: CtrlFrame) = Known frame.endtypes

let Unreachable() = 
    vals.Resize ctrls.[0].height
    ctrls.[0].unreachable <- true

let i32 : Type = NumType(I32()) |> Some
let any = None

// used to simplify code heavily 
// by minimizing cases for binary, unary and compare opcodes to 1 each
let GetType op : Type =
    match op with
    | I32 _ | U32 _ -> I32()
    | I64 _ | U64 _ -> I64()
    | F32 _ -> F32()
    | F64 _ -> F64()
    |> NumType
    |> Some

let GetRefType rt: Type = 
    rt
    |> RefType
    |> Some

let SizeOf m = 
    match m.sz with
    | None -> 
        match m.ty with
        | I32 _ | U32 _ | F32 _ -> 4
        | _ -> 8
    | Some (p, _) -> 
        match p with
        | Pack8 -> 1
        | Pack16 -> 2
        | Pack32 -> 4

let CheckBlockType (context: Context) bt =
    match bt with
    | ValBlockType t -> FuncType(Result [], Result (Option.toList t))
    | VarBlockType idx ->
        Require (context.types.Length > idx) ""
        context.types.[idx]


let rec Validate (context: Context) opcode =
    match opcode with
    | Nop _ -> 
        [] --> []
    | Drop (Stack, _) -> 
        [] --> [any]
    | Unreachable _ -> 
        Unreachable()

    | Binary(op, _) ->
        let t = GetType op
        [t; t] --> [t]
        

    | Unary(op, _) ->
        let t = GetType op
        [t] --> [t]

    | Test(op, _) ->
        let t = GetType op
        [t] --> [i32]

    | Compare(op, _) ->
        let t = GetType op
        [t; t] --> [i32]

    | Convert(F32 FloatOp.PromoteF32, _) -> error "Illegal instruction f32.promotef32"
    | Convert(F64 FloatOp.DemoteF64, _) -> error "Illegal instruction f64.demotef64"
    | Convert(op, _) ->
        let t1 = GetType op
        let t2 = GetCvtOpType op
        [t2] --> [t1]

    | Spec(I64 IntOp.Reinterpret, _) -> 
        let t1 = GetType (I64())
        let t2 = GetType (F64())
        [t2] --> [t1]
        

    | Spec(I32 IntOp.Reinterpret, _) -> 
        let t1 = GetType (I32())
        let t2 = GetType (F32())
        [t2] --> [t1]
        

    | Spec(F64 FloatOp.Reinterpret, _) -> 
        let t1 = GetType (F64())
        let t2 = GetType (I64())
        [t2] --> [t1]
        

    | Spec(F32 FloatOp.Reinterpret, _) -> 
        let t1 = GetType (F32())
        let t2 = GetType (I32())
        [t2] --> [t1]
       

    | Spec(I32 (IntOp.ExtendS Pack32), _) -> error "Illegal instruction i32.extendi32_s"

    | Spec(I32 op, _) ->
        let t1 = GetType (I32())
        let t2 = GetIntSpcOp op
        [t2] --> [t1]
        

    | Spec(I64 op, _) ->
        let t1 = GetType (I64())
        let t2 = GetIntSpcOp op
        [t2] --> [t1]
        
    | RefNull(rt, _) ->
        GetRefType rt
        |> PushVal

    | RefIsNull _ ->
        let t = PopVal()
        Require (IsRef t) "Not a reference type for ref.is_null"
        [] --> [i32]

    | RefFunc(x, _) ->
        Require (x < context.funcs.Length) $"function {x} are not defined"
        Require (Seq.contains x context.refs) $"the reference {x} does not refere to a legal point"
        [] --> [GetRefType refType.FuncRef]
        


    | Load(Local x, _) ->
        Require (x < callframes.[0].starttypes.Length) $"Local variabel {x} not defined"
        let t = callframes.[0].starttypes.[x] |> Some
        [] --> [t] 

    | Store(Local x, _) ->
        Require (x < callframes.[0].starttypes.Length) $"Local variabel {x} not defined"
        let t = callframes.[0].starttypes.[x] |> Some
        [t] --> []

    | Tee(Local x, _) ->
        Require (x < callframes.[0].starttypes.Length) $"Local variabel {x} not defined"
        let t = callframes.[0].starttypes.[x] |> Some
        [t] --> [t]
        
    | Load(Global x, _) ->
        Require (x < context.globals.Length) $"Global variabel {x} not defined"
        let (GlobalType(t, _)) = context.globals.[x]
        [] --> [Some t] 

    | Store(Global x, _) ->
        Require (x < context.globals.Length) $"Global variabel {x} not defined"
        let (GlobalType(t, m)) = context.globals.[x]
        Require (m = Mutable) $"Global variable {x} are not mutable"
        [Some t] --> []

    | Tee(Global x, _) -> // allowed for smaller internal representation
        Require (x < context.globals.Length) $"Global variabel {x} not defined"
        let (GlobalType(t, m)) = context.globals.[x]
        Require (m = Mutable) $"Global variable {x} are not mutable"
        let t = Some t
        [t] --> [t]

    | Load(Table x, _) ->
        Require (x < context.tables.Length) $"Table {x} not defined"
        let (TableType(_, rt)) = context.tables.[x]
        [i32] --> [GetRefType rt]

    | Store(Table x, _) ->
        Require (x < context.tables.Length) $"Table {x} not defined"
        let (TableType(_, rt)) = context.tables.[x]
        [i32; GetRefType rt] --> []

    | Tee(Table _, _) -> error "Illegal instruction table.tee"

    | Size(Table x, _) ->
        Require (x < context.tables.Length) $"Table {x} not defined"
        let (TableType(_, rt)) = context.tables.[x]
        [] --> [i32]

    | Grow(Table x, _) ->
        Require (x < context.tables.Length) $"Table {x} not defined"
        let (TableType(_, rt)) = context.tables.[x]
        [GetRefType rt; i32] --> [i32]

    | Fill(Table x, _) ->
        Require (x < context.tables.Length) $"Table {x} not defined"
        let (TableType(_, rt)) = context.tables.[x]
        [i32; GetRefType rt; i32] --> []

    | Copy(Table x, Table y, _) ->
        Require (x < context.tables.Length) $"Table {x} not defined"
        let (TableType(_, rtx)) = context.tables.[x]
        Require (y < context.tables.Length) $"Table {y} not defined"
        let (TableType(_, rty)) = context.tables.[y]
        Require (rty = rtx) "Table reference types are not the same"
        [i32; i32; i32] --> []

    | Init(Table x, Table y, _) ->
        Require (x < context.tables.Length) $"Table {x} not defined"
        let (TableType(_, rtx)) = context.tables.[x]
        Require (y < context.elements.Length) $"Element {y} not defined"
        let rty = context.elements.[y]
        Require (rty = rtx) "Table reference types are not the same"
        [i32; i32; i32] --> []

    | Drop(Table x, _) ->
        Require (x < context.elements.Length) $"Element {x} not defined"

    | Load(Memory m, _) ->
        Require (context.memories.Length > 0) "Memory is not defined"
        Require (1 <<< m.align <= SizeOf m) "Alginment out of bound"
        [i32] --> [GetType m.ty]

    | Store(Memory m, _) ->
        Require (context.memories.Length > 0) "Memory is not defined"
        Require (1 <<< m.align <= SizeOf m) "Alginment out of bound"
        [i32; GetType m.ty] --> []

    | Size(Memory m, _) ->
        Require (context.memories.Length > 0) "Memory is not defined"
        [] --> [i32]

    | Grow(Memory m, _) ->
        Require (context.memories.Length > 0) "Memory is not defined"
        [i32] --> [i32]

    | Fill(Memory _, _) 
    | Copy(Memory _, _, _) ->
        Require (context.memories.Length > 0) "Memory is not defined"
        [i32; i32; i32] --> [i32]

    | Init(Memory m, _, _) ->
        Require (context.memories.Length > 0) "Memory is not defined"
        Require (context.data.Length > m.name) "Memory is not defined"
        [i32; i32; i32] --> [i32]

    | Drop(Memory m, _) ->
        Require (context.data.Length > m.name) "Memory is not defined"
        [] --> []


    | Select([], _) ->
        PopExpectedVal i32 |> ignore
        let t1 = PopVal()
        let t2 = PopVal()
        Require (IsNum t1 && IsNum t2) "Not a number type"
        Require (t1 = t2 || t1 = None || t2 = None) "Not same types"
        [] --> [if t1 = None then t2 else t1] 


    | Select([t], _) ->
        PopExpectedVal i32 |> ignore
        PopExpectedVals (Known [t; t]) |> ignore
        [] --> [Some t]

    | Select _ ->
        error "the number of size of the vector of types of select must be less or equal to 1"

    | Block(bt, body, _)
    | Loop(bt, body, _) ->
        let (FuncType(Result input, Result output)) = CheckBlockType context bt
        Known input --> []
        PushCtrl opcode input output
        // validate body
        ValidateBody context body
        PopCtrl() |> ignore

    | If(bt, tbody, fbody, _) ->
        let (FuncType(Result input, Result output)) = CheckBlockType context bt
        Known input --> []
        // validate true branch
        PushCtrl opcode input output
        ValidateBody context tbody
        PopCtrl() |> ignore
        
        // validate false branch
        PushCtrl opcode input output
        ValidateBody context fbody
        PopCtrl() |> ignore


    | Br(l, _) ->
        Require (l < ctrls.Size) "Label out of bound"
        LabelTypes ctrls.[l] --> []

    | BrIf(l, _) ->
        Require (l < ctrls.Size) "Label out of bound"
        (LabelTypes ctrls.[l] @ [i32]) --> LabelTypes ctrls.[l]

    | BrTable(ls, l, _) ->
        [i32] --> []
        Require (l < ctrls.Size) "Label out of bound"
        let ts = LabelTypes(ctrls.[l])
        let arity = ts.Length
        for l in ls do
            Require (l < ctrls.Size) "Label out of bound"
            let ts = LabelTypes(ctrls.[l])
            Require (ts.Length = arity) "Label out of bound"
            ts --> ts
        ts --> []
        Unreachable()

    | Call(x, _) ->
        Require (x < context.funcs.Length) "Function not defined"
        let (FuncType(Result locals, Result rets)) = context.funcs.[x]
        Call opcode locals rets
        [] --> Known rets

    | CallIndirect(x, y, _) ->
        Require (x < context.tables.Length) $"Table {x} is not defined"
        let (TableType(_,t)) = context.tables.[x]
        Require (t = refType.FuncRef) $"The reference are not a funcref"
        Require (y < context.types.Length) $"Type {y} are not defined"
        let (FuncType(Result locals, Result rets)) = context.types.[y]
        [i32] --> []
        Call opcode locals rets
        [] --> Known rets

    | Return _ ->
        Require (callframes.IsNotEmpty()) "No return point given"
        let rets = ReturnType callframes.[0]
        Return() |> ignore
        [] --> rets

and ValidateBody context opcodes = Seq.iter (Validate context) opcodes
   

let ValidateExpression context (Expr e) =
    ValidateBody context e


let IsConstant (context: Context) opcode =
    match opcode with
    | Const _ -> true
    | RefNull _ -> true
    | RefFunc _ -> true
    | Load(Global x, _) -> 
        Require (x < context.globals.Length) $"the global {x} not define"
        let (GlobalType(_, m)) = context.globals.[x]
        m = Immutable

        
    | _ -> false

let ValidateConstantExpression context (Expr e) =
    Require (List.forall (IsConstant context) e) "Not a constant expression"
    ValidateExpression context (Expr e)


let ValidateGlobal (context: Context) (glb: Global<_,_,_,_>) =
    let (GlobalType(t, m)) = glb.ty
    ValidateConstantExpression context glb.init
    Known [t] --> []
    Require (vals.IsEmty()) "Expected the stack to be empty"

let ValidateLimits k lim =
    let m = defaultArg lim.max (uint k)
    Require (m <= k && lim.min <= m) $"The limit must not be bigger than {k}"


let ValidateTableType (TableType(lim, _)) =
    ValidateLimits tabmax lim


let ValidateMemoryType (MemoryType lim) =
    ValidateLimits memmax lim


let ValidateExternalDescribtion (context: Context) = function
    | ExFunc idx -> 
        Require (idx < context.types.Length) $"The external function {idx} are not defined"
    
    | ExTable idx ->  
        Require (idx < context.tables.Length) $"The external table {idx} are not defined"
        
    | ExMemory idx ->
        Require (idx < context.tables.Length) $"The external table {idx} are not defined"

    | ExGlobal idx ->
        Require (idx < context.tables.Length) $"The external table {idx} are not defined"


let ValidateExternal (context: Context) (ext: export) =
    ValidateExternalDescribtion context ext.desc



let ValidateImportDescribtion (context: Context) = function
    | ImFunc x -> Require (x < context.types.Length) $"The imported function {x} are not defined"
    | ImTable tt -> ValidateTableType tt
    | ImGlobal _ -> ()
    | ImMemory mt -> ValidateMemoryType mt
    
let ValidateImport context imp =
    ValidateImportDescribtion context imp.desc



let ValidateMemory (mem: memory) = ValidateMemoryType mem.ty
let ValidateTable (tab: table) = ValidateTableType tab.ty


let ValidateElemMode (context: Context) rt = function
    | elemmode.Active mode ->
        Require (mode.index < context.tables.Length) $"The table {mode.index} is not defined"
        let (TableType(_, t)) = context.tables.[mode.index]
        Require (t = rt) $"The element does not have the proper type"
        ValidateConstantExpression context mode.offset
    | _ -> () 


let ValidateElement context (elm: element<_,_,_,_>) = 
    List.iter (ValidateConstantExpression context) elm.init
    ValidateElemMode context elm.ty elm.mode


let ValidateDataMode (context: Context) = function
    | Passive -> ()
    | Active mode -> 
        Require (mode.index < context.memories.Length) $"The memory {mode.index} is not defined"
        ValidateConstantExpression context mode.offset
        [i32] --> []
        Require (vals.IsEmty()) "Data segment are supposed to return [i32]"
        

let ValidateData context data =
    ValidateDataMode context data.mode



let ValidateStart (context: Context) start =
    match start.func with
    | None -> ()
    | Some f ->
        Require (f < context.funcs.Length) "the start function are not defined"
        Require 
            (context.funcs.[f] = FuncType(Result [], Result [])) 
            $"The start function does not have the proper type: [] -> [], but: {ppFunctionType context.funcs.[f]}" 

let ValidateFunction (context: Context) (func: func<_,_,_,_>) =
    let (FuncType(Result locals, Result rets)) = Type context func.ty
    let op = instr.Call(func.ty, ())
    Call op locals rets // setup valuestack and callstack
    ValidateExpression context func.body // validate
    Return() // validate return
    |> ignore

let ValidateModule (m: Module<_,_,_,_>) = 
    let { 
        types = tys; imports = imp; 
        tables = tas; memories = ms; 
        globals = gs; funcs = fs;
        start = s; elements = es;
        datas = d;
        } = m
    // setup context to test imports
    { Context.empty with
        types = tys
        // refs OBS need to set this correctly
    }
    |> Seq.foldBack (fun imp context ->
        ValidateImport context imp
        match imp.desc with
        | ImFunc x -> { context with funcs = Type context x :: context.funcs }
        | ImTable tt -> { context with tables = tt :: context.tables }
        | ImGlobal gt -> { context with globals = gt :: context.globals }
        | ImMemory mt -> { context with memories = mt :: context.memories }
        ) imp
    |> fun context ->
        { context with
            funcs = context.funcs @ List.map (fun (f: func<_,_,_,_>) -> Type context f.ty) fs
            tables = context.tables @ List.map (fun (t: table) -> t.ty) tas
            memories = context.memories @ List.map (fun (m: memory) -> m.ty ) ms
            elements = List.map (fun elm -> elm.ty ) es
            data = List.map (fun _ -> ()) d
            globals = context.globals @ List.map (fun (g: Global<_,_,_,_>) -> g.ty) gs
        }
    |> fun context ->
        List.iter (ValidateGlobal context) gs
        List.iter ValidateTable tas
        List.iter ValidateMemory ms
        List.iter (ValidateElement context) es
        List.iter (ValidateData context) d
        List.iter (ValidateFunction context) fs
        ValidateStart context s
        Require (context.memories.Length <= 1) "multiple memories are not allowed yet"
