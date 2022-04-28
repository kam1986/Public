
namespace AbstractSyntax


module Wasm = 

 
    open Wasm.Types
    open Wasm.Values
    (*

        The six modules below are used to allow for declaring multiple instances of the same type name and specify how it is ment to be used.
        all but the IntOp.unop are made to enums since this makes the encoder more streamline, and more effecient.
        i.e. integer addition by a constant are must more effecient than jump tables. 
    *)

    module IntOp =
        type unop =  Clz | Ctz  | Popcnt //| ExtendS of packSize  
        type binop = Add  | Sub  | Mul   | Div  | Rem | And | Or | Xor | Shl | Shr | Rotl | Rotr 
        type testop = Eqz 
        type relop = Eq | Ne | Lt | Gt | Le | Ge
        type cvtop = 
            // the double surjectiv encoding scheme are meant to help the compiler optimize encoding
            | WrapI64   
            | TruncF32 
            | TruncF64 
            | Extend

        type atomic = 
            | Add of packSize * extension
            | Sub of packSize * extension
            //| Mul of packSize * extension
            //| Div of packSize * extension
            | And of packSize * extension
            | Or of packSize * extension
            | Xor of packSize * extension
            | Xchg of packSize * extension
            | CmpXchg of packSize * extension
    

        type spec = 
            | ExtendS of packSize  
            | TruncSatF32 
            | TruncSatF64  
            | Reinterpret


    module FloatOp =
        type unop = Neg | Abs | Ceil | Floor | Trunc | Nearest | Sqrt 
        type binop = Add | Sub | Mul | Div | Min | Max | CopySign 
        type testop = NotDefined 
        type relop = Eq | Ne | Lt | Gt | Le | Ge 
        type cvtop = 
            | ConvertSI32 
            | ConvertUI32 
            | ConvertSI64 
            | ConvertUI64 
            | DemoteF64    
            | PromoteF32  
        type atomic = unit // no instructions
        type spec = 
            | Reinterpret


    // wraps the two modules into type and size specific types without shadowing.
    module I32Op = IntOp
    module I64Op = IntOp
    module F32Op = FloatOp
    module F64Op = FloatOp

    // a value is a tag of the four types and some generic value changing compared to the need, in the validation the input will be types, in the execution/reduction it will be actual value instances.
    type unop   = op<I32Op.unop, I64Op.unop, I32Op.unop, I64Op.unop, F32Op.unop, F64Op.unop>
    type binop  = op<I32Op.binop, I64Op.binop, I32Op.binop, I64Op.binop, F32Op.binop, F64Op.binop>
    type testop = op<I32Op.testop, I64Op.testop, I32Op.testop, I64Op.testop, F32Op.testop, F64Op.testop>
    type relop  = op<I32Op.relop, I64Op.relop, I32Op.relop, I64Op.relop, F32Op.relop, F64Op.relop>
    type cvtop  = op<I32Op.cvtop, I64Op.cvtop, I32Op.cvtop, I64Op.cvtop, F32Op.cvtop, F64Op.cvtop>
    type specop = op<I32Op.spec, I64Op.spec, I32Op.spec, I64Op.spec, F32Op.spec, F64Op.spec> 
    type atomic = op<I32Op.atomic, I64Op.atomic, I32Op.atomic, I64Op.atomic, F32Op.atomic, F64Op.atomic>

    type memop<'id, 'a> = { ty:numType; align:int; name: 'id; offset: int; sz: 'a option }

    type 'id loadop =  memop<'id, packSize * extension>
    type 'id storeop = memop<'id, packSize> 

    type name = string

    type 'id blockType = VarBlockType of 'id | ValBlockType of valueType option

    type Location<'id,'fid> =
        | Stack
        | Local  of 'id
        | Global of 'id
        | Table  of 'fid
        | Memory of 'id loadop

    type instr<'id, 'fid, 'mem, 'info> =
        | Unreachable   of 'info                                                                (* trap unconditionally *)
        | Nop           of 'info                                                                (* forget a value *)
        | Select        of valueType list * 'info                                               (* branchless conditional *)
        | Block         of 'id blockType * instr<'id, 'fid, 'mem, 'info> seq  * 'info                 (* execute in sequence *)
        | Loop          of 'id blockType * instr<'id, 'fid, 'mem, 'info> seq * 'info                  (* loop header *)
        | If            of 'id blockType * instr<'id, 'fid, 'mem, 'info> seq * instr<'id, 'fid, 'mem, 'info> seq * 'info   (* conditional *)
        | Br            of 'fid * 'info                                                                                                       (* break to n-th surrounding label *)
        | BrIf          of 'fid * 'info                                                                                                       (* conditional break *)
        | BrTable       of 'fid list * 'fid * 'info                                             (* indexed jump *)
        | Return        of 'info                                                                (* break from function body *)
        | Call          of 'fid * 'info                                                         (* call function *)
        | CallIndirect  of 'fid * 'fid * 'info                                                  (* call function through table *)
        | Load  of 'mem * 'info
        | Store of 'mem * 'info
        | Tee   of 'mem * 'info
        | Grow  of 'mem * 'info
        | Size  of 'mem * 'info
        | Fill  of 'mem * 'info
        | Copy  of 'mem * 'mem * 'info
        | Init  of 'mem * 'mem * 'info
        | Drop  of 'mem * 'info                    
        | RefNull of refType * 'info                                                    (* null reference *)
        | RefFunc of 'fid * 'info                                                       (* function reference *)
        | RefIsNull of 'info                                                            (* null test *)
        | Const of num * 'info                                                                 (* Constant *)
        | Test of testop * 'info                                                        (* numeric test *)
        | Compare of relop * 'info                                                      (* numeric comparison *)
        | Unary of unop * 'info                                                         (* unary numeric operator *)
        | Binary of binop * 'info                                                       (* binary numeric operator *)
        | Convert of cvtop * 'info                                                      (* conversion *)
        | Spec of specop * 'info                                                        // added since this makes the code faster
    with
        static member Info i = 
            match i with
            | Unreachable info
            | Nop info          
            | Select(_, info)        
            | Block(_, _, info)         
            | Loop(_, _, info)         
            | If(_, _, _, info)            
            | Br(_, info)            
            | BrIf(_, info)          
            | BrTable(_, _, info)       
            | Return info       
            | Call(_, info)          
            | CallIndirect(_, _, info)  
            | Load(_, info) 
            | Store(_, info)
            | Tee(_, info)
            | Grow(_, info)
            | Size(_, info)
            | Fill(_, info)
            | Copy(_, _, info)
            | Init(_, _, info)
            | Drop(_, info)
            | RefNull(_, info)
            | RefFunc(_, info)
            | RefIsNull info
            | Const(_, info)
            | Test(_, info)
            | Compare(_, info)
            | Unary(_, info)
            | Binary(_, info)
            | Convert(_, info)
            | Spec(_, info) -> info
            

    // all code below are first converted into the above form before translated into binary
    // or normal wat form
    type SExpression<'id, 'fid, 'mem, 'info> =
        | SUnreachable   of 'info                                                                
        | SNop           of 'info                                                                
        | SSelect        of valueType list * SExpression<'id, 'fid, 'mem, 'info> * SExpression<'id, 'fid, 'mem, 'info> * SExpression<'id, 'fid, 'mem, 'info> * 'info 
        | SBlock         of 'id blockType * SExpression<'id, 'fid, 'mem, 'info>  * 'info           
        | SLoop          of 'id blockType * SExpression<'id, 'fid, 'mem, 'info> * 'info            
        | SIf            of 'id blockType * SExpression<'id, 'fid, 'mem, 'info> * SExpression<'id, 'fid, 'mem, 'info> *  'info   
        | SBr            of 'fid * 'info                                                                                                       
        | SBrIf          of 'fid * 'info                                                                                                       
        | SBrTable       of 'fid list * 'fid * 'info                                             
        | SReturn        of 'info                                                                
        | SCall          of 'fid * 'info                                                         
        | SCallIndirect  of 'fid * 'fid * 'info                                                  
        | SLoad  of 'mem * 'info
        | SStore of 'mem * 'info
        | STee   of 'mem * 'info
        | SGrow  of 'mem * 'info
        | SSize  of 'mem * 'info
        | SFill  of 'mem * 'info
        | SCopy  of 'mem * 'mem * 'info
        | SInit  of 'mem * 'mem * 'info
        | SDrop  of 'mem * 'info                    
        | SRefNull of refType * 'info                                                    
        | SRefFunc of 'fid * 'info                                                       
        | SRefIsNull of 'info                                                            
        | SConst of num * 'info                                                                 
        | SUnary of unop * SExpression<'id, 'fid, 'mem, 'info> * 'info                                                         
        | STest of testop * SExpression<'id, 'fid, 'mem, 'info> * 'info                  
        | SConvert of cvtop * SExpression<'id, 'fid, 'mem, 'info> * 'info                                                      
        | SSpec of specop * SExpression<'id, 'fid, 'mem, 'info> * 'info  
        | SCompare of relop * SExpression<'id, 'fid, 'mem, 'info> * SExpression<'id, 'fid, 'mem, 'info> * 'info                                                      
        | SBinary of binop * SExpression<'id, 'fid, 'mem, 'info> * SExpression<'id, 'fid, 'mem, 'info> * 'info                                                       
        | Seq of SExpression<'id, 'fid, 'mem, 'info> seq
 


    type expr<'id, 'fid, 'mem, 'info>  = Expr of instr<'id, 'fid, 'mem, 'info> list

    type func<'id, 'fid, 'mem, 'info>  =
        {
            ty: int                                // type reference
            name: string
            locals: ('id * valueType) list          // argument list
            body: expr<'id, 'fid, 'mem, 'info>      // function body
        }


    type table = { ty: tableType }

    type memory = { ty: memoryType }

    type Global<'id, 'fid, 'mem, 'info> = { ty: globalType; name: string; init: expr<'id, 'fid, 'mem, 'info>  }

    type elemmode<'id, 'fid, 'mem, 'info> =
        | Passive 
        | Active of {| index: int; offset: expr<'id, 'fid, 'mem, 'info>  |}
        | Declarative

    type element<'id, 'fid, 'mem, 'info>  =
        {
            ty: refType
            init: expr<'id, 'fid, 'mem, 'info>  list
            mode: elemmode<'id, 'fid, 'mem, 'info> 
        }

    type datamode<'id, 'fid, 'mem, 'info>  = Passive | Active of {| memory: int; offset: expr<'id, 'fid, 'mem, 'info> |}

    type data<'id, 'fid, 'mem, 'info>  = 
        {
            init: byte []
            mode: datamode<'id, 'fid, 'mem, 'info> 
        }

    type 'id Start = { func: 'id option}


    type exportdesc =
        | ExFunc   of int
        | ExTable  of int
        | ExMemory of int
        | ExGlobal of int

    type export = 
        {
            name: string
            desc: exportdesc
        }

    type importdesc = 
        | ImFunc of int
        | ImTable of tableType
        | ImMemory of memoryType
        | ImGlobal of globalType

    type import =
        {
            modulename: name
            name: name
            desc: importdesc
        }


    // to easy redefine the type
    type Vec<'a> = 'a seq

    type Module<'id, 'fid, 'mem, 'info>  =
        {
            types:     Vec<funcType>
            funcs:     Vec<func<'id, 'fid, 'mem, 'info>>
            tables:    Vec<table>
            memories:  Vec<memory>
            globals:   Vec<Global<'id, 'fid, 'mem, 'info> >
            elements:  Vec<element<'id, 'fid, 'mem, 'info> >
            datas:     Vec<data<'id, 'fid, 'mem, 'info> >
            imports:   Vec<import>
            export:    Vec<export>
            start:     'fid Start
        }
    with
        static member inline empty() = 
            { 
                types       = []
                funcs       = []
                tables      = []
                memories    = []
                globals     = []
                elements    = []
                datas       = []
                imports     = []
                export      = []
                start       = { func = None }           
            }
