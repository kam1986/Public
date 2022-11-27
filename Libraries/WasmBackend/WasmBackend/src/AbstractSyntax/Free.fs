module Free

type zet = int Set

type t =
    {
        types    : zet
        globals  : zet
        tables   : zet
        memories : zet
        funcs    : zet
        elems    : zet
        datas    : zet
        locals   : zet
        labels   : zet
    }

let empty =
    {
         types    = set[]
         globals  = set[]
         tables   = set[]
         memories = set[]
         funcs    = set[]
         elems    = set[]
         datas    = set[]
         locals   = set[]
         labels   = set[]
     }

let types s = {empty with types = s}
let globals s = {empty with globals = s}
let tables s = {empty with tables = s}
let memories s = {empty with memories = s}
let funcs s = {empty with funcs = s}
let elems s = {empty with elems = s}
let datas s = {empty with datas = s}
let locals s = {empty with locals = s}
let labels s = {empty with labels = s}

let (<+>) t1 t2 =
    {
        types    = t1.types    + t2.types   
        globals  = t1.globals  + t2.globals 
        tables   = t1.tables   + t2.tables  
        memories = t1.memories + t2.memories
        funcs    = t1.funcs    + t2.funcs   
        elems    = t1.elems    + t2.elems   
        datas    = t1.datas    + t2.datas   
        locals   = t1.locals   + t2.locals  
        labels   = t1.labels   + t2.labels  
    }

let var x = set[x]
let zero = set[0]
let shift s = Set.map ((+) -1) (Set.remove 0 s)

let list free xs = List.fold (<+>) empty (List.map free xs)
let opt free xo = defaultArg (Option.map free xo) empty


