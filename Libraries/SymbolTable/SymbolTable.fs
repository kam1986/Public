module SymbolTable



type ISymbolic<'id,'item when 'id: equality and 'id: comparison> =
    abstract member Empty: unit -> ISymbolic<'id,'item> 
    abstract member Bind: 'id -> 'item -> ISymbolic<'id,'item>
    abstract member LookUp: 'id -> Result<'item, string>
    abstract member Delete: 'id -> ISymbolic<'id,'item>
    

let Bind (tab: ISymbolic<_,_>) name item = tab.Bind name item
let Delete (tab: ISymbolic<_,_>) name = tab.Delete name
let LookUp (tab: ISymbolic<_,_>) name = tab.LookUp name


type SymbolList<'id,'item when 'id: equality and 'id: comparison> = SymTable of ('id * 'item) list with
    static member Empty() = SymTable [] :> ISymbolic<'id,'item>
    member tab.Bind name item = 
        let (SymTable tab) = tab
        SymTable ((name, item) :: tab)

    member tab.Delete name =
        let (SymTable tab) = tab
        let rec loop acc tab =
            match tab with
            | [] -> List.rev acc
            | (n, _) :: tab when n = name -> List.fold (fun tab t -> t :: tab) tab acc
            | t :: tab -> loop (t :: acc) tab

        loop [] tab
        |> SymTable


    member tab.LookUp name =
        let (SymTable tab) = tab
        let rec loop tab =
            match tab with
            | [] -> Error $"Look up error:\n\t the id {name} is not defined\n"
            | (n, v) :: _ when n = name -> Ok v
            | _ :: tab -> loop tab
        loop tab


    interface ISymbolic<'id,'item> with
        member _.Empty() = SymbolList<'id,'item>.Empty() 
        member tab.Bind name item = tab.Bind name item :> _
        member tab.Delete name = tab.Delete name :> _
        member tab.LookUp name = tab.LookUp name 


let map s = Map.ofSeq s

type SymbolMap<'id,'item when 'id: equality and 'id: comparison> = SymMap of Map<'id,'item> with
    static member Empty() = SymMap (map[]) :> ISymbolic<'id, 'item>
    member tab.Bind name item = 
        let (SymMap tab) = tab
        Map.add name item tab
        |> SymMap
    
    member tab.Delete name =
        let (SymMap tab) = tab
        Map.remove name tab
        |> SymMap

    member tab.LookUp name =
        let (SymMap tab) = tab
        match Map.tryFind name tab with
        | None -> Error $"Look up error:\n\t the id {name} is not defined\n"
        | Some item -> Ok item

    interface ISymbolic<'id,'item> with
        member _.Empty() = SymbolMap<'id,'item>.Empty()
        member tab.Bind name item = tab.Bind name item :> _
        member tab.Delete name = tab.Delete name :> _
        member tab.LookUp name = tab.LookUp name 


type DynamicTable<'id,'item when 'id: equality and 'id: comparison> = Table of count: int * ISymbolic<'id,'item>
with    
    static member Empty() = Table(0, SymTable []) :> ISymbolic<'id,'item>

    member tab.Bind name item =
        let (Table(count, tab)) = tab
        if count = 150 then
            let (SymTable tab) = tab :?> SymbolList<_,_>
            Table(151, (SymMap (map tab)).Bind name item) 
        else
            Table(count+1, tab.Bind name item) 

    member tab.Delete name = 
        let (Table(count, tab)) = tab
        if count = 100 then
            let (SymMap tab) = tab :?> SymbolMap<_,_>
            Table(99, SymTable (Map.toList <| Map.remove name tab)) 
        else
            let tab' = tab.Delete name
            if tab' <> tab then
                Table(count-1, tab') 
            else
                Table(count, tab) 

    member tab.LookUp name = 
        let (Table(_, tab)) = tab
        tab.LookUp name 

    interface ISymbolic<'id,'item> with
        member tab.Empty() = DynamicTable<'id,'item>.Empty() 
        member tab.Bind name item = tab.Bind name item :> _
            
        member tab.Delete name = tab.Delete name :> _
        member tab.LookUp name = tab.LookUp name 


