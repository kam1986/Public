module Miljø
#nowarn "62"

type miljø<'navn, 'v, 'f, 'k when 'navn: comparison> =
    {
        Værdier: ('navn, 'v) Map
        Funktioner: ('navn, 'f) Map
        Kanaler: ('navn, 'k) Map
    }

type Map<'a,'b when 'a: comparison> with
    static member tryfind navn map =
        match Map.tryFind navn map with
        | None -> Error $"{navn} findes ikke"
        | Some k -> Ok k



let FindVærdi navn miljø = Map.tryfind navn miljø.Værdier
let FindFunktion navn miljø = Map.tryfind navn miljø.Funktioner
let FindKanal navn miljø = Map.tryfind navn miljø.Kanaler

let BindVærdi navn værdi miljø =  { miljø with Værdier = Map.add navn værdi miljø.Værdier }
let BindFunktion navn værdi miljø =  { miljø with Værdier = Map.add navn værdi miljø.Funktioner }
let BindKanal navn værdi miljø =  { miljø with Værdier = Map.add navn værdi miljø.Kanaler }
