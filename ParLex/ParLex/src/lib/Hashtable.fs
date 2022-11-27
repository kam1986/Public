module HashTable

(* 

    a resizeable hashtable with effecient hashing and a amortized O(1) lookup time
    it will at anytime on average have at most 2 item per entry

*)
type HashTable<'key, 'item when 'key: comparison> =
    val mutable table : ('key * 'item) list[]
    val mutable sz : int
    new() = { table = [| []; [] |]; sz = 2 }
    internal new(arr) = {table = arr; sz = Array.sumBy List.length arr }
with

    override h.ToString() =
        List.concat h.table
        |> List.distinctBy (fun (key, _) -> key)
        |> List.sortBy (fun (key, _) -> key)
        |> List.map string
        |> List.filter (fun t -> t <> "")
        |> String.concat "\n"
        |> fun table -> "{" + table + "}"

    
    static member empty = HashTable<'key,'item>()


    member internal h.grow() =
        let arr = Array.init (h.table.Length <<< 1) (fun _ -> [])
        let table = HashTable (arr)
        for row in h.table do
            for (key, item) in row do
                table.[key] <- item
        h.table <- table.table
        h.sz <- table.sz


    member h.add key item =
        let sz = Array.sumBy List.length h.table
        // if there are more than avagered 2 items in each list
        if (h.table.Length <<< 1) < h.sz then h.grow()

        // could maybe be optimized since length always are a power of 2 but the compiler should do that by itself
        let l = &h.table.[abs (key.GetHashCode() % h.table.Length)]
        match h.[key] with
        | None -> h.sz <- h.sz + 1
        | _ -> ()
        
        l <- (key, item) :: l


    member h.Item
        with get key = 
            List.tryFind (fun (k, _) -> key = k) h.table.[abs (key.GetHashCode() % h.table.Length)]
            |> Option.map snd

        and set key item = h.add key item
