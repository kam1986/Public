module Table

type ('id, 'item) Table = Tab of ('id * 'item) list
with
    static member empty = Tab [] : Table<'id,'item>




let Lookup id (Tab tab) =
    List.tryFind (fun (name, _) -> id = name) tab // find latest binding of id return None if not found
    |> Option.map snd

let Bind id item (Tab tab) =
    (id, item) :: tab
    |> Tab

let Delete id (Tab tab) =
    let rec loop lst l =
        match l with
        | [] -> List.rev lst
        | (x, _) :: xs when x = id -> List.fold (fun xs x ->  x :: xs) xs lst
        | x :: xs -> loop (x::lst) xs
    loop [] tab
    |> Tab

let Union (Tab tab1) (Tab tab2) =
    Tab (tab1 @ tab2)


(*
    let case1 =  Table.empty
    let case2 = List.fold2 Bind case1 ["et"; "to"; "tre"; "to"] [1;2;3;4] 

    let test1 = case1 = Delete case1 "nothing" 
    let test2 = case2 = Delete case2 "nothing"
    let test3 = Delete case2 "to" = Tab (List.rev <| List.zip ["et"; "to"; "tre"] [1;2;3] )
    let test() = assert (test1 && test2 && test3)
*)
