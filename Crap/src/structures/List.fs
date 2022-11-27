module List

exception Empty of Message: string

type 'item IList =
    abstract member Empty: 'item IList
    abstract member Head: 'item
    abstract member Tail: IList<'item>
    abstract member Size: uint
    abstract member IsEmpty: bool
    

type 'item EmptyList = EMPTY with
    
    interface 'item IList with
        member E.Empty = EMPTY :> _
        member E.Head = Empty "the list is empty" |> raise
        member E.Tail = (E :> _)
        member E.Size = 0u
        member E.IsEmpty = true

    static member empty = EMPTY :> 'item IList
    

type 'item List =
    {
        item: 'item
        next: 'item IList 
    }
with
    interface 'item IList with
        member E.Empty = EMPTY :> _
        member E.Head = E.item
        member E.Tail = E.next
        member E.Size = E.next.Size + 1u
        member E.IsEmpty = false

    static member Empty = EMPTY :> 'item IList


let cons (item : 'item) (list: 'item IList) : 'item IList =
    {
        item = item
        next = list
    } :> _

let test = cons 1 (cons 2 List.Empty)


