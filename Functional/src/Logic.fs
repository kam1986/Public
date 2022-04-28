module Logic

    type 'p Predicate = Predicate of ('p -> bool)


    let Evaluate (Predicate p) = p

    // Bind
    let (~&) p = Predicate p

    // Not
    let (!!) p = 
        not << (Evaluate p)
        |> Predicate

    // And
    let ( <&> ) p1 p2 =
        fun domain ->
            Evaluate p1 domain && Evaluate p2 domain
        |> Predicate

    // Or
    let ( <|> ) p1 p2 =
        fun domain ->
            Evaluate p1 domain || Evaluate p2 domain
        |> Predicate

    // Xor
    let ( <^> ) p1 p2 = (p1 <&> !!p2) <|> (p2 <&> !!p1)

    // Imply
    let ( => ) p1 p2 = p1 <|> !!p2


    let ForAll p =
        fun domain ->
            Seq.map (Evaluate p) domain
            |> Seq.fold ( && ) true
        |> Predicate

    
    let Exist p =
        fun domain ->
            Seq.map (Evaluate p) domain
            |> Seq.fold ( || ) false
        |> Predicate

