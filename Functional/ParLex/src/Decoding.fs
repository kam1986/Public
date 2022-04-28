module Decoding

// needed to filter out 0x00 bytes 
let inline GetBytes str =
    Seq.fold 
        ( fun bytes c -> 
            let double = int16 c
            if int16 double > 255s
            then (byte (double >>> 8)) :: (byte double) :: bytes
            else (byte double) :: bytes
        ) [] str
    |> List.rev // this way we minimize memory compsumtion