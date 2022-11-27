module Ast

// different type variable annotation
#nowarn "62"

type ('i8, 'i16,'i32,'i64, 'u8, 'u16,'u32,'u64, 'f32,'f64) operand =
    | I8  of 'i8
    | I16 of 'i16
    | I32 of 'i32
    | I64 of 'i64
    | U8  of 'u8
    | U16 of 'u16
    | U32 of 'u32
    | U64 of 'u64
    | F32 of 'f32
    | F64 of 'f64


type ('i, 'f)  operator = ('i, 'i,'i,'i, 'i, 'i,'i,'i, 'f, 'f) operand


type Position =
    {
        Linje: int
        Tegn: int
        Total: int
    }


module IntOp =

    type unop = Neg | IsZero | TrailingZeros | LeadingZeros
    type binop = 
        | Plus | Minus | Gange | Division 
        | RykHøjre | RykVenstre | RoterHøjre | RoterVenstre
        | Min | Max 

    type relop = Eq | Ne | Le | Lt | Ge | Gt

    

module FloatOp =

    type unop = Neg | Sqrt | Ceil | Floor | Round
    type binop = Plus | Minus | Gange | Division 

    type relop = Eq | Ne | Le | Lt | Ge | Gt
    

type logop = Og | Eller | EllersKun | Medføre

type unop = (IntOp.unop, FloatOp.unop) operator

type binop = (IntOp.binop, FloatOp.binop) operator

type relop = (IntOp.relop, FloatOp.relop) operator


type 'a liste = 'a list

// et udtryk vil altid returnere en værdi
type ('navn, 'værdi, 'info) udtryk =
    | Værdi    of 'værdi * 'info
    | Variable of 'navn * 'info
    | Unær     of unop * ('navn, 'værdi, 'info) udtryk * 'info
    | Binær    of binop * ('navn, 'værdi, 'info) udtryk *  ('navn, 'værdi, 'info) udtryk * 'info
    | Kald     of 'navn * ('navn, 'værdi, 'info) udtryk liste * 'info
    | Læs      of 'navn * ('navn, 'værdi, 'info) udtryk liste * 'info


type ('navn, 'værdi, 'info) betingelse =
    | Sandhed    of 'værdi * 'info
    | BVar       of 'navn * 'info
    | Ikke       of ('navn, 'værdi, 'info) betingelse
    | Sammenling of relop * ('navn, 'værdi, 'info) udtryk * ('navn, 'værdi, 'info) udtryk * 'info
    | Logisk     of logop * ('navn, 'værdi, 'info) betingelse * ('navn, 'værdi, 'info) betingelse * 'info


type ('navn, 'værdi, 'info) udsagn =
    | Tildel    of 'navn * ('navn, 'værdi, 'info) udtryk * 'info
    | Ændre     of 'navn * ('navn, 'værdi, 'info) udtryk * 'info
    | Hvis      of ('navn, 'værdi, 'info) betingelse * ('navn, 'værdi, 'info) udtryk * ('navn, 'værdi, 'info) udtryk * 'info
    | Kald      of 'navn * ('navn, 'værdi, 'info) udtryk liste * 'info
    | Mens      of ('navn, 'værdi, 'info) betingelse * ('navn, 'værdi, 'info) udsagn liste * 'info
    | Skriv     of 'navn * ('navn, 'værdi, 'info) udtryk liste * 'info
    | Resultat  of ('navn, 'værdi, 'info) udtryk * 'info 


type ('navn, 'værdi, 'info) funktion = 
    {
        Navn: 'navn
        argumenter: 'navn liste
        Krop: ('navn, 'værdi, 'info) udtryk
        Info: 'info
    }


// bitmask representation
type omskrivningsProtocol =
    | optimer1 = 1 
    | optimer2 = 2
    | optimer3 = 4 
    | fejlfind = 8
    | dynamisk = 16 // hvis den er sat vil den indlæse funktioner dynamisk, ellers vil den lave et samlet program.
    | indsæt1  = 32 // udskift kald til små funktioner med dens krop
    | indsæt2  = 64 // udskift alle funktions kald med deres krop det gælder også endelige rekursive kald.


// Et bibliotek er en samling af funktioner, med en angivning af hvilke funktioner der er tilgængelige for en bruger af biblioteket
// hver bibliotek, har desuden et navn, referencer til andre bibliotekter og information omkring hvordan de skal omskrives
// det sidste giver mulighed, for at give programmøren mulighed for at lave micro omskrivnings kontrol, så f.eks. kun dele af et 
// program får fuld optimering eller indsæt/omskrivning.  
type ('navn, 'værdi, 'info) bibliotek =
    {
        Navn: 'navn
        Referencer: ('navn, 'værdi, 'info) bibliotek liste
        Funktioner: ('navn, 'værdi, 'info) funktion liste
        Data: byte[][] // kend størrelse data ved omskrivning
        Synlige: int liste // hvilke funktioner der er synlige i
        OmskrivningsProtocol: omskrivningsProtocol liste
    }


type ('navn, 'værdi, 'info) Program =
    {
        Navn: 'navn
        Referencer: ('navn, 'værdi, 'info) bibliotek liste
        Data: byte[][]
        Adfærd: ('navn, 'værdi, 'info) funktion
        OmskrivningsProtocol: omskrivningsProtocol liste
    }

