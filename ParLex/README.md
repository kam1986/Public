# Documentation
This is the documentation of the Library part of the Lexer and Parser genereator.


## Good to know
This library contains both in code generators which do not genereator an explicit .dll file, but rather create some
data structures and functions, given an array of regex pattern token token and a possible action. This makes the F# compiler
able to see the user defined lexer/parser, without generating some file first. 

The code generator is just like any other regular generator, given some file in the proper format it will generate a code file (for now only in F# code) to be compiled.

Both types uses jumptables to lex and parse code which makes them rather fast and compact.

The lexer run code at a byte level, which both makes the lexer more compact and independend of text encoding

## Lexing
### Regular Expressions
A regular expression, is a pattern a sequence of symbols in a language of that symbol can take.
We use regular expressions to describe lexical patterns, hence how a `words` looks like in the language. Below you can see the patterns a regular expression can take.

- `Atom: a, where a is any symbol s in the language L`
- `Concatenation: ab, where a and b are either an atom or a subexpression`
- `Or: a|b, where a and b are either an atom or a subexpression`
- `Star: a*, where a is either an atom or a subexpression, which can occur zero or more times.`
- `Plus: a+, where a is either an atom or a subexpression, which can occur one or more times.`
- `maybe: a?, where a is either an atom or a subexpression, which can occur zero or one time.`
- `Interval: [a-b], [c], or [a-bc], where a and b are atoms where a < b and c either are an atom or of the from x-y.`
- `Complement: [^], [^c], [^a-b] or [^a-bc], where a,b and c are as for the interval. This are the set of all bytes subtracted the collection of symbols given by a,b and c.`

- In general `|` has higher precedence than concatenation this means that `a|bc = (a|b)c` and the postfix operators `* + ?` has higher precedence than `|`.
-  The `|` are associative `(a|b)|c = a|(b|c)`.
-  Concatenation are commutative over `|`, hence `c(a|b) = (ca)|(cb)`.

### In code
A regular expression is simple just a string with a regular expression pattern. 
To help format them correctly and make them somewhat more readable there is a bunch helper functions and infix operators.
- `!"a": generate a legal representation of the atom a, where a is any printable character or of the form \xhh where h is a hexadecimal. the prefix operator ! expand this to take any size string s and generate the regular expression of the concatenation of all symbols in s.`
- `a <|> b: Or operator.`
- `a => b: Concat operator.`
- `a .-. b: Interval operator.`
- `a .^. b: Complement operator.`
- `a .@. b: ASCII complement operator.`
- `star a: a*`
- `plus a: a+`
- `mayby a: a?`

#### Code example
```F#
open Regex // load operators and function

let digit = '0' .-. '9' // The interval defining all digits. OBS the a and b can take any possible type.
let sing = maybe (!"+" <|> !"-") // Define the pattern of a possible sign.
let number = sign => digit => maybe (!"e" <|> !"E") => star digit // define signed integers. 

// UTF8 character format
let utf8 =
  // trailing byte
  let trailing = 0b10000000 .-. 0b10111111                                  // here we use bit represantion of a byte
  let one   = 0 .-. 127                                                     // normal ascii representation
  let two   = 0b11000000 .-. 0b11011111 => trailing                         // a chacter of byte size 2
  let three = 0b11100000 .-. 0b11101111 => trailing => trailing             // a chacter of byte size 3
  let four  = 0b11110000 .-. 0b11110111 => trailing => trailing => trailing // a chacter of byte size 4
  
  // Return: The combined possible pattern 
  one <|> two <|> three <|> four
```

In general there is some limitation on what we can define with regular expressions, it is not possible to define size of a token match by the a regular expression, but there is cases as uft8 encoding we can limit the size, but often it is very cumbersome, **F#** can in some way medigate this by smart use `map, filter, fold and scan`, which makes the ability to define the patterns directly in **F#** code so much more convinient. Another reason it is convinient, is that we can define naming for each patterns, hence enhance code reuse, readability and allows for extremely targeted error handling. 

### Creating the lexer
The lexer pattern generator are given as the function `lexer` which takes an array of patterns, token types and possible transformation functions.
Again there is defined infix operators to enhance readability. There are two patterns formed by three infix operators.
- `pattern != f --> token, where pattern is a regular expression, token is some user defined tokentype and f is a function of the form f: string -> 'a.`
- `pattern := token, where pattern and token is the same as above.`

> **OBS!** Given to patterns: `pat1 != f --> tok1` and `pat1 != g --> tok2`, where `f: string -> 'a` and `g: string -> 'b` do not need to hold the constraint `'a = 'b` i.e. the type of any pair of functions given in the pattern array can differ in return type. 

To define tokens the user simply define a descreminated union with no feilds or a enum type.
```F#
type token =
  | Number
  | Char
  | Plus
  | EOF     // end of file mark explicitly needed for the lexer and parser to run without error and used by the parser
```
It will result in build error if the user tries to add fields. 
These are only used by the parser to take type of token, to carry the token data collected we use `!=` and if no data are
needed we ude `:=`, hence we the one of the take a function as argument.
The `token` type above are stored in a underlying type with a data field and the position of the first byte in the token.
To get access to them we use the code below
```F#
open Token
open Position

let Pos token = PosOf token    // PosOf: 'a Token -> Position.
let Val token = ValueOf token  // ValueOf: 'a Token -> 'b, where 'b is the return type of the function given in the pattern. 
let Type token = TypeOf token  // TypeOf: 'a Token -> 'a, return the token instance given above.
```

We handle errors by defining exceptions
```F#
exception NumberOverFlow
exception LexerError
```
Now we can define the pattern table of our lexer, we use the patterns defined above.
```F#
open Lexer

let Numb (i: string) =
    try
       int i
    with
    | _ -> NumberOverFlow

let patterns =
  [|
      number  !=  Numb --> Number   // will through the exception to be catch by the parser
      utf8    !=  id --> Char       // the id function are defined as let id a = a
      !"+"    := Plus               // := do not carry the string with it
      ""      := EOF                // simple add a end of file mark
  |]
  |> lexer
  
let LexString code = 
  try
    LexString pattern code // LexString
  with
  | :? NumberOverFlow -> reraise
  | _ -> LexerError
  
let LexFile path =
  try
    using 
      (new LexBuffer(path: string))       // LexBuffer handle backtracking over buffer ends 
      (fun buf -> LexFile pattern buf)    // LexString
  with
  | :? NumberOverFlow | :? FileNotFoundException -> reraise
  | _ -> LexerError
```

> **OBS!** The statical type system of **F#** is somewhat disabled when generating patterns for the lexer, this is a delibrerate implementation choice.

 
