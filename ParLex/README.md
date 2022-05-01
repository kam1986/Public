# Documentation
This is the documentation of the Library part of the Lexer and Parser genereator.

> OBS! This library has no dependencies other than .net Core 6.0.

## Lexing
### Regular Expressions
A regular expression, is a pattern a sequence of symbols in a language of that symbol can take i.e. lexical analysis.
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
-  The `|` is associative `(a|b)|c = a|(b|c)`.
-  Concatenation is commutative over `|`, hence `c(a|b) = (ca)|(cb)`.

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
let hexdigit = 'a' .-. 'f' <|> 'A' .-. 'F' <|> digit

let sing = maybe (!"+" <|> !"-") // Define the pattern of a possible sign.
let number = 
  // define signed integers.
  (sign => digit => maybe (!"e" <|> !"E") => star digit) <|> (!"0x" => plus hexdigit) 

// UTF8 character format
let utf8 =
  // trailing byte
  let trailing = 0b10000000 .-. 0b10111111                                  // here we use bit represantion of a byte
  let one   = 0 .-. 127                                                     // normal ascii representation
  let two   = 0b11000000 .-. 0b11011111 => trailing                         // a character of byte size 2
  let three = 0b11100000 .-. 0b11101111 => trailing => trailing             // a character of byte size 3
  let four  = 0b11110000 .-. 0b11110111 => trailing => trailing => trailing // a character of byte size 4
  
  // Return: The combined possible pattern 
  one <|> two <|> three <|> four
```

In general there is some limitation on what we can define with regular expressions, it is most many cases not possible to define size of a token match by the a regular expression, but there is cases as with the uft8 encoding we can limit the size, but often it is very cumbersome, **F#** can in some way medigate this by smart use of `map, filter, fold and scan`, which makes the ability to define the patterns directly in **F#** code so much more convinient. Another reason it is convinient, is that we can define naming for each patterns, hence enhance code reuse, readability and allows for extremely targeted error handling. 

### Creating the lexer
The lexer pattern generator are given as the function `lexer` which takes an array of patterns, token types and possible transformation functions.
Again there is defined infix operators to enhance readability. There are two patterns formed by three infix operators.
- `pattern != f --> token, where pattern is a regular expression, token is some user defined tokentype and f is a function of the form f: string -> 'a.`
- `pattern := token, where pattern and token is the same as above.`

> **OBS!** Given to patterns: `pat1 != f --> tok1` and `pat1 != g --> tok2`, where `f: string -> 'a` and `g: string -> 'b` the constraint `'a = 'b` do not need to hold i.e. the type of any pair of functions given in the pattern array can differ in return type. 

To define tokens the user simply define a descreminated union with no feilds or a enum type.
```F#
type token =
  | Number
  | Char
  | Plus
  | EOF     // end of file mark explicitly needed for the lexer and parser to run without error and used by the parser
```
It will result in build error if the user tries to add fields. 
These are only used by the parser to ask about the type of a token. To carry the token data collected use `pat != f --> token` where f define what data
to carry. If no data are needed we `pat := token`. The `token` type above are stored in a underlying type with a data field and the position of the first byte in the token. To get access to them we use the code below
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
      utf8    !=  id --> Char       // the id function are defined as let id a = a
      number  !=  Numb --> Number   // will through the exception to be catch by the parser
      !"+"    := Plus               // := do not carry the string with it
      ""      := EOF                // simple add a end of file mark
  |]
  |> lexer
  
let LexString code = 
  try
    LexString pattern code // LexString are a function declared in Lexer
  with
  | :? NumberOverFlow -> reraise
  | _ -> LexerError
  
let LexFile path =
  try
    using 
      (new LexBuffer(path: string))       // LexBuffer handle backtracking over buffer line
      (fun buf -> LexFile pattern buf)    // LexFile are a function declared in Lexer
  with
  | :? NumberOverFlow | :? FileNotFoundException -> reraise
  | _ -> LexerError
```
To handle conflicting patterns the rule that the first pattern in the array of two conflifting patterns are choosen.
> **OBS!** The statical type system of **F#** is somewhat disabled when generating patterns for the lexer, this is a delibrerate implementation choice.

The underlying lexer algorithm runs over a DFA graph made by the function `lexer`. This makes the initialisation time cost larger than one using regex as underlying representation, but if the lexer needs to run for longer time it is regained by the much faster search time which is linear by the length of the input, where as an algorithm searching over a regular expression takes the number of cases in the regular times the length of the input string. Cases where this is usefull, is when running a decoding algorithm from a socket in a server. This could be a userdefined `JSon` parser, or a wrapper over `SQL` queueies in a user application. 
The DFA graph for the lexer are fix sized at `256*256` bytes big jumptable, a bitmask of size `4*64` bytes. This makes the whole table be located in cache when in use, which makes the algorithm as fast as possible. We could have choosen to make it dynamically sized which makes the table for the lexer of regular expression tokenizer for this lib be the size `8 * 256` byte and `1` byte for bit mask and we might decide to change it in later version.


## Parsing
The Parser Library of ParLex are for now only for SLR(0) parsing, but en near future it will include LR(1) and indentation sensitive possibilities, which for now are not stable but on the way.

Parsing is the way to validate syntax of a language. By a language we mean any format having symbols and formal rules on who the symbols relate to each other. To define syntactical structure we use context free grammar.

### Context Free Grammar
Context free grammar are a language defining structure just like regular expressions, but where regular expression are good to define patterns of lexical tokens, context free grammar are good to define rules of syntactival structure.
The rules of context free grammar are build op over productions and terminals
-  `A -> `
-  `A -> bA`
-  `A -> Ba`
-  `B -> a`
Here `A` and `B` are productions and `a` and `b` are terminals. The uppermost production are assumed to be the initial one. The first rule stats that the production `A` are allowed to return nothing. The second rule states that are recursive over the terminal `b`, the third rule states the production `A` has a transition to production `B` and but after `B` end it should af a postfixing `a`. the last rule just state that the production `B` terminates with terminal `a`. `aa`, `baa`, `bbaa` and `bbbbbbaa` are all legal patterns but `bab` and `bba` are not. 

### In Code
As for the Lexer we have several operators to help make the code readable and consice.
A production and its' rules are given in the form
```F#
open Productions
open Parser

type Prod =
  | Start
  | A
  | B
  
let syntax =
  Productions [
      Start => [
          [%A; !EOF]
          >> fun args -> ValueOf args.[0]
      ]
      
      A => [
          []
          >> fun _ -> ret1

          [!b; %A]
          >> fun args -> ret2

          [%prod2; !a]
          >> fun args -> ret3
      ]

      B => [
          [!a]
          >> fun arg -> ret4
      ]
  ]
  |> SLR
  
let Parse tokens =
  try
    Run syntax tokens
  with
  | _ -> //error handling 
```
Here the starting point are production `Start` which only use is to check that we parse the whole sequence, it has the three cases as above, the empty case, the recursive one and the one that transition to production `B`. the terminals `a` and `b`, are meant to be tokens passed by the lexer. As for the Lexer we can insert `try .. with .. -> ..` to catch exceptions, by defining exception carefully an implementation can make fine graint error handling since the programmer can insert code knowningly about patterns the engine cannot. Instead of testing all values of `args` we can handle specific exception as information on where it occur, we can even make an exception `Handled` with trailing data to propagate error information upward the parse tree. Both tokens and productions carry positional information that can be accessed with `PosOf args.[i]` for the position of the ith argument of the pattern. Carefully implementation can make dynamic operator predence possible with only constant speed penalty.

