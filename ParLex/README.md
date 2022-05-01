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
### In code
A regular expression is simple just a string with a regular expression pattern 
