module Json


type Value = 
    | Array  of JSon seq
    | String of string
    | Number of float
    | Bool   of bool
    
and JSon =
    Element of {|  name: string; value: Value option |} seq
    

type Token =
    | STRING
    | WHITESPACE
    | COMMA
    | COLON
    | NUMBER
    | BOOL
    | LPAR
    | RPAR
    | LSQRT
    | RSQRT
    | NULL


type Type =
    | P_json
    | P_array
    | P_value
    | P_string
    | P_number
