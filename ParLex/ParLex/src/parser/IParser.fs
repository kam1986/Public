module IParser

// type constraints should help the runtime/compiler to transform indirect calls to direct once
// and inline functions where needed
type IParser<'token, 'ret, 'parser when 'parser: struct and 'parser :> IParser<'token, 'ret, 'parser> > =
 
    abstract member Run : 'token seq -> 'ret

