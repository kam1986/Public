module Productions


open Token
open TypeAcrobatics

type Symbol<'T,'N>
type Production<'t, 'T,'N> 
type Productions<'t, 'T,'N> 

val (!) : 'T -> Symbol<'T,'N>
val (~%): 'N -> Symbol<'T,'N>