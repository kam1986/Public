module Typer


#nowarn "62"

open Ast

type Ty = (unit, unit) operator

type 'navn Type =
    | App of TyKon * 'navn Type liste
    | Var of 'navn
    | Ingen


and TyKon = 
    | Type of Ty
    | Sandhed
    | Array
    | Tom
    | Pil


