module T = Term.Basic
module ST = Strategy.Traversal (T)
open ST
open Strategy
open Strategy.Infix

let try_ s = s <+ fail
let rec repeat s = try_ (s >> repeat s)
let rec topdown s = s >> all (topdown s)
let rec bottomup s = all (bottomup s) >> s
let rec downup s = s >> all (downup s) >> s

let rec oncetd s = s <+ one (oncetd s)
let rec oncebu s = one (oncebu s) <+ s
