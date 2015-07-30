type 'a strategy
type 'a t = 'a strategy
type 'a rule = 'a -> 'a option

val apply : 'a t -> 'a -> 'a option

val rule : 'a rule -> 'a t
val identity : 'a t
val fail : 'a t
val test : 'a t -> 'a t
val negate : 'a t -> 'a t
val sequence : 'a t -> 'a t -> 'a t
val left_choice : 'a t -> 'a t -> 'a t

val path : int -> 'a t -> 'a list t
val congruence : 'a t list -> 'a list t
val all : 'a t -> 'a list t
val one : 'a t -> 'a list t
val some : 'a t -> 'a list t

module Infix :
sig
  val (>>) : 'a t -> 'a t -> 'a t
  val (<+) : 'a t -> 'a t -> 'a t
end
