type 'a strategy
type 'a t = 'a strategy
type 'a rule = 'a -> 'a option

val rule : 'a rule -> 'a t
val id : 'a t
val fail : 'a t
val test : 'a t -> 'a t
val negate : 'a t -> 'a t
val seq : 'a t -> 'a t -> 'a t
val left_choice : 'a t -> 'a t -> 'a t

module Traversal (Term : Term.S) :
sig
  val constructor : 'a -> 'a Term.t strategy
  val path : int -> 'a Term.t strategy -> 'a Term.t strategy
  val congruence : 'a Term.t strategy list -> 'a Term.t strategy
  val all : 'a Term.t strategy -> 'a Term.t strategy
  val one : 'a Term.t strategy -> 'a Term.t strategy
  val some : 'a Term.t strategy -> 'a Term.t strategy
end

module Infix :
sig
  val (>>) : 'a t -> 'a t -> 'a t
  val (<+) : 'a t -> 'a t -> 'a t
end
