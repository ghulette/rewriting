type 'a t
type 'a rule = 'a -> 'a option

val rule : 'a rule -> 'a t
val id : 'a t
val fail : 'a t
val test : 'a t -> 'a t
val negate : 'a t -> 'a t
val seq : 'a t -> 'a t -> 'a t
val left_choice : 'a t -> 'a t -> 'a t

module type Term =
  sig
    type 'a t
    val constructor : 'a t -> 'a
    val subterms : 'a t -> 'a t list
    val with_subterms : 'a t list -> 'a t -> 'a t
    val arity : 'a t -> int
    val ith : int -> 'a t -> 'a t
    val with_ith : int -> 'a t -> 'a t -> 'a t
  end
    
module Traversal (Term : Term) :
sig
  val path : int -> 'a Term.t t -> 'a Term.t t
  val constructor : 'a -> 'a Term.t t
  val congruence : 'a Term.t t list -> 'a t
end
