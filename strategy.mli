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
    type t
    type constructor
    val constructor : t -> constructor
    val subterms : t -> t list
    val with_subterms : t list -> t -> t
    val arity : t -> int
    val ith : int -> t -> t
    val with_ith : int -> t -> t -> t
    val eq : t -> t -> bool
  end
    
module Traversal (Term : Term) :
sig
  val path : int -> Term.t t -> Term.t t
  val congruence : Term.constructor -> Term.t t list -> Term.t t
end
