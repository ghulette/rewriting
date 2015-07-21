module type S =
  sig
    type 'a t
    val constructor : 'a t -> 'a
    val subterms : 'a t -> 'a t list
    val with_subterms : 'a t list -> 'a t -> 'a t
    val arity : 'a t -> int
    val ith : int -> 'a t -> 'a t
    val with_ith : int -> 'a t -> 'a t -> 'a t
  end

module Basic : S
