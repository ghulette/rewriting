module type Term =
  sig
    type 'a t
    val on_children : 'a list Strategy.t -> 'a t Strategy.t
  end

module type S =
  sig
    type 'a term
    val path : int -> 'a Strategy.t -> 'a term Strategy.t
    val congruence : 'a Strategy.t list -> 'a term Strategy.t
    val all : 'a Strategy.t -> 'a term Strategy.t
    val one : 'a Strategy.t -> 'a term Strategy.t
    val some : 'a Strategy.t -> 'a term Strategy.t
  end

module Make (T : Term) : (S with type 'a term := 'a T.t)
