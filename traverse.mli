module type Term =
  sig
    type 'a t
    val on_children : 'a t list Strategy.t -> 'a t Strategy.t
  end

module type S =
  sig
    type 'a term
    val path : int -> 'a term Strategy.t -> 'a term Strategy.t
    val congruence : 'a term Strategy.t list -> 'a term Strategy.t
    val all : 'a term Strategy.t -> 'a term Strategy.t
    val one : 'a term Strategy.t -> 'a term Strategy.t
    val some : 'a term Strategy.t -> 'a term Strategy.t
  end

module Make (Term : Term) : (S with type 'a term := 'a Term.t)

val on1 : 'a list Strategy.t ->
          ('a -> 'a) ->
          'a -> 'a option
val on2 : 'a list Strategy.t ->
          ('a -> 'a -> 'a) ->
          'a -> 'a -> 'a option
val on3 : 'a list Strategy.t ->
          ('a -> 'a -> 'a -> 'a) ->
          'a -> 'a -> 'a -> 'a option
val on4 : 'a list Strategy.t ->
          ('a -> 'a -> 'a -> 'a -> 'a) ->
          'a -> 'a -> 'a -> 'a -> 'a option
val on5 : 'a list Strategy.t ->
          ('a -> 'a -> 'a -> 'a -> 'a -> 'a) ->
          'a -> 'a -> 'a -> 'a -> 'a -> 'a option
