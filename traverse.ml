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

module Make (T : Term) =
  struct
    let path i s = T.on_children (Strategy.path i s)
    let congruence ss = T.on_children (Strategy.congruence ss)
    let all s = T.on_children (Strategy.all s)
    let one s = T.on_children (Strategy.one s)
    let some s = T.on_children (Strategy.some s)
  end
