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

module Make (T : Term) =
  struct
    let path i s = T.on_children (Strategy.path i s)
    let congruence ss = T.on_children (Strategy.congruence ss)
    let all s = T.on_children (Strategy.all s)
    let one s = T.on_children (Strategy.one s)
    let some s = T.on_children (Strategy.some s)
  end

open Option.Infix

let on1 s f a =
  Strategy.apply s [a] >>= function
  | [a'] -> Some (f a')
  | _ -> None

let on2 s f a b =
  Strategy.apply s [a;b] >>= function
  | [a';b'] -> Some (f a' b')
  | _ -> None

let on3 s f a b c =
  Strategy.apply s [a;b;c] >>= function
  | [a';b';c'] -> Some (f a' b' c')
  | _ -> None

let on4 s f a b c d =
  Strategy.apply s [a;b;c;d] >>= function
  | [a';b';c';d'] -> Some (f a' b' c' d')
  | _ -> None

let on5 s f a b c d e =
  Strategy.apply s [a;b;c;d;e] >>= function
  | [a';b';c';d';e'] -> Some (f a' b' c' d' e')
  | _ -> None
