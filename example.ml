type 'a formula =
  | True
  | False
  | Atom of 'a
  | Not of 'a formula
  | And of 'a formula * 'a formula
  | Or of 'a formula * 'a formula
  | Impl of 'a formula * 'a formula
  | Equiv of 'a formula * 'a formula

let mk_not p = Not p
let mk_and p q = And (p,q)
let mk_or p q = Or (p,q)
let mk_impl p q = Impl (p,q)
let mk_equiv p q = Equiv (p,q)

let rec to_string = function
  | True -> "true"
  | False -> "false"
  | Atom a -> Char.escaped a
  | Not p -> Format.sprintf {|~(%s)|} (to_string p)
  | And (p,q) -> Format.sprintf {|(%s /\ %s)|} (to_string p) (to_string q)
  | Or (p,q) -> Format.sprintf {|(%s \/ %s)|} (to_string p) (to_string q)
  | Impl (p,q) -> Format.sprintf {|(%s ==> %s)|} (to_string p) (to_string q)
  | Equiv (p,q) -> Format.sprintf {|(%s <=> %s)|} (to_string p) (to_string q)

module Term =
  struct
    type 'a t = 'a formula

    let on_children s =
      let inner = function
        | Not p -> Traverse.on1 s mk_not p
        | And (p,q) -> Traverse.on2 s mk_and p q
        | Or (p,q) -> Traverse.on2 s mk_or p q
        | Impl (p,q) -> Traverse.on2 s mk_impl p q
        | Equiv (p,q) -> Traverse.on2 s mk_equiv p q
        | fm -> Some fm
      in
      Strategy.rule inner
  end

module T = Traverse.Make (Term)
