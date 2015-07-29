type t =
  | True
  | False
  | Atom of char
  | Not of t
  | And of t * t
  | Or of t * t
  | Impl of t * t
  | Equiv of t * t

let rec to_string = function
  | True -> "true"
  | False -> "false"
  | Atom a -> Char.escaped a
  | Not p -> Format.sprintf {|~(%s)|} (to_string p)
  | And (p,q) -> Format.sprintf {|(%s /\ %s)|} (to_string p) (to_string q)
  | Or (p,q) -> Format.sprintf {|(%s \/ %s)|} (to_string p) (to_string q)
  | Impl (p,q) -> Format.sprintf {|(%s ==> %s)|} (to_string p) (to_string q)
  | Equiv (p,q) -> Format.sprintf {|(%s <=> %s)|} (to_string p) (to_string q)
