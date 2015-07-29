type t =
  | True
  | False
  | Atom of char
  | Not of t
  | And of t * t
  | Or of t * t
  | Impl of t * t
  | Equiv of t * t

module Token =
  struct
    type t = Kwd of string | Sym of string

    let is_whitespace = function
      | ' ' | '\t' | '\n' -> true
      | _ -> false

    let is_alpha = function
      | 'a'..'z' -> true
      | _ -> false

    let is_sym = function
      | '\\' | '/' | '=' | '<' | '>' | '~' -> true
      | _ -> false

  end
