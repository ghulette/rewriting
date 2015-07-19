type 'a t = 'a -> 'a option
type 'a rule = 'a -> 'a option

let rule r = r
                     
let id t = Some t

let fail t = None
                            
let test s t =
  match s t with
  | Some _ -> Some t
  | None -> None

let negate s t =
  match s t with
  | Some _ -> None
  | None -> Some t

let seq s1 s2 t =
  match s1 t with
  | Some t' -> s2 t'
  | None -> None

let left_choice s1 s2 t =
  match s1 t with
  | Some t' -> Some t'
  | None -> s2 t

module type Eq = sig
    type t
    val eq : t -> t -> bool
  end
               
module type Term = sig
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

module Traversal (T : Term) =
  struct

    open Option.Infix
    
    let path i s t =
      if i < 0 || i >= T.arity t then None else
        s (T.ith i t) >>= fun ti' ->
        Some (T.with_ith i ti' t)

    let congruence f ss t =
      let rec congruence_aux acc = function
        | [] -> Some (List.rev acc)
        | (s,t)::sts ->
           s t >>= fun t' ->
           congruence_aux (t'::acc) sts
      in
      let ts = T.subterms t in
      if T.constructor t <> f || List.(length ts <> length ss) then None else
        congruence_aux [] (List.combine ss ts) >>= fun ts' ->
        Some (T.with_subterms ts' t)
  end
    
