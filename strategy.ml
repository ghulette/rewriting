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

    type 'a t = 'a Term.t t
           
    let path i s t =
      if i < 0 || i >= T.arity t then None else
        s (T.ith i t) >>= fun ti' ->
        Some (T.with_ith i ti' t)

    let constructor g t =
      if T.constructor t = g then Some t else None
             
    let congruence ss t =
      let rec congruence_aux acc = function
        | [] -> Some (List.rev acc)
        | (si,ti)::stis ->
           si ti >>= fun ti' ->
           congruence_aux (ti'::acc) stis
      in
      let ts = T.subterms t in
      if List.(length ts <> length ss) then None else
        congruence_aux [] (List.combine ss ts) >>= fun ts' ->
        Some (T.with_subterms ts' t)

    let all s t =
      let rec all_aux acc = function
        | [] -> Some (List.rev acc)
        | ti::tis ->
           match s ti with
           | Some ti' -> all_aux (ti'::acc) tis
           | None -> None
      in
      let ts = T.subterms t in
      all_aux [] ts >>= fun ts' ->
      Some (T.with_subterms ts' t)
            
    let one s t = None
    let some s t = None
      
  end
    
