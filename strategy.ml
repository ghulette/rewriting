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

(* val bind : 'a option -> ('a -> 'b option) -> 'b option *)
let bind mx f = 
  match mx with
  | Some x -> f x
  | None -> None

let (>>=) = bind


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

    let path i s t =
      match s (T.ith i t) with
      | Some ti' -> Some (T.with_ith i ti' t)
      | None -> None
      | exception _ -> None

    let congruence f ss t =
      let rec congruence_aux acc = function
        | [] -> Some (List.rev acc)
        | (s,t)::sts -> s t >>= fun t' -> congruence_aux (t'::acc) sts
      in                       
      if T.constructor t <> f then None else
        let ts = T.subterms t in
        match congruence_aux [] (List.combine ss ts) with
        | None -> None
        | Some ts' -> Some (T.with_subterms ts' t)
        | exception _ -> None
  end
    
