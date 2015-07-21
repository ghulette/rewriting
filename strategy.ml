type 'a strategy = 'a -> 'a option
type 'a t = 'a strategy
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

module Traversal (T : Term.S) =
  struct

    let constructor g t =
      if T.constructor t = g then Some t else None

    let path i s t =
      let open Option.Infix in
      if i < 0 || i >= T.arity t then None else
        s (T.ith i t) >>= fun ti' ->
        Some (T.with_ith i ti' t)

    let congruence ss t =
      let open Option.Infix in
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
      let open Option.Infix in
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
            
    let one s t =
      let open Option.Infix in
      let rec one_aux_fnd acc = function
        | [] -> Some (List.rev acc)
        | ti::tis ->
           match s ti with
           | Some _ -> None
           | None -> one_aux_fnd (ti::acc) tis
      in
      let rec one_aux acc = function
        | [] -> None
        | ti::tis ->
           match s ti with
           | Some ti' -> one_aux_fnd (ti'::acc) tis
           | None -> one_aux (ti::acc) tis
      in
      let ts = T.subterms t in
      one_aux [] ts >>= fun ts' ->
      Some (T.with_subterms ts' t)

    let some s t =
      let open Option.Infix in
      let rec some_aux_fnd acc = function
        | [] -> Some (List.rev acc)
        | ti::tis ->
           match s ti with
           | Some ti' -> some_aux_fnd (ti'::acc) tis
           | None -> some_aux_fnd (ti::acc) tis
      in
      let rec some_aux acc = function
        | [] -> None
        | ti::tis ->
           match s ti with
           | Some ti' -> some_aux_fnd (ti'::acc) tis
           | None -> some_aux (ti::acc) tis
      in
      let ts = T.subterms t in
      some_aux [] ts >>= fun ts' ->
      Some (T.with_subterms ts' t)
      
  end

module Infix =
  struct
    let (>>) = seq
    let (<+) = left_choice
  end
