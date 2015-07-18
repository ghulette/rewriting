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

module type Term =
  sig
    type 'a t

    val make : 'a -> 'a t list -> 'a t
    val constructor : 'a t -> 'a
    val children : 'a t -> 'a t list
  end

module Traversal (T : Term) =
  struct
    let path i s t =
      let ts = Array.of_list (T.children t) in
      match s ts.(i) with
      | Some ti' ->
         ts.(i) <- ti';
         let t' = T.make (T.constructor t) (Array.to_list ts) in
         Some t'
      | None -> None
      | exception (Invalid_argument _) -> None
  end

    
