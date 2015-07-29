type 'a strategy = 'a -> 'a option
type 'a t = 'a strategy
type 'a rule = 'a -> 'a option

let rule r = r

let identity t = Some t

let fail t = None

let test s t =
  match s t with
  | Some _ -> Some t
  | None -> None

let negate s t =
  match s t with
  | Some _ -> None
  | None -> Some t

let sequence s1 s2 t =
  match s1 t with
  | Some t' -> s2 t'
  | None -> None

let left_choice s1 s2 t =
  match s1 t with
  | Some t' -> Some t'
  | None -> s2 t

module Infix =
  struct
    let (>>) = sequence
    let (<+) = left_choice
  end

open Option.Infix

let path n s =
  let rec aux i acc = function
    | [] -> Some (List.rev acc)
    | t::ts when i = 0 ->
       s t >>= fun ti' ->
       aux (i-1) (ti' :: acc) ts
    | t::ts -> aux (i-1) (t :: acc) ts
  in
  aux n []

let congruence ss ts =
  let rec aux acc = function
    | [],[] -> Some (List.rev acc)
    | _,[] -> None
    | [],_ -> None
    | s::ss,t::ts ->
       s t >>= fun t' ->
       aux (t::acc) (ss,ts)
  in
  aux [] (ss,ts)

module LC =
  struct
    let rec all s acc = function
      | [] -> Some (List.rev acc)
      | t::ts ->
         s t >>= fun t' ->
         all s (t'::acc) ts

    let rec none s acc = function
      | [] -> Some (List.rev acc)
      | t::ts ->
         match s t with
         | Some _ -> None
         | None -> none s (t::acc) ts

    let rec star s acc = function
      | [] -> Some (List.rev acc)
      | t::ts ->
         match s t with
         | Some t' -> star s (t'::acc) ts
         | None -> star s (t::acc) ts

    let rec one_and k s acc = function
      | [] -> None
      | t::ts ->
         match s t with
         | Some t' -> k s (t'::acc) ts
         | None -> one_and k s (t::acc) ts
  end

let all s =
  LC.all s []

let one s =
  LC.one_and LC.none s []

let some s =
  LC.one_and LC.star s []
