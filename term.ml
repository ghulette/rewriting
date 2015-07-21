module type S =
  sig
    type 'a t
    val constructor : 'a t -> 'a
    val subterms : 'a t -> 'a t list
    val with_subterms : 'a t list -> 'a t -> 'a t
    val arity : 'a t -> int
    val ith : int -> 'a t -> 'a t
    val with_ith : int -> 'a t -> 'a t -> 'a t
  end

module Basic : S =
  struct
    type 'a t = { cns : 'a; subts : 'a t list }

    let constructor t =
      t.cns

    let subterms t =
      t.subts

    let with_subterms subts t =
      { t with subts }
                                  
    let arity t =
      List.length t.subts

    let ith i t =
      List.nth t.subts i

    let with_ith i ti' t =
      let subts = List.mapi (fun j ti -> if i = j then ti' else ti) t.subts in
      {t with subts}
  end
