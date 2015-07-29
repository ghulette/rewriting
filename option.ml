let bind mx f =
  match mx with
  | Some x -> f x
  | None -> None

module Infix =
  struct
    let (>>=) = bind
  end
