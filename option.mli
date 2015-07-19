val bind : 'a option -> ('a -> 'b option) -> 'b option

module Infix :
sig
  val ( >>= ) : 'a option -> ('a -> 'b option) -> 'b option
end
