
(* unique stamps. This implementation suffers from
   the possibility of overflow, so don't create
   more than 2.1 billion stamps. *)

signature STAMP =
sig
    type stamp

    val eq : stamp * stamp -> bool
    val is : stamp -> stamp -> bool
    val compare : stamp * stamp -> order

    val tostring : stamp -> string

    val new : unit -> stamp
end