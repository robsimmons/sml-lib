(* Implements equivalence classes. *)
signature UNIONFIND =
sig

    (* Equivalence relation between objects of type 'a *)
    type 'a arena

    type 'a class

    (* Order is arbitrary. Equality respects the unions 
       that have been made, of course. *)
    val compare : 'a class * 'a class -> order
    val eq : 'a class * 'a class -> bool

    (* Create a new empty arena. *)
    val new : unit -> 'a arena

    (* Get the arena that this equivalence class is from *)
    val arena : 'a class -> 'a arena

    (* Add as its own singleton class *)
    val add : 'a arena -> 'a class

    (* Must be from the same arena *)
    val union : 'a class * 'a class -> unit

end