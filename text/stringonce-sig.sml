
(* Allows for uniqueified variable names (ie x_1 and x_2 from 'x'),
   but can return the original variable name 'x' in the case that only
   one variable called 'x' is ever introduced. *)
signature STRINGONCE =
sig

    type stringarena

    (* create a new arena *)
    val arena : unit -> stringarena

    (* string to use as a separator before versions (default is "") *)
    val arenaex : string -> stringarena

    (* clear an arena. query functions hanging around
       are now invalid. *)
    val clear : stringarena -> unit

    (* symbol a s

       insert a symbol into the arena. returns a unique
       integer id for this symbol, and a function to convert
       the symbol into a unique string. Tthis function's behavior
       will change if another symbol with the same string is
       inserted into the table! But once the function is called,
       that name is "locked in," and future calls will always
       return the same thing.

       *)
    val symbol : stringarena -> string -> int * (unit -> string)

end