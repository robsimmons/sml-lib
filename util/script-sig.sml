(* Collection of recipes for scripting. These are less general than
   the utilities in Util, StringUtil, and ListUtil but encapsulate
   idioms that are used frequently in small scripting tasks.

   (Requires Util, StringUtil, ListUtil. XXX make CM.)
*)
signature SCRIPT =
sig
    (* Read all the lines from a named file into a list. 
       Works with any standard line ending. Skips blank lines.
       Strips whitespace from fore and aft of each line. *)
    val linesfromfile : string -> string list

end
