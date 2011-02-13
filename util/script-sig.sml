(* Collection of recipes for scripting. These are less general than
   the utilities in Util, StringUtil, and ListUtil but encapsulate
   idioms that are used frequently in small scripting tasks.

   (Requires Util, StringUtil, ListUtil, Params, njlib, etc. XXX make CM.)
*)
signature SCRIPT =
sig
    exception Script of string

    (* Read all the lines from a named file into a list. 
       Works with any standard line ending. Skips blank lines.
       Strips whitespace from fore and aft of each line. 

       In line-by-line applications where the file may be very
       large, consider SimpleStream. *)
    val linesfromfile : string -> string list

    (* Read an association list from a file. Blank lines
       are ignored. The file format is
       <optional whitespace>key-without-whitespace<optional whitespace>value<optional whitespace>\n
       A line with only token on it is treated as a key with an empty string value.
       If a key is duplicated, raises Script. *)
    val alistfromfile : string -> { alist : (string * string) list, 
                                    lookup : string -> string option }

    (* Creates a word list from a file, assuming that each
       line contains a word and ignoring whitespace. Staged
       so that repeated calls to the returned function are
       efficient. Words are normalized to lowercase. *)
    val wordlist : string -> string -> bool

end
