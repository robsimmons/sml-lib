signature CSV =
sig
    exception CSV of string
    datatype options = TRIM_WHITESPACE | ALLOW_CRLF

    (* Parse a file from disc into rows, using default options ([ALLOW_CRLF]). *)
    val read : string -> string list list

    val readex : options list -> Reader.reader -> string list list

    (* XXX implement as needed *)
    (* val write : string -> string list list -> unit *)

end