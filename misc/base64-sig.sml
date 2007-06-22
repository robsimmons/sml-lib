
(* encode binary strings into base64,
   decode base64 strings into binary ones. *)

signature BASE64 =
sig

    val decode: string -> string option
    val encode: string -> string

end
