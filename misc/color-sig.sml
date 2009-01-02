
signature COLOR =
sig
    
    (* Convert from HSV color space to RGB *)
    val hsvtorgbf : (real * real * real) -> (real * real * real)
    val hsvtorgb : (Word8.word * Word8.word * Word8.word) ->
                   (Word8.word * Word8.word * Word8.word)

    (* To HTML RRGGBB as a 6-character hexadecimal string, and back. *)
    val tohexstring : Word8.word * Word8.word * Word8.word -> string
    val fromhexstring : string -> (Word8.word * Word8.word * Word8.word) option
    (* eg "fE" to 0w254 *)
    val onefromhexstring : string -> Word8.word option

end
