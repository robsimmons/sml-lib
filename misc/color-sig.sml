
signature COLOR =
sig
    
    (* Convert from HSV color space to RGB *)
    val hsvtorgbf : (real * real * real) -> (real * real * real)
    val hsvtorgb : (Word8.word * Word8.word * Word8.word) ->
                   (Word8.word * Word8.word * Word8.word)

end
