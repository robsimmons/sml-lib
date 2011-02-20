structure Color : COLOR =
struct

    (* translated from C++ code in Escape. *)
    fun hsvtorgbf (h, s, v) =
        (* Singularity when saturation is 0 *)
        if Real.==(s, 0.0)
        then (v, v, v)
        else
            let
                val hue = h * 6.0
                val fh = real (Real.trunc hue)
                val var_1 = v * (1.0 - s)
                val var_2 = v * (1.0 - s * (hue - fh))
                val var_3 = v * (1.0 - s * (1.0 - (hue - fh)))
            in
                case Real.trunc hue of
                    0 => (v, var_3, var_1)
                  | 1 => (var_2, v, var_1)
                  | 2 => (var_1, v, var_3)
                  | 3 => (var_1, var_2, v)
                  | 4 => (var_3, var_1, v)
                  | _ => (v, var_1, var_2)
            end

    fun wf (w : Word8.word) = real (Word8.toInt w) / 255.0
    fun fw (f : Real.real) = Word8.fromInt (Real.trunc (f * 255.0))
    fun rgbf (r, g, b) = (fw r, fw g, fw b)
    fun hsvtorgb (h, s, v) =
        let val (r, g, b) = hsvtorgbf (wf h, wf s, wf v)
        in rgbf (r, g, b)
        end

    val digits = "0123456789ABCDEF"
    fun hexdig i = implode [CharVector.sub (digits, i div 16),
                            CharVector.sub (digits, i mod 16)]
    fun ws w = hexdig (Word8.toInt w)
    fun tohexstring (r, g, b) = ws r ^ ws g ^ ws b

    (* ASCII trick: (ch | 4400) % 55 *)
    fun hexvalue ch =  
        Word8.fromInt (SysWord.toInt (SysWord.orb(SysWord.fromInt(ord ch), SysWord.fromInt 4400)) mod 55)

    fun ishex c = ((ord c >= ord #"0" andalso
                    ord c <= ord #"9") orelse
                   (ord c >= ord #"a" andalso
                    ord c <= ord #"f") orelse
                   (ord c >= ord #"A" andalso
                    ord c <= ord #"F"))

    fun fromhexstring s =
        if size s <> 6 orelse not (CharVector.all ishex s)
        then NONE
        else SOME (hexvalue (String.sub(s, 0)) * 0w16 +
                   hexvalue (String.sub(s, 1)),
                   hexvalue (String.sub(s, 2)) * 0w16 +
                   hexvalue (String.sub(s, 3)),
                   hexvalue (String.sub(s, 4)) * 0w16 +
                   hexvalue (String.sub(s, 5)))

    fun onefromhexstring s =
        if size s <> 2 orelse not (CharVector.all ishex s)
        then NONE
        else SOME (hexvalue (String.sub(s, 0)) * 0w16 +
                   hexvalue (String.sub(s, 1)))

end