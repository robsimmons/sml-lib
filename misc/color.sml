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
                val fh = Real.trunc hue
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

    fun wf (w : Word.word) = real (Word8.toInt w) / 255.0
    fun fw (f : Real.real) = Word8.fromInt (f * 255.0)
    fun hsvtorgb (h, s, v) =
        let val (r, g, b) = hsvtorgbf (wf h, wf s, wf v)
        in (fw r, fw g, fw b)
        end

end