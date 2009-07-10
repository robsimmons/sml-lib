structure Waveform :> WAVEFORM =
struct

    (* This algorithm literally IS Bresenham's line drawing one. The idea
       is to draw a line starting at 0,0 to (+infinity,+infinity) with
       a slope m, but instead of drawing pixels at all y values, we
       "draw" at y mod 2, giving us an alternating square waveform.

       We want to calculate the slope m from the sample rate s and
       frequency f, which are given. 
       
       f = (s * m) / 2, so
       m = (2 * f) / s.

       To use just integers we can represent m as a ratio y/x (which
       is the input in the classic presentation of Bresenham's
       algorithm anyway):

       y / x = (2 * f) / s

       and then just set x = s and y = 2 * f.
    *)
    structure FastSquare =
    struct

        type square = { error : int, y : int, deltax : int, deltay : int, volume : int }

        fun square { samplerate, freq, volume } =
            let
                (* draw a line from 0,0 to x,y *)
                val x = samplerate
                val y = 2 * freq

                val deltax = x
                val deltay = y
                
                val error = deltax div 2
            in
                { error = error, y = y, deltax = deltax, deltay = deltay, volume = volume }
            end
            
        fun next { error, y, deltax, deltay, volume } =
            let
                val sample = if y mod 2 <> 0 then volume else 0
                val error = error - deltay
                val (error, y) = if error < 0
                                 then (error + deltax, y + 1)
                                 else (error, y)
            in
                ({ error = error, y = y, deltax = deltax, deltay = deltay, volume = volume }, sample)
            end

    end

end