
structure Test =
struct

    val sq = Waveform.FastSquare.square { samplerate = 44100,
                                          freq = 440,
                                          volume = 16384 }
       
    val a = Array.array (2 * 44100, 0)
    fun write sq n =
        if n >= Array.length a
        then Array.vector a
        else 
            let val (sq, s) = Waveform.FastSquare.next sq
            in
                Array.update (a, n, s - 8192);
                write sq (n + 1)
            end

    val v = write sq 0
    val wave = { frames = Wave.Bit16 (Vector.fromList [v]),
                 samplespersec = 0w44100 }
    val () = Wave.tofile wave "test.wav"

end