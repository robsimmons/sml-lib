
structure Test =
struct

    val sq = Waveform.FastSquare.wave
              (Waveform.FastSquare.square { samplerate = 44100,
                                            freq = 440,
                                            volume = 16384 })

    val lowsq = Waveform.lowpass 0.25 sq
    val lowsq = Waveform.lowpass 0.25 lowsq

    val a = Array.array (2 * 44100, 0)
    fun write w n =
        if n >= Array.length a
        then Array.vector a
        else 
            let val s = w ()
            in
                Array.update (a, n, s - 8192);
                write w (n + 1)
            end

    val v = write lowsq 0
    val wave = { frames = Wave.Bit16 (Vector.fromList [v]),
                 samplespersec = 0w44100 }
    val () = Wave.tofile wave "test.wav"

end