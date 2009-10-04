signature WAVEFORM =
sig

    type wave = unit -> int
    (* Give the alpha value (0..1) which determines the 
       cutoff frequency (lower alpha means lower cutoff). 
       TODO: There should be some formula for computing
       alpha from a sample rate and desired cutoff.
       *)
    val lowpass : real -> wave -> wave

    (* Fast, good quality square wave.
       
       This uses an integer-only algorithm for efficiency,
       but attempts to avoid alasing by
         - Accumulating error like in Bresnham's line drawing algorithm
         - Faking anti-aliasing on edge samples, based on that error.

       *)
    structure FastSquare :
    sig
        type square
        val square : { 
                       (* both are measured in hertz, but only
                          the ratio matters. So, if you want a
                          non-integer (real) frequency, you can
                          multiply both your sample rate and
                          frequency by any fixed amount within
                          the numeric limits. *)
                       samplerate : int, 
                       freq : int,
                       (* returns samples in [0, volume] *)
                       volume : int } -> square

        (* Gets the next sample and state *)
        val next : square -> square * int
        val wave : square -> wave
    end


end
