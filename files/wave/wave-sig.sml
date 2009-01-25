(* Read and write uncompressed wave files from memory, and only with
   the minimal set of chunks (format and data). *)
signature WAVE =
sig

    exception Wave of string

    (* Any positive number of channels is allowed.
       Many programs interpret them as follows:

       Monophonic: [Center]
       Stereo: [Left, Right]
       3ch: [Left, Right, Center]
       quad: [Front Left, Front Right, Rear Left, Rear Right]
       4ch: [Left, Center, Right, Surround]
       6ch: [Left Center, Left, Center, Right Center, Right, Surround]

       All channels must have the same number of samples!
       *)
    type 'a channels = 'a Vector.vector
    
    datatype frames =
        (* eight-bit is unsigned samples *)
        Bit8 of Word8.word Vector.vector channels 
        (* but anything else is signed.
           Int16 structure is not standard, so use
           system int. *)
      | Bit16 of int Vector.vector channels
      | Bit32 of Int32.int Vector.vector channels

    (* Everything else is computed from input *)
    type wave = 
        { frames : frames,
          samplespersec : Word32.word }
          
    val tobytes : wave -> Word8.word Vector.vector
    val tofile : wave -> string -> unit

    val read : Reader.reader -> wave

end