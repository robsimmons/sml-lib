
structure Wave :> WAVE =
struct

    exception Wave of string

    type w8 = Word8.word

    type 'a channels = 'a Vector.vector
    
    datatype frames =
        (* eight-bit is unsigned samples *)
        Bit8 of Word8.word Vector.vector channels 
        (* but anything else is signed *)
      | Bit16 of Int16.int Vector.vector channels
      | Bit32 of Int32.int Vector.vector channels

    type wave = 
        { frames : frames,
          samplespersec : Word32.word }

    fun info { frames, samplespersec } =
        let
            val (nchannels, nframes, bits) = 
                case frames of
                    Bit8  v => (Vector.length v, Vector.length (Vector.sub(v, 0)), 8)
                  | Bit16 v => (Vector.length v, Vector.length (Vector.sub(v, 0)), 16)
                  | Bit32 v => (Vector.length v, Vector.length (Vector.sub(v, 0)), 32)

            val databytes = (bits div 8) * nchannels * nframes
            val blockalign = nchannels * (bits div 8)
        in
            { databytes = databytes,
              formatsize = 16,
              (* Not sure why these are present, since they could be calculuated
                 by the program that reads it... *)
              blockalign = blockalign,
              bytespersec = Word32.toInt samplespersec * blockalign,
              nchannels = nchannels,
              nframes = nframes,
              bits = bits }
        end handle Subscript => raise Wave "must have a least 1 channel"

    (* For pre-allocation of output array *)
    fun bytesneeded (w : wave) =
        let
            val { databytes, formatsize, ... } = info w
        in
            (* RIFF + size + WAVE *)
            4 + 4 + 4 +
            (* actual bytes plus riff header *)
            (formatsize + 8) +
            (* actual bytes plus riff header *)
            (databytes + 8)
        end

    fun for lo hi f =
        if lo > hi then ()
        else (ignore (f lo); for (lo + 1) hi f)

    fun toany (wb : w8 -> unit) (w : wave) =
        let
            fun wid s =
                case explode s of
                    a :: b :: c :: d :: nil =>
                        let in
                            wb (Word8.fromInt (ord a));
                            wb (Word8.fromInt (ord b));
                            wb (Word8.fromInt (ord c));
                            wb (Word8.fromInt (ord d))
                        end
                  | _ => raise Wave "bad id??"
                        
            fun w16 (w : Int16.int) =
                (* LSB order *)
                let in
                    wb (Word8.fromInt (Int16.toInt (Int16.mod(w, 256))));
                    wb (Word8.fromInt (Int16.toInt (Int16.div(w, 256))))
                end

            fun w32 (w : Int32.int) =
                let in
                    wb (Word8.fromInt (Int32.toInt (Int32.mod(w, 256))));
                    wb (Word8.fromInt (Int32.toInt (Int32.mod(Int32.div(w, 256), 256))));
                    wb (Word8.fromInt (Int32.toInt (Int32.mod(Int32.div(w, 256 * 256), 256))));
                    wb (Word8.fromInt (Int32.toInt (Int32.mod(Int32.div(w, 256 * 256 * 256), 256))))
                end

            fun wu32 (w : Word32.word) =
                let in
                    wb (Word8.fromInt (Word32.toInt (Word32.mod(w, 0w256))));
                    wb (Word8.fromInt (Word32.toInt (Word32.mod(Word32.div(w, 0w256), 0w256))));
                    wb (Word8.fromInt (Word32.toInt (Word32.mod(Word32.div(w, 0w256 * 0w256), 0w256))));
                    wb (Word8.fromInt (Word32.toInt (Word32.mod(Word32.div(w, 0w256 * 0w256 * 0w256), 0w256))))
                end

            val { databytes, formatsize,
                  blockalign, bytespersec,
                  nchannels, nframes, bits } = info w
            val filesize = bytesneeded w

            val () = if (case #frames w of
                             Bit8  v => Vector.all (fn ch => Vector.length ch = nframes) v
                           | Bit16 v => Vector.all (fn ch => Vector.length ch = nframes) v
                           | Bit32 v => Vector.all (fn ch => Vector.length ch = nframes) v)
                     then ()
                     else raise Wave "not all channels have the same number of samples!"
        in
            (* RIFF header *)
            wid "RIFF";
            (* minus RIFF, WAVE *)
            w32 (filesize - 8);
            wid "WAVE";

            (* Format header *)
            wid "fmt "; (* nb. space *)
            w32 (Int32.fromInt formatsize);
            w16 1; (* compression code 1 = PCM *)
            w16 (Int16.fromInt nchannels);
            wu32 (#samplespersec w);
            w32 (Int32.fromInt bytespersec);
            w16 (Int16.fromInt blockalign);
            w16 (case #frames w of
                     Bit8  _ => 8
                   | Bit16 _ => 16
                   | Bit32 _ => 32);
            
            (* w16 0; *) (* no extra format bytes *)

            (* Data chunk *)
            wid "data";
            w32 (Int32.fromInt databytes);
            (* ... *)
            case #frames w of
                Bit8 v => for 0 (nframes - 1)
                          (fn i =>
                           for 0 (nchannels - 1)
                           (fn c =>
                            wb (Vector.sub(Vector.sub(v, c), i))))
              | Bit16 v => for 0 (nframes - 1)
                           (fn i =>
                            for 0 (nchannels - 1)
                            (fn c =>
                             w16 (Vector.sub(Vector.sub(v, c), i))))
              | Bit32 v => for 0 (nframes - 1)
                           (fn i =>
                            for 0 (nchannels - 1)
                            (fn c =>
                             w32 (Vector.sub(Vector.sub(v, c), i))))
        end

    fun tobytes w =
        let
            val s = bytesneeded w
            val a = Array.array (s, 0w0 : w8)
            val p = ref 0
            fun wb b =
                let in
                    Array.update(a, !p, b);
                    p := !p + 1
                end
        in
            toany wb;
            Vector.tabulate (s, fn x => Array.sub(a, x))
        end

    fun tofile w f =
        let
            val f = BinIO.openOut f handle e => raise Wave ("could not open " ^ f ^ " for output: " ^ exnMessage e)
        in
            toany (fn b => BinIO.output1 (f, b)) w;
            BinIO.closeOut f
        end


end
