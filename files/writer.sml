
structure Writer :> WRITER =
struct

    exception Writer of string

    type writer =
        { byte : Word8.word -> unit,
          (* Invalidates the writer. *)
          close : unit -> unit }

    fun fromfile filename =
        let
            val f = BinIO.openOut filename
        in
            { byte = (fn w => BinIO.output1(f, w)),
              close = (fn () => BinIO.closeOut f) }
        end

    fun fromgrow8array ga =
        { byte = GWord8Array.append ga,
          close = ignore }

    fun newgrow8array () =
        let val ga = GWord8Array.empty ()
        in (ga, fromgrow8array ga)
        end

    fun fromgrowchararray ga =
        { byte = GCharArray.append ga o chr o Word8.toInt,
          close = ignore }

    fun newgrowchararray () =
        let val ga = GCharArray.empty ()
        in (ga, fromgrowchararray ga)
        end

    fun wstring ({ byte, ... } : writer) s =
        CharVector.app (fn c =>
                        byte (Word8.fromInt (ord c))) s

    fun nbytes ({ byte, ... } : writer) w n =
        let fun nb 0 = ()
              | nb n = (byte w; nb (n - 1))
        in nb n
        end

    fun nchars w c n = nbytes w (Word8.fromInt (ord c)) n
end
