
(* FIXME -- get real random numbers here ! 
   This is absolute rubbish for testing only!!! *)

structure CryptRand =
struct

    infix 6 ++ --
    infix 7 divv modd **
    infix 4 << >> <<= >>=
        
    structure N = IntInf

    val op ++ = N.+
    val op ** = N.*
    val op divv = N.div
    val op modd = N.mod

    val iton = N.fromInt

    val n0 = iton 0
    val n1 = iton 1
    val n2 = iton 2
    val n8 = iton 8
    val n256 = iton 256
    val n2_32 = N.pow(n2, 32)

    exception CryptRand of string

    val bad = ref (Word32.fromLargeInt (Time.toSeconds (Time.now())))


(* XXX from MLton.Random structure *)
      local
         fun make (file, name) =
            let
               val buf = Word8Array.array (4, 0w0)
            in
               fn () =>
               (let
                   val fd =
                      let
                         open Posix.FileSys
                      in
                         openf (file, O_RDONLY, O.flags [])
                      end
                   fun loop rem =
                      let
                         val n = Posix.IO.readArr (fd,
                                                   Word8ArraySlice.slice
                                                   (buf, 4 - rem, SOME rem))
                         val _ = if n = 0
                                    then (Posix.IO.close fd; raise Fail name)
                                 else ()
                         val rem = rem - n
                      in
                         if rem = 0
                            then ()
                         else loop rem
                      end
                   val _ = loop 4
                   val _ = Posix.IO.close fd
                in
                   SOME (Word.fromLarge (PackWord32Little.subArr (buf, 0)))
                end
                   handle OS.SysErr _ => NONE)
            end
      in
         val seed = make ("/dev/random", "Random.seed")
         val useed = make ("/dev/urandom", "Random.useed")
      end


    (* XXX not great; useed can return lower quality bits *)
    fun random () = valOf (useed ())
(*
      let
      in
        bad := Word32.+(!bad, 0w1);
        bad := Word32.*(!bad, 0w177801);
        bad := Word32.xorb(0wxF3014823, !bad);
        bad := Word32.orb(Word32.<<(!bad, 0w31),
                          Word32.>>(!bad, 0w1));

        (* OMG !!!! *)
        !bad
      end
*)
(*
        let in
            bad := Word32.*(!bad, 0w177801);
            bad := Word32.xorb(0wxF3014823, !bad);
            bad := Word32.+(!bad, 0w9999111);
            bad := Word32.>>(!bad, 0w1);
            bad := Word32.+(!bad, 0wx7);
            !bad
        end
*)

    (* XXX bad *)
    fun bit () = (Word32.andb(random (), 0wx100) > 0w0)

    (* good. *)
    fun byte () = chr (Word32.toInt (Word32.andb(0w255, random ())))

    fun bytes 0 = nil
      | bytes n = byte () :: bytes (n - 1)

    fun vec n = String.implode (bytes n)

    (* good. *)
    (* XXX PERF be smart -- use calls to byte() for more efficient
       use of randomness *)
    fun nbits bits =
        let
          fun gb acc 0 = acc
            | gb acc n =
            if n >= 32
            then 
              let 
                val accs = acc ** n2_32
                (* okay, since if IntInf is available,
                   LargeInt = IntInf = N *)
                val nr : N.int = Word32.toLargeInt (random ())
              in
                gb (accs ++ nr) (n - 32)
              end
            else
              if n >= 8
              then
                let
                  val accs = acc ** n256
                  val nr : N.int = Word32.toLargeInt (Word32.andb(0w255,
                                                                  random()))
                in
                  gb (accs ++ nr) (n - 8)
                end
              else
                (* last <8 bits are done individually *)
                gb (if bit () then acc ** n2 ++ n1 else acc ** n2) (n - 1)
        in
            gb n0 bits
        end

    (* good. *)
    fun inrange (low, hi) =
        let
            val range = N.-(hi, low)
            val _ = N.> (range, n0) orelse raise CryptRand "bad range to inrange"
            val rangel = 1 + Number.log2(range)

            fun try () =
                let val b = N.+(nbits rangel, low)
                in if N.< (b, hi) then b else try ()
                end
        in
            try ()
        end

end