
exception SSHProt of string

    fun wtov (i : Word32.word) =
        CharVector.fromList [chr (Word32.toInt 
                                  (Word32.andb(Word32.>>(i, 0w24), 0w255))),
                             chr (Word32.toInt
                                  (Word32.andb(Word32.>>(i, 0w16), 0w255))),
                             chr (Word32.toInt
                                  (Word32.andb(Word32.>>(i, 0w8), 0w255))),
                             chr (Word32.toInt
                                  (Word32.andb(i, 0w255)))]
        handle _ => raise SSHProt (print "exn at 1578"; print "\n"; "exn at 1578")

    fun ciphers session =
      let

        (* I can reproduce what putty does using this code.
           but it seems we must get the wrong keys from
           the session key. Maybe we are not doing that
           XOR stuff? 

           That would make sense -- the first 16 bytes cover
           the first two keys.
           *)
(*
        val read = Reader.fromvec session

        fun getkey () =
          let
            val l = Reader.rbw32 read
            val r = Reader.rbw32 read
          in 
            print ("key : " ^ Word32.toString l ^ " / " ^
                   Word32.toString r ^ "\n");
            DES.key (l, r)
          end

        val k1 = getkey ()
        val k2 = getkey ()
        val k3 = getkey ()

        *)

        val k1 = DES.key (0wx960DD7EB, 0wx6D2AD5FB)
        val k2 = DES.key (0wx777B8CB3, 0wxA73985CD);
        val k3 = DES.key (0wxDAD00C7B, 0wx77904266);

        (* IVs for encryption *)
        val ev1 = ref (0w0 : Word32.word, 0w0 : Word32.word)

        (* IVs for decryption *)
        val dv1 = ref (0w0 : Word32.word, 0w0 : Word32.word)

        fun encrypt (bl, br) =
          let
            (* round one - encrypt *)
            val bl = Word32.xorb(bl, #1 (!ev1))
            val br = Word32.xorb(br, #2 (!ev1))
            val (bl, br) = DES.encrypt k3 (bl, br)
            val (bl, br) = DES.decrypt k2 (bl, br)
            val (bl, br) = DES.encrypt k1 (bl, br)
            val _ = ev1 := (bl, br)
          in
            (bl, br)
          end

        fun decrypt (bl, br) =
          let
            (* round one - decrypt *)
            val bl = Word32.xorb(bl, #1 (!dv1))
            val br = Word32.xorb(br, #2 (!dv1))

            val (bl, br) = DES.decrypt k3 (bl, br)

            val _ = print ("after first:  " ^ Word32.toString bl ^ " / " ^
                           Word32.toString br ^ "\n")

            val (bl, br) = DES.encrypt k2 (bl, br)

            val _ = print ("after second:  " ^ Word32.toString bl ^ " / " ^
                           Word32.toString br ^ "\n")


            val (bl, br) = DES.decrypt k1 (bl, br)
            val _ = dv1 := (bl, br)
          in
            (bl, br)
          end

        fun debug f =
          (fn (l, r) =>
           let
             val _ = print ("in:  " ^ Word32.toString l ^ " / " ^
                            Word32.toString r ^ "\n")
             val (l, r) = f (l, r)
             val _ = print ("out: " ^ Word32.toString l ^ " / " ^
                            Word32.toString r ^ "\n")
           in
             (l, r)
           end)

      in
        { enc = debug encrypt,
          dec = debug decrypt }
      end

(*
    val session = 
      implode 
      (map chr
       [0x69, 0x3A, 0x98, 0xAF, 0x18, 0x91, 0xC4, 0x90, 0x10, 0x1D, 0xBE, 
        0x1B, 0xA3, 0x39, 0x63, 0x4F, 0x56, 0x9D, 0xF9, 0xC6, 0x41, 0x6B, 
        0x78, 0x6F, 0x2B, 0x4D, 0x14, 0xF1, 0x6A, 0x8C, 0x24, 0xF8])


    val dat = 
      (0wxEFB8FB32 : Word32.word, 
       0wxB2642944 : Word32.word)
*)

    val dat =
      (0wxCF2CD38A : Word32.word,
       0wx9677A07A : Word32.word)

    val xx = ciphers 0

(*
val _ =
  let 
(*    val k = DES.key (0wx55555555, 0wx55555555)
    val i = (0wxFFFFFFFF, 0wxFFFFFFFF) *)
(*    val k = DES.key (0wx01010101, 0wx01010101)
    val i = (0wx95F8A5E5, 0wxDD31D900) *)
(*
    val k = DES.key (0wx7CA11045, 0wx4A1A6E57)
    val i = (0wx01A1D6D0, 0wx39776742) *)

    val k = DES.key (0wx37D06BB5, 0wx16CB7546)
    val i = (0wx164D5E40, 0wx4F275232)

    val (el, er) = DES.encrypt k i
    val (el, er) = DES.decrypt k (el, er)
  in
    print (Word32.toString el ^ " / " ^
           Word32.toString er ^ "\n")
  end



exception Oops
val test = (valOf (IntInf.fromString "123456789123456789"))

fun burn_rsa 0 = print "Done.\n"
  | burn_rsa x =
  let

    val ((n,e), (_, d)) = RSA.keygen (fn _ => ()) 256
      
    val _ = print ("n: " ^ IntInf.toString n ^ "\n");
    val _ = print ("e: " ^ IntInf.toString e ^ "\n");
    val _ = print ("d: " ^ IntInf.toString d ^ "\n");
      
    val enc = RSA.crypt e n test
    val dec = RSA.crypt d n enc
    val _  = if test <> dec
             then (print ("FAILED! " ^ IntInf.toString dec ^ "\n");
                   raise Oops)
             else print "OK.\n"

    val enc2 = RSA.crypt d n test
    val dec2 = RSA.crypt e n enc2

    val _  = if test <> dec2
             then (print ("FAILED(2)! " ^ IntInf.toString dec2 ^ "\n");
                   raise Oops)
             else print "OK(2).\n"
  in
    burn_rsa (x - 1)
  end

(* val _ = burn_rsa 100 *)
*)