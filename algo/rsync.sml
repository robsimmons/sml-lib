
(* The Rsync hash algorithm is a weak hash on fixed length blocks of
   elements. Its primary feature is that it is a "rolling" hash, in
   that it is very cheap to compute the hash for e1...en given the
   hash for e0...e(n-1). This makes it especially useful when
   computing the hash for each overlapping block in a large vector of
   elements.

   The hash algorithm is fairly generic. It represents hash values
   using words, for some word size 2K. Each hash value is actually two
   smaller hashes, eack of size K. Therefore, this functor needs two
   word structures, HalfWord and FullWord. To get the original Rsync
   algorithm, instantiate this with Word16 and Word32. (SML/NJ does
   not support Word16 natively, so see ../misc/wordn.sml)

   Again, note that this hash algorithm is quite weak and collisions
   are relatively common. A stronger cryptographic hash should be used
   to verify that matches from the Rsync hash are real.

   *)

functor RsyncFn (type elt
                 type vector
                 structure HalfWord : WORD
                 structure FullWord : WORD
                 val word : elt -> HalfWord.word

                 val extend : HalfWord.word -> FullWord.word
                 val trunc  : FullWord.word -> HalfWord.word

                 val sub : vector * int -> elt
                   ) =
struct

  (* the rsync hash consists of two essentially separable 16-bit hashes.
     hash A is simply the sum of all the values mod M.
     hash B is the sum of the 1-based offset times the value, mod M.
     *)

  structure HW = HalfWord
  structure FW = FullWord

  infix 1 ++
  infix 1 --
  infix 9 **
  val op ++ = HW.+
  val op ** = HW.*
  val op -- = HW.-

  val halfbits = Word.fromInt HW.wordSize

  val iw = HW.fromInt

  exception Rsync of string

  val () = if FullWord.wordSize = HalfWord.wordSize * 2
           then ()
           else raise Rsync "FullWord is not twice the size of HalfWord in Rsync functor argument"

  (* single one, slowish *)
  fun hash(v, k, len) = 
    let
      fun a (acc, i) =
        if i >= len
        then acc
        else a (word(sub(v, k + i)) ++ acc, i + 1)

      fun b (acc, i) =
        if i >= len
        then acc
        else b (acc ++ (word(sub(v, k + i)) ** iw (i + 1)), 
                i + 1)

      val aa = (a (iw 0, 0))
      val bb = (b (iw 0, 0))
    in
      FW.orb(extend aa, FW.<<(extend bb, halfbits))
    end

  (* from the previous hash value (must be of (v, k - 1, len)),
     generate hash(v, k, len) more efficiently *)
  fun hash_next (v, prev, k, len) =
    let
      val a_prev = trunc prev
      val b_prev = trunc (FW.>>(prev, halfbits))


      val a_new = 
        (a_prev --
         (* subtract out the char right before this one starts *)
         word(sub(v, k - 1))) ++ 
         (* and add in the new one *)
         word(sub(v, k + len - 1))

      (* 
         b(k, l) = ( b(k-1,l-1) - ( ((l-1) - (k -1)) + 1)v(k-1) ) + a(k, l)
         mod 2^16
         *)
      val b_new = 
        (* we had 1x + 2y + 3z ... + (len)c,
           subtract x + y + z ... + c
           to get      1y + 2z ... + (len-1)c
           and then add (len)d 
           *)
        (b_prev -- a_prev) ++ (word(sub(v, k + len - 1)) ** iw len)
    in
      FW.orb(extend a_new, FW.<<(extend b_new, halfbits))
    end

end
