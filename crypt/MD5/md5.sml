
(* RFC-1321 (MD5) compliant hashing function. 
   By Tom 7, 2001: Code placed in the Public Domain.
*)

structure MD5 :> MD5 =
struct
  exception Unimplemented

  val xorb = Word32.xorb
  val andb = Word32.andb
  val orb  = Word32.orb
  val <<   = Word32.<<
  val >>   = Word32.>>
  val notb = Word32.notb

  type w32 = word
  infix xorb andb orb << >>

  (* workaround for andb bug in MLton 20010706 *)
  fun mkbyte w = Word32.mod (w, 0w256)

  fun F(X,Y,Z) = (X andb Y) orb (notb X andb Z)
  fun G(X,Y,Z) = (X andb Z) orb (Y andb notb Z)
  fun H(X,Y,Z) = X xorb Y xorb Z
  fun I(X,Y,Z) = Y xorb (X orb notb Z)

  fun ROL(X, N) = (X << N) orb (X >> (0w32-N))

  fun wc hi lo = (hi << 0w16) orb lo

  fun w2b w = map chr 
    [Word32.toInt (mkbyte w),
     Word32.toInt (mkbyte (w >> 0w8)),
     Word32.toInt (mkbyte (w >> 0w16)),
     Word32.toInt (mkbyte (w >> 0w24))]

  (* pad a message (in the prescribed manner) to be congruent to
     56 bytes (mod 64) *)
  fun pad m l =
    let val v = l mod 64
        val p = if v < 56 then 56 - v else 120 - v
    in m ^ (str (chr 0x80)) ^
       implode (List.tabulate (p - 1, fn _ => chr 0))
    end

  (* append the length as a 64-bit quantity. *)
  fun applen m l =
    m ^
    implode (w2b (Word32.fromInt (l * 8))) ^
    implode (List.tabulate (4, fn _ => chr 0))

  fun FF (a, b, c, d, x, s, ac) =
    ROL(a + F(b, c, d) + x + ac, s) + b

  fun GG (a, b, c, d, x, s, ac) =
    ROL(a + G(b, c, d) + x + ac, s) + b

  fun HH (a, b, c, d, x, s, ac) =
    ROL(a + H(b, c, d) + x + ac, s) + b

  fun II (a, b, c, d, x, s, ac) =
    ROL(a + I(b, c, d) + x + ac, s) + b

  fun doblock (aa,bb,cc,dd) x =
    let
      (* get the nth logical byte from the input block *)
      val a = aa
      val b = bb
      val c = cc
      val d = dd

      val a = FF (a, b, c, d, x  0, 0w7,  wc 0wxd76a 0wxa478)
      val d = FF (d, a, b, c, x  1, 0w12, wc 0wxe8c7 0wxb756)
      val c = FF (c, d, a, b, x  2, 0w17, wc 0wx2420 0wx70db)
      val b = FF (b, c, d, a, x  3, 0w22, wc 0wxc1bd 0wxceee)
      val a = FF (a, b, c, d, x  4, 0w7,  wc 0wxf57c 0wx0faf)
      val d = FF (d, a, b, c, x  5, 0w12, wc 0wx4787 0wxc62a)
      val c = FF (c, d, a, b, x  6, 0w17, wc 0wxa830 0wx4613)
      val b = FF (b, c, d, a, x  7, 0w22, wc 0wxfd46 0wx9501)
      val a = FF (a, b, c, d, x  8, 0w7,  wc 0wx6980 0wx98d8)
      val d = FF (d, a, b, c, x  9, 0w12, wc 0wx8b44 0wxf7af)
      val c = FF (c, d, a, b, x 10, 0w17, wc 0wxffff 0wx5bb1)
      val b = FF (b, c, d, a, x 11, 0w22, wc 0wx895c 0wxd7be)
      val a = FF (a, b, c, d, x 12, 0w7,  wc 0wx6b90 0wx1122)
      val d = FF (d, a, b, c, x 13, 0w12, wc 0wxfd98 0wx7193)
      val c = FF (c, d, a, b, x 14, 0w17, wc 0wxa679 0wx438e)
      val b = FF (b, c, d, a, x 15, 0w22, wc 0wx49b4 0wx0821)


      val a = GG (a, b, c, d, x  1, 0w5,  wc 0wxf61e 0wx2562)
      val d = GG (d, a, b, c, x  6, 0w9,  wc 0wxc040 0wxb340)
      val c = GG (c, d, a, b, x 11, 0w14, wc 0wx265e 0wx5a51)
      val b = GG (b, c, d, a, x  0, 0w20, wc 0wxe9b6 0wxc7aa)
      val a = GG (a, b, c, d, x  5, 0w5,  wc 0wxd62f 0wx105d)
      val d = GG (d, a, b, c, x 10, 0w9,  wc 0wx0244 0wx1453)
      val c = GG (c, d, a, b, x 15, 0w14, wc 0wxd8a1 0wxe681)
      val b = GG (b, c, d, a, x  4, 0w20, wc 0wxe7d3 0wxfbc8)
      val a = GG (a, b, c, d, x  9, 0w5,  wc 0wx21e1 0wxcde6)
      val d = GG (d, a, b, c, x 14, 0w9,  wc 0wxc337 0wx07d6)
      val c = GG (c, d, a, b, x  3, 0w14, wc 0wxf4d5 0wx0d87)
      val b = GG (b, c, d, a, x  8, 0w20, wc 0wx455a 0wx14ed)
      val a = GG (a, b, c, d, x 13, 0w5,  wc 0wxa9e3 0wxe905)
      val d = GG (d, a, b, c, x  2, 0w9,  wc 0wxfcef 0wxa3f8)
      val c = GG (c, d, a, b, x  7, 0w14, wc 0wx676f 0wx02d9)
      val b = GG (b, c, d, a, x 12, 0w20, wc 0wx8d2a 0wx4c8a)

      val a = HH (a, b, c, d, x  5, 0w4,  wc 0wxfffa 0wx3942)
      val d = HH (d, a, b, c, x  8, 0w11, wc 0wx8771 0wxf681)
      val c = HH (c, d, a, b, x 11, 0w16, wc 0wx6d9d 0wx6122)
      val b = HH (b, c, d, a, x 14, 0w23, wc 0wxfde5 0wx380c)
      val a = HH (a, b, c, d, x  1, 0w4,  wc 0wxa4be 0wxea44)
      val d = HH (d, a, b, c, x  4, 0w11, wc 0wx4bde 0wxcfa9)
      val c = HH (c, d, a, b, x  7, 0w16, wc 0wxf6bb 0wx4b60)
      val b = HH (b, c, d, a, x 10, 0w23, wc 0wxbebf 0wxbc70)
      val a = HH (a, b, c, d, x 13, 0w4,  wc 0wx289b 0wx7ec6)
      val d = HH (d, a, b, c, x  0, 0w11, wc 0wxeaa1 0wx27fa)
      val c = HH (c, d, a, b, x  3, 0w16, wc 0wxd4ef 0wx3085)
      val b = HH (b, c, d, a, x  6, 0w23, wc 0wx0488 0wx1d05)
      val a = HH (a, b, c, d, x  9, 0w4,  wc 0wxd9d4 0wxd039)
      val d = HH (d, a, b, c, x 12, 0w11, wc 0wxe6db 0wx99e5)
      val c = HH (c, d, a, b, x 15, 0w16, wc 0wx1fa2 0wx7cf8)
      val b = HH (b, c, d, a, x  2, 0w23, wc 0wxc4ac 0wx5665)

      val a = II (a, b, c, d, x  0, 0w6,  wc 0wxf429 0wx2244)
      val d = II (d, a, b, c, x  7, 0w10, wc 0wx432a 0wxff97)
      val c = II (c, d, a, b, x 14, 0w15, wc 0wxab94 0wx23a7)
      val b = II (b, c, d, a, x  5, 0w21, wc 0wxfc93 0wxa039)
      val a = II (a, b, c, d, x 12, 0w6,  wc 0wx655b 0wx59c3)
      val d = II (d, a, b, c, x  3, 0w10, wc 0wx8f0c 0wxcc92)
      val c = II (c, d, a, b, x 10, 0w15, wc 0wxffef 0wxf47d)
      val b = II (b, c, d, a, x  1, 0w21, wc 0wx8584 0wx5dd1)
      val a = II (a, b, c, d, x  8, 0w6,  wc 0wx6fa8 0wx7e4f)
      val d = II (d, a, b, c, x 15, 0w10, wc 0wxfe2c 0wxe6e0)
      val c = II (c, d, a, b, x  6, 0w15, wc 0wxa301 0wx4314)
      val b = II (b, c, d, a, x 13, 0w21, wc 0wx4e08 0wx11a1)
      val a = II (a, b, c, d, x  4, 0w6,  wc 0wxf753 0wx7e82)
      val d = II (d, a, b, c, x 11, 0w10, wc 0wxbd3a 0wxf235)
      val c = II (c, d, a, b, x  2, 0w15, wc 0wx2ad7 0wxd2bb)
      val b = II (b, c, d, a, x  9, 0w21, wc 0wxeb86 0wxd391)
    in
      (a + aa, b + bb, c + cc, d + dd)
    end

  fun b2w (a, b, c, d) =
    (Word32.fromInt (ord a)) +
    (Word32.fromInt (ord b) << 0w8)  +
    (Word32.fromInt (ord c) << 0w16) +
    (Word32.fromInt (ord d) << 0w24)   

  fun md5_advanced {iv = (a, b, c, d), msg} =
    let
      val len = size msg
      val m = pad msg len
      val m = applen m len

      val len = size m

      fun mkx off x =
        b2w (CharVector.sub (m, off + (x*4)    ),
             CharVector.sub (m, off + (x*4) + 1),
             CharVector.sub (m, off + (x*4) + 2),
             CharVector.sub (m, off + (x*4) + 3))

      fun loop (a, b, c, d) off =
          if off = len then (a, b, c, d)
          else loop (doblock (a, b, c,d) (mkx off)) (off + 64)

      val (a, b, c, d) = loop (a, b, c, d) 0
    in
      implode (w2b a @ w2b b @ w2b c @ w2b d)
    end

  val initialization_vector = (wc 0wx6745 0wx2301,
                               wc 0wxefcd 0wxab89,
                               wc 0wx98ba 0wxdcfe,
                               wc 0wx1032 0wx5476)

  fun md5 m = md5_advanced {iv = initialization_vector, msg = m}

  val digits = "0123456789ABCDEF"
  fun bintohex s =
    String.translate (fn c =>
                      implode [CharVector.sub (digits, ord c div 16),
                               CharVector.sub (digits, ord c mod 16)]) s
end
