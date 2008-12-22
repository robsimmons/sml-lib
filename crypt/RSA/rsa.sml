
structure RSA =
struct

  exception RSA

  infix 6 ++ --
  infix 7 divv modd **
  infix 4 << >> <<= >>=

  structure N = IntInf

  val op ++ = N.+
  val op ** = N.*
  val op -- = N.-
  val op divv = N.div
  val op modd = N.mod

  val iton = N.fromInt

  val n0 = iton 0
  val n1 = iton 1
  val n2 = iton 2

  fun keygen callback bits =
      let
        (* XXX need to avoid weak keys.
           p and q should not be "too close"
           no p-1 and q-1 with small factors
           d must be "large enough" *)
          val p = Prime.makeprime callback 50 bits
          val q = Prime.makeprime callback 50 bits
          val n = p ** q
          val e = iton 65537
          val phi = (p -- n1) ** (q -- n1)
          val (g, x, y) = Number.euclid(phi, e)
          val _ = print ("g: " ^ N.toString g ^ "\n" ^
                         "x: " ^ N.toString x ^ "\n" ^
                         "y: " ^ N.toString y ^ "\n")
          val _ =
            if (phi ** x ++ e ** y) <> g
            then (print ("euclid was wrong\n");
                  raise RSA)
            else print ("euclid ok\n")

          val d = if N.<(y,n0) then phi ++ y
                  else y

          val _ =
            if (d ** e) modd phi <> n1
            then (print "d * e isn't congruent to 1 mod phi\n";
                  raise RSA)
            else print "d * e === 1   (mod phi)\n"
      in
          ((n, e), (n, d))
      end

  (* en/decrypt one block *)

  fun crypt key n msg =
      Number.modexp msg key n

end