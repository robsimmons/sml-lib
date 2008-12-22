
(* Extension to SML's IntInf that are useful for cryptography. *)

structure Number =
struct

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

  (* XXX to workaround MLton's misspelling. Should be "N.quotrem" *)
  fun quotrem (a, b) = (N.quot (a, b), N.rem (a, b))

  (* XXX to workaround mlton's missing function. Should be "N.log2" *)
  fun log2 a =
      let
          fun try acc n =
              if N.>= (acc, a) then n
              else try (acc ** n2) (n + 1)
      in
          try n1 0
      end

  (* modexp x y n
     computes x^y mod n *)
  fun modexp x y n =
      if y = n1 then x modd n
      else if y = n0 then n1
           else
               let 
                   val (q, r) = quotrem (y, n2)
                   val t = modexp x q n
                   val t = (t ** t) modd n
               in
                   if r = n0 then t
                   else (t ** x) modd n
               end 

  (* euclid(a, b)
     returns (g, x, y)
     such that ax + by = g
     and g = gcd(a, b). *)
  fun euclid(a, b) =
      if b = n0 then (a, n1, n0)
      else let
               fun loop a b x1 x2 y1 y2 =
                   if N.<=(b, n0) then (a, x2, y2)
                   else let
                            val q = a divv b
                            val r = a  -- q ** b
                            val x = x2 -- q ** x1
                            val y = y2 -- q ** y1
                        in
                            loop b r x x1 y y1
                        end
          in
              loop a b n0 n1 n1 n0
          end

end