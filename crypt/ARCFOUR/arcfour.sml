
(* Alleged RC4 algorithm.
   The RC4 name is trademarked by RSA DSI.
   This implementation is based on the algorithm
   published in Applied Cryptography. *)

structure ARCFOUR :> ARCFOUR =
struct

  structure W8 = Word8
  structure W8A = Word8Array
  structure W8V = Word8Vector
  
  type arcfour = { s : W8A.array,
                   i : W8.word ref,
                   j : W8.word ref }

  fun byte { s, i, j } =
    let
      fun sub x = W8A.sub(s, Word8.toInt x)
      fun set x e = W8A.update(s, Word8.toInt x, e)

      (* val () = print ("[0] i : " ^ W8.toString (!i) ^ " j : " ^ W8.toString (!j) ^ "\n") *)

      val () = i := !i + 0w1
      val () = j := !j + sub (!i)

      (* val () = print ("[1] i : " ^ W8.toString (!i) ^ " j : " ^ W8.toString (!j) ^ "\n") *)

      val ti  = sub (!i)
      val tj  = sub (!j)
    in
      (* print ("[2] ti : " ^ W8.toString ti ^ " tj : " ^ W8.toString tj ^ "\n"); *)
      set (!i) tj;
      set (!j) ti;
      
      sub (ti + tj)
    end

  fun discard a 0 = ()
    | discard a n = (ignore (byte a); discard a (n - 1))

  fun init v =
    let
      val s = W8A.tabulate(256, Word8.fromInt)
      val k = W8A.tabulate(256, fn i =>
                           W8V.sub(v, i mod W8V.length v))

      fun loop 0 _ _ = {s = s, j = ref 0w0, i = ref 0w0} : arcfour
        | loop n i j =
        let
          val j = j + W8A.sub(s, W8.toInt i) + W8A.sub(k, W8.toInt i)
          val t = W8A.sub(s, W8.toInt i)
        in
          W8A.update(s, W8.toInt i, W8A.sub(s, W8.toInt j));
          W8A.update(s, W8.toInt j, t);
          loop (n - 1) (i + 0w1) j
        end
    in
      loop 256 0w0 0w0
    end

end