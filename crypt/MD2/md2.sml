
(* The MD2 algorithm by Rivest. Code by Tom 7 in 2000. 

   Note: this code is incomplete and will be incompatible with other
   implementations. Needed: correct checksum algorithm.

*)

structure MD2 :> DIGEST =
struct

  structure W8 = Word8
  structure CV = CharVector
  type byte = W8.word

    (* MD2 produces digests 16 bytes long *)
  val digestlen = 16

  local 

    val table = CharVector.fromList (map chr
     [ 41, 46, 67, 201, 162, 216, 124, 1, 61, 54, 84, 161, 236, 240, 6,
       19, 98, 167, 5, 243, 192, 199, 115, 140, 152, 147, 43, 217, 188,
       76, 130, 202, 30, 155, 87, 60, 253, 212, 224, 22, 103, 66, 111, 24,
       138, 23, 229, 18, 190, 78, 196, 214, 218, 158, 222, 73, 160, 251,
       245, 142, 187, 47, 238, 122, 169, 104, 121, 145, 21, 178, 7, 63,
       148, 194, 16, 137, 11, 34, 95, 33, 128, 127, 93, 154, 90, 144, 50,
       39, 53, 62, 204, 231, 191, 247, 151, 3, 255, 25, 48, 179, 72, 165,
       181, 209, 215, 94, 146, 42, 172, 86, 170, 198, 79, 184, 56, 210,
       150, 164, 125, 182, 118, 252, 107, 226, 156, 116, 4, 241, 69, 157,
       112, 89, 100, 113, 135, 32, 134, 91, 207, 101, 230, 45, 168, 2, 27,
       96, 37, 173, 174, 176, 185, 246, 28, 70, 97, 105, 52, 64, 126, 15,
       85, 71, 163, 35, 221, 81, 175, 58, 195, 92, 249, 206, 186, 197,
       234, 38, 44, 83, 13, 110, 133, 40, 132, 9, 211, 223, 205, 244, 65,
       129, 77, 82, 106, 220, 55, 200, 108, 193, 171, 250, 36, 225, 123,
       8, 12, 189, 177, 74, 120, 136, 149, 139, 227, 99, 232, 109, 233,
       203, 213, 254, 59, 0, 29, 57, 242, 239, 183, 14, 102, 88, 208, 228,
       166, 119, 114, 248, 235, 117, 75, 10, 49, 68, 80, 180, 143, 237,
       31, 26, 219, 153, 141, 51, 159, 17, 131, 20 ])

  in

    fun S t = CV.sub (table, ord t)

  end
           
  (* like Array.array *)
  fun stringof (i, c) = implode (List.tabulate (i, fn _ => c))

  (* this pads even if s is already a multiple! *)
  fun padtomult n s = 
           let 
             val i = n - (size s mod n)
           in
             s ^ stringof (i, chr i)
           end

  (* CharVector iterator. 
   iterl 
   : (CharVector.elem * 'a -> CharVector.elem * 'a)
      -> 'a -> CharVector.vector -> CharVector.vector * 'a
   *)
  fun iterl f b v =
           let
             val (l, t) =
               CharVector.foldr (fn (a, (l,t)) =>
                                 let val (e, t) = f (a, t)
                                 in (e::l, t)
                                 end) (nil, b) v
           in
             (CharVector.fromList l, t)
           end


  fun w8 c : W8.word = Word8.fromInt (ord c)

  fun XOR (c1, c2) : char = chr (W8.toInt (W8.xorb(w8 c1, w8 c2)))

  fun dup x = (x,x)

  (* _Applied Cryptography_ does not explain the checksum algorithm *)
  fun checksum msg = "0000000000000000"

  fun digest msg =
     let
       val msg = padtomult 16 msg
       val M = msg ^ checksum msg

       val blocks = size msg div 16

       val _ = print M
       val _ = print ("\nblocks: " ^ Int.toString blocks ^"\n")

       fun loop_i (X, i) =
         if i = blocks then
           X
         else 
           let
             (* do the shift operation on X *)
             val X = 
               (String.substring (X, 0, 16)) ^
               (CV.mapi (fn (u, c) => CV.sub(M, i*16 + (u-16)))
                       (X, 16, SOME 32)) ^
               (CV.mapi (fn (u, c) => XOR(CV.sub(X, u - 32),
                                         CV.sub(X, u - 16)))
                       (X, 32, NONE))

             val _ = print ("\n in iter " ^ Int.toString i ^
                            ", X: " ^ X ^"\n")

             fun loop_j ((X, _), 18) = X
               | loop_j ((X, t), j) =
               let
                 val (X, t) = iterl (fn (e, t) =>
                                     dup(XOR(S t, e))) (chr t) X
               in
                 loop_j ((X, (ord t + j) mod 256), j + 1)
               end

           in

             loop_i (loop_j ((X, 0), 0), i + 1)
           end

     in
       CV.extract(loop_i (stringof (48, chr 0), 0), 0, SOME 16)
     end

end
