
signature BLOWFISH =
sig

  (* abstract type of decryption state. 
     you must create this from a key before
     encrypting or decrypting.
     *)
  type fish

    
  (* string can be up to 56 bytes. *)
  val init : string -> fish

  (* weak fish have duplicate entries in the S box.
     certain attacks are more efficient on these keys,
     though it's not generally something to worry about. *)
  val is_weak : fish -> bool

  val encrypt1 : fish -> Word32.word * Word32.word -> Word32.word * Word32.word
  val decrypt1 : fish -> Word32.word * Word32.word -> Word32.word * Word32.word

end

(* If you don't care about speed, just use the Blowfish structure. *)

(* Otherwise, you can get better speed by using a Word32Array structure,
   if your compiler supports it. Apply BlowfishFn to any structure matching
   the below signature, as in:

   structure FastBlowfish = BlowfishFn(Word32Array)
   
   Note that with mlton, Word32.word Array.word is compiled as Word32Array.array,
   so the provided Blowfish structure is fast already.
*)
signature W32ARRAY =
sig

  type array

  val fromList : Word32.word list -> array
  val sub : array * int -> Word32.word
  val update : array * int * Word32.word -> unit
  val foldl : (Word32.word * 'b -> 'b) -> 'b -> array -> 'b

end
