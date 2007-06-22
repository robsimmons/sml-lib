
(* Alleged RC4 algorithm.
   The RC4 name is trademarked by RSA DSI.
   This implementation is based on the algorithm
   published in Applied Cryptography. *)

signature ARCFOUR =
sig

  type arcfour

  val init : Word8Vector.vector -> arcfour

  (* Calling this function repeatedly generates
     a stream of pseudo-random bytes. These should
     be XORed with the plaintext/ciphertext to
     encrypt or decrypt. *)
  val byte : arcfour -> Word8.word
    
  (* throw away n bytes. it is strongly
     recommended that new uses of arcfour
     discard at least 1024 bytes after
     initialization, to prevent against the
     2001 attack by Fluhrer, Mantin, and Shamir. *)
  val discard : arcfour -> int -> unit

end
