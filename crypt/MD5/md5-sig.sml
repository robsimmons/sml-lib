
(* Implementation of an MD5-compatible hash function.
   Written by Tom 7 in 2001; code in the public domain. *)

(* 

   NOTE:

   MD5 has known collisions and is considered weak. Are you sure you
   want to use this algorithm? The SHA1 implementation that's part of
   SML-lib has a similar interface (except it can be more efficient by
   allowing streamed access) and is still considered secure.

*)

signature MD5 =
sig

  (* Perform the MD5 hash function on a message.
     Returns the 128 bit (16 byte) hash.

     recall that string = CharVector.vector.
     The input string may contain non-ascii data,
     the output almost certainly will. *)

  val md5 : string -> string

  (* XXX add stream-oriented hash functions from SHA1 
     implementation *)

  (* convert a binary string to one built of hex digits *)
  val bintohex : string -> string

  (* Same, but supply the initialization vector manually. *)
  val md5_advanced : { iv : Word32.word * Word32.word * Word32.word * Word32.word,
                       msg : string } -> string
  (* The reference IV. *)
  val initialization_vector : Word32.word * Word32.word * Word32.word * Word32.word                  

end
