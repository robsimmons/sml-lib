
(* DES, the Data Encryption Standard.

   (DES is considered weak because of its short key length.
    Triple-DES or "3DES" is a better choice. 

    I wanted to support 3DES here, but it turns out that there
    are a number of different 3DES "standards," generally having
    to do with the feedback mode.) *)

signature DES =
sig

  type key
    
  (* this does a significant amount of initialization *)
  val key  : Word32.word * Word32.word -> key

  (* a single block *)
  val encrypt : 
    key ->
    Word32.word * Word32.word ->
    Word32.word * Word32.word

  val decrypt :
    key ->
    Word32.word * Word32.word ->
    Word32.word * Word32.word

end