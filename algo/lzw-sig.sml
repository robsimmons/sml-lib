(* The well-known Lempel-Ziv-Welch (LZW) compression algorithm.
   The patent for this algorithm expired on 20 June 2003.
*)
signature LZW =
sig

  (* characters to encode/decode *)
  type ch
    
  type 'a stream = unit -> 'a option

  datatype output =
  (* CODE (c, b)
     the output is mainly a sequence of codes. as the
     table grows, the number of bits needed to encode
     the codes increases. here 'c' is the code and
     'b' is the number of bits needed to encode
     table indices at this step. *)
    CODE of int * int
    (* reset the table to initial state. this will
       only occur if the argument to the LZW functor
       fixes a maximum table size. *)
  | RESET

  exception LZW of string

  val compress : ch stream -> output stream
  val compress_array : ch Array.array -> output Array.array

  (* when in 'extended mode', decompression can be
     instructed to reset the table. Moreover, each
     character from the input stream is read with
     a variable number of bits. *)
  datatype input =
    ICODE of int
  | ORESET

  val decompress : int stream -> ch Array.array
  val decompressex : (int -> input) stream -> ch Array.array

end