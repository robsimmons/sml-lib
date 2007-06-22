(* Implementation of the CRC32 checksumming algorithm
   with polynomial 0wxEDB88320. *)

signature CRC32 =
sig

    val table : Word32.word Vector.vector

    (* calculate the 32-bit checksum for a string *)
    val crcstring : string -> Word32.word

    (* calculate the 32-bit checksum for a string,
       as if it were preceded by the a string with
       the given CRC value. *)
    val crcstringi : string -> Word32.word -> Word32.word

end
