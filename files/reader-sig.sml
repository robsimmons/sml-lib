
(* A reader allows the extraction of vectors of characters from
   arbitrary points. We can think of it as an abstract interface to
   files on disk or in memory. This is intended to work around the
   bizarre omission in the BinIO basis structure of a way to seek
   arbitrarily in a file.

   The reader type is a record of all of the operations you can perform
   on it. All readers have the same type.

   The reader structure includes functions that generate readers from
   files or CharVector.vectors in memory. Recall that
   CharVector.vector is the same type as string. 
*)

signature READER =
sig

    exception Bounds
    exception Reader of string

    type reader =
        { size  : int,
          seek  : int -> unit,
          pos   : unit -> int,
          char  : unit -> char,
          vec   : int -> CharVector.vector,
          (* may invalidate any subreader or superreader
             XXX need clone () *)
          close : unit -> unit }

    (* CharVector.vector = string *)
    val fromvec : CharVector.vector -> reader

    val fromfile : string -> reader

    (* create a reader constrained to a certain size from the current
       reader. The new reader begins at the current seek point, and
       the old seek point is not modified. *)
    val fromreader : reader -> int -> reader

    (* run the function on the reader, but then return the position
       to its original location. (also through exceptions) *)
    val saveexcursion : (reader -> 'a) -> reader -> 'a

    (* read 16-bit and 32-bit ints from a file,
       little-endian or big-endian. *)

    val rl16 : reader -> int
    val rl32 : reader -> int
    val rb16 : reader -> int
    val rb32 : reader -> int

    val rlw16 : reader -> Word32.word
    val rlw32 : reader -> Word32.word
    val rbw16 : reader -> Word32.word
    val rbw32 : reader -> Word32.word

    (* skip n bytes, can be negative *)
    val skip : reader -> int -> unit

    (* tell if a reader doesn't have any more data *)
    val eof : reader -> bool

    (* read the remainder of the reader. Puts position at EOF. *)
    val rest : reader -> CharVector.vector

    (* grab a vector without altering the position *)
    val vecat : reader -> int -> int -> CharVector.vector

    (* grab a zero-terminated string without altering the position *)
    val strzat : reader -> int -> string

    (* without terminating carraige return or newline. 
       considers EOF to end the line, as well. *)
    val line : reader -> string option

    (* token f r 
       Read the next token (if any) from the reader r. A
       token is some sequence of characters for which f(c) is
       false. Leading characters for which f(c) is true are
       ignored. StringUtil.whitespec may be a useful value for f. *)
    val token : (char -> bool) -> reader -> string option

end
