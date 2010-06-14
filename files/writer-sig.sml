
(* Simple abstraction over output streams.
   Unlike Reader.reader, these are not (currently) seekable,
   so the interface is much simpler. *)
signature WRITER =
sig

    exception Writer of string
    type writer =
        { byte : Word8.word -> unit,
          (* Invalidates the writer. *)
          close : unit -> unit }

    (* From a named file. The writer is positioned at the beginning
       of the file. Caller is responsible for calling #close when done. *)
    val fromfile : string -> writer

    (* For these, close does nothing. *)
    val fromgrow8array : GWord8Array.growarray -> writer
    val fromgrowchararray : GCharArray.growarray -> writer
    val newgrow8array : unit -> GWord8Array.growarray * writer
    val newgrowchararray : unit -> GCharArray.growarray * writer

    (* utilities... *)
    val wstring : writer -> string -> unit

    (* Write n copies of the byte *)
    val nbytes : writer -> Word8.word -> int -> unit
    val nchars : writer -> char -> int -> unit

end