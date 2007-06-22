
(* utilities for stream package *)

signature STREAMUTIL =
sig

    (* converts a string to a char stream *)
    val stostream : string -> char Stream.stream

    (* convert a file to a char stream *)
    val ftostream : string -> char Stream.stream

    (* convert a list to a stream *)
    val ltostream : 'a list -> 'a Stream.stream

    val concat : 'a Stream.stream -> 'a Stream.stream -> 'a Stream.stream

    val flatten : 'a Stream.stream Stream.stream -> 'a Stream.stream

    val headn : int -> 'a Stream.stream -> 'a list

end
