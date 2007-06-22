
signature DIGEST =
sig
  (* takes a string of any length to a short string.
     The input string may contain any characters 0-255,
     and the output string almost certainly will. *)
  val digest : string -> string
  val digestlen : int
end
