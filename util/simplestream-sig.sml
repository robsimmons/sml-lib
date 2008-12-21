
(* This alternate imperative stream type is 
   designed, using only types available at
   the top-level, so that imperative libraries
   can avoid a dependency on the Stream.stream
   source code. 

   This collection of utilities is therefore
   intended for use by clients, but not by
   libraries. *)
signature SIMPLESTREAM =
sig

  type 'a stream = unit -> 'a option
    
  val map : ('a -> 'b) -> 'a stream -> 'b stream

  val fromstring : string -> char stream
  val fromfile : string -> BinIO.elem stream
  val fromlist : 'a list -> 'a stream
  val fromvector : 'a Vector.vector -> 'a stream
  val fromarray : 'a Array.array -> 'a stream
  (* Give the largest chunk size to return. The chunks
     may be smaller than the requested size, particularly
     the final chunk, but never zero length. *)
  val fromfilechunks : int -> string -> string stream
      
  val tolist : 'a stream -> 'a list
  val toarrayslice : 'a stream -> 'a ArraySlice.slice
  val tovector : 'a stream -> 'a Vector.vector
  val tofile : string -> char stream -> unit
  (* BinIO.elem = Word8.word *)
  val tobinfile : string -> BinIO.elem stream -> unit

  val flatten : 'a stream stream -> 'a stream
  val app : ('a -> 'b) -> 'a stream -> unit

end
