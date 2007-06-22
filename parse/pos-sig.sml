(* Chris Okasaki / Robert Harper / Tom Murphy VII
   School of Computer Science
   Carnegie Mellon University
   Pittsburgh, PA 15213
*)

(* positions within a file (for reporting errors) *)
signature POS =
sig

  type pos

  val markstream : char Stream.stream -> (char * pos) Stream.stream
  (* alternately, pass in a filename *)
  val markstreamex : string -> char Stream.stream -> (char * pos) Stream.stream

  val markany : 'a Stream.stream -> ('a * pos) Stream.stream

  val initpos   : pos
  val initposex : string -> pos
  val nextchar  : pos -> pos
  val nextline  : pos -> pos
  (* to roll your own markstream for non-strings *)

  val rightedge : pos -> pos

  val union  : pos * pos -> pos
  val max    : pos * pos -> pos
  val min    : pos * pos -> pos
  (* if positions are ranges (start,finish) :          *)
  (*   union ((s,f),(s',f')) = (min (s,s'),max (f,f')) *)
  (*   max   ((s,f),(s',f')) = (max (s,s'),max (f,f')) *)
  (*   min   ((s,f),(s',f')) = (min (s,s'),min (f,f')) *)

  val toString : pos -> string

  (* when reading a position, it is always returned as a pair
     of the left edge and right edge, since a position may
     delimit a range. If a point, then these are equal. *)

  (* absolute source file position *)
  val getabs : pos -> int * int

  val getcol : pos -> int * int
end
