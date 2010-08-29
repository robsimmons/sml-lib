(* Computes lines in pixel space using Bresenham's algorithm. 

   TODO(twm): Could improve line, which has some awkwardness about that
   first position. Might be simpler and faster to just pass data
   and externalize the "next" function like in the original algorithm.
*)
signature BRESENHAM =
sig

  type coord = int * int
  type state

  (* line start finish

     Returns an iterator that generates points between start and finish,
     and a first coordinate. *)
  val line : coord -> coord -> { step : state -> (state * coord) option, 
                                 seed : state } * coord

  (* app f start finish
     Apply the function to every point from start to finish. *)
  val app : (coord -> unit) -> coord -> coord -> unit

  (* all f start finish 
     True if every point between the start and finish satisfies the predicate f. *)
  val all : (coord -> bool) -> coord -> coord -> bool

  (* exists f start finish
     True if any point between the start and finish satisfies the predicate f. *)
  val exists : (coord -> bool) -> coord -> coord -> bool

end