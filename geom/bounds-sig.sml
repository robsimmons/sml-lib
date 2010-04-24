(* Imperative 2D bounding box *)
signature BOUNDS =
sig

    exception Empty

    type bounds
    (* Starts with no points. *)
    val nobounds : unit -> bounds
    val boundpoint : bounds -> real * real -> unit

    (* Raises Empty if no points have ever been added. *)
    val getbounds : bounds -> { minx : real, maxx : real, miny : real, maxy : real }

    (* The offset of the point within the bounding box.
       Probably should only use this after you're done adding all the
       points to the bounds. *)
    val offsetx : bounds -> real -> real
    val offsety : bounds -> real -> real
    val width : bounds -> real
    val height : bounds -> real
    (* Modifies the left; leaves the right the same. Either may be empty. *)
    val union : bounds -> bounds -> unit

    (* Add a margin of fixed size around the entire bounds, in absolute units. 
       Must be non-empty. *)
    val addmargin : bounds -> real -> unit
    (* Add a margin that's a fraction of the longest dimension. If empty, does
       nothing. *)
    val addmarginfrac : bounds -> real -> unit

end