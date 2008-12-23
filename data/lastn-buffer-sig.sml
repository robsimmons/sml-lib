(* The LastNBuffer is an efficient fixed-size (N elements) buffer that
   stores (only) the last N elements pushed into it. Pushing is
   constant time and accessing any element by index is also constant
   time. (It is implemented as a circular array of size N.) *)
signature LASTNBUFFER =
sig

    type 'a buffer

    (* lastnbuffer n elt
       Creates a buffer of size n with n copies of elt in it. *)
    val buffer : int * 'a -> 'a buffer
    val tabulate : int * (int -> 'a) -> 'a buffer
    val fromList : 'a list -> 'a buffer

    (* if the front of the array (element 0) is at the left,
       rotate the contents to the left so that element zero
       becomes the final element, element 1 becomes the 0
       element, etc. *)
    val rotate_left : 'a buffer -> unit
    (* Inverse of the above. *)
    val rotate_right : 'a buffer -> unit

    (* Last element. Element zero falls off. *)
    val push_back : 'a buffer * 'a -> unit
    (* Element zero. An element falls off the back. *)
    val push_front : 'a buffer * 'a -> unit

    val sub : 'a buffer * int -> 'a
    val update : 'a buffer * int * 'a -> unit

    val length : 'a buffer -> int

    val appi : (int * 'a -> unit) -> 'a buffer -> unit
    val app : ('a -> unit) -> 'a buffer -> unit
  
end