(* Functional queues. *)
signature QUEUE =
sig

    type 'a queue

    (* empty queue *)
    val empty : unit -> 'a queue

    (* isempty q
       true iff q contains no elements *)
    val isempty : 'a queue -> bool

    (* enq (x, q)
       enqueue x in q (at the end) *)
    val enq : 'a * 'a queue -> 'a queue

    (* cons (x, q)
       cons x onto the front of q *)
    val cons : 'a * 'a queue -> 'a queue

    (* deq q
       dequeue head element from from 'q' *)
    val deq : 'a queue -> 'a option * 'a queue

    (* app f q
       apply f to every element of q in an arbitrary order *)
    val app : ('a -> unit) -> 'a queue -> unit
      
    (* map f q
       produce a new queue created from f applied to the elements of q.
       the function is applied to elements in an arbitrary order, but 
       the new queue has the same order as the old queue. *)
    val map : ('a -> 'b) -> 'a queue -> 'b queue

    (* filter p q
       evaluates to a queue containing only the elements of q 
       for which p returns true.
       p is applied in an arbitrary order. *)
    val filter : ('a -> bool) -> 'a queue -> 'a queue

    (* all p q
       true iff p is true for every element of q. 
       p is applied in an arbitrary order. *)
    val all : ('a -> bool) -> 'a queue -> bool

    (* convert to and from lists *)
    val tolist   : 'a queue -> 'a list
    val fromlist : 'a list -> 'a queue

end
