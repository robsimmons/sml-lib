structure Queue :> QUEUE =
struct

    (* The queue is, conceptually, front @ rev back.
       We delay the application of rev to back, and memoize this, so
       queues are references (memo cells) despite having a functional
       interface.

       Because we have a functional interface, we only should use a
       ref cell to represent the *same* queue (that is, different
       cleavings into a front and back list). Any time we push or pop,
       therefore, we have to make a new reference cell. *)
    type 'a queue = ( 'a list * 'a list ) ref

    (* empty queues have empty lists *)
    fun empty () = ref (nil,nil)
    fun isempty (ref (nil,nil)) = true
      | isempty _ = false

    (* enq inserts into the back *)
    fun enq (x, ref (front, back)) = ref (front, x :: back)

    (* put x at the front *)
    fun cons (x, ref (front, back)) = ref (x :: front, back)

    fun normalize (q as ref (front, back)) = q := (front @ rev back, nil)

    (* deq returns SOME of the head element of the front. If there's
       nothing in the front, it reverses the back, switches the two
       lists and returns SOME of the front. If the back list is empty,
       it returns NONE *)
    fun deq q =
      (case !q of
         (nil, nil) => (NONE, q)
       | (x::front, back) => (SOME x, ref (front, back))
       | (nil, back) => 
           let in
             (* memoizing the result... *)
             normalize q;
             deq q
           end)

    (* app over the queue apps on both lists *)
    fun app f (ref (front, back)) =
        let in
            List.app f front;
            List.app f back
        end

    fun map f (ref (front, back)) = ref (List.map f front, List.map f back)

    (* filter a queue *)
    fun filter f (ref (front, back)) =
      ref (List.filter f front,  List.filter f back)

    (* check to make sure all elements in queue satisfy a given predicate *)
    fun all f (ref (front, back)) =
      List.all f front andalso List.all f back

    (* create a queue from a list *)
    fun fromlist l = ref (l, nil)

    (* convert a queue to a list *)
    fun tolist (r as ref (front, back)) =
      let 
        val l = front @ List.rev back
      in
        (* might as well memoize this, since it is the same as normalization *)
        r := (l, nil);
        l
      end

end
