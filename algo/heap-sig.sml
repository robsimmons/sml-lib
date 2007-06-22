
(* imperative priority queues, implemented as a heap. *)
signature HEAP =
sig

  exception Heap of string

  (* given by the argument to HeapFn *)
  type priority
    
  type 'a heap

  (* handle that can be used to adjust priority later.
     don't use a hand after the corresponding element has
     been removed by 'min' or 'delete'. 

     If used in the wrong heap, all bets are off! If you have a static
     number of heaps, I suggest instantiating the heap functor
     separately for each one, to give you a new type.

     (XXX use 'phantom types' to control type equality?)
     *)
  type hand

  val empty : unit -> 'a heap

  val min : 'a heap -> (priority * 'a) option

  val insert : 'a heap -> priority -> 'a -> hand

  (* same as delete followed by re-insert, but keeps
     old handle *)
  val adjust : 'a heap -> hand -> priority -> unit

  val get : 'a heap -> hand -> priority * 'a

  val delete : 'a heap -> hand -> unit

  (* is a handle still valid? *)
  val valid : hand -> bool

  (* for debugging *)
  val size : 'a heap -> int
  val printheap : ('a -> string) -> (priority -> string) -> 'a heap -> unit
  val handtostring : hand -> string

end
