signature STREAM =
sig

    type 'a stream
    datatype 'a front = Nil | Cons of 'a * 'a stream

    val force : 'a stream -> 'a front
    val delay : (unit -> 'a front) -> 'a stream
      
    val map : ('a -> 'b) -> 'a stream -> 'b stream
    val app : ('a -> 'b) -> 'a stream -> unit
      
    val filter : ('a -> bool) -> 'a stream -> 'a stream
    val append : 'a stream -> 'a stream -> 'a stream

    val foldr : ('a * 'b -> 'b) -> 'b -> 'a stream -> 'b
    val foldl : ('a * 'b -> 'b) -> 'b -> 'a stream -> 'b
      
    val tolist : 'a stream -> 'a list

    val empty : 'a stream

    exception Empty

    (* Baroque interface *)

    val cons : 'a * 'a stream -> 'a stream
    val lcons : 'a * (unit -> 'a stream) -> 'a stream
    val old_delay : (unit -> 'a stream) -> 'a stream

    val is_empty : 'a stream -> bool

    val uncons : 'a stream -> 'a * 'a stream
    val hd : 'a stream -> 'a
    val tl : 'a stream -> 'a stream
    val ltl : 'a stream -> 'a stream

end
