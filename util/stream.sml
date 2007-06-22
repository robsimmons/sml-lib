
(* This structure is the unholy marriage of two competing
   implementations of streams, which accounts for its
   ugliness. It should be simplified, and parse should
   be rewritten in terms of the new interface.

   parse doesn't use 'em any more, so just check that they're
   not used anywhere else and then get rid of the baroque stuff *)

structure Stream :> STREAM =
struct

    exception Empty

    type 'a susp = unit -> 'a

    datatype 'a front = Nil | Cons of 'a * 'a stream
    withtype 'a stream = 'a front susp

    fun force s = s()
    fun forceTo f s = f(s())

    fun delay (s : unit -> 'a front) : 'a stream =
      let
        val r = ref (fn () => raise Match)
      in
        r := (fn () => 
              let val ss = s ()
              in 
                r := (fn () => ss);
                ss
              end);
        (fn () => (!r) ())
      end


    (* why double delay? *)
    fun old_delay s =
        let
            exception Impossible
            val memo = ref (fn () => raise Impossible)
            fun s'() = let val r = s()() in memo := (fn () => r); r end
        in
            memo := s';
            fn () => (!memo)()
        end

    fun append s1 s2 =
        let
            fun first () =
              case force s1 of
                 Nil => force s2
               | Cons(x,tt) => Cons(x, append tt s2)
        in
            first
        end

    val empty = fn () => Nil
    fun cons (h, t) = fn () => Cons (h,t)

    fun lcons (x, f) = cons (x, old_delay f)

    fun is_empty s = forceTo
        (fn Nil => true
          | _ => false) s

    fun uncons s = forceTo
        (fn Nil => raise Empty
          | Cons p => p) s

    fun hd s = #1 (uncons s)
    fun tl s = #2 (uncons s)
    fun ltl s = old_delay (fn () => tl s)

    fun map' f s () = forceTo
        (fn Nil => empty
          | Cons (h, t) => lcons (f h, map' f t)) s
    fun map f s = old_delay (map' f s)

    fun app f s = forceTo
        (fn Nil => ()
          | Cons (h, t) => (ignore(f h); app f t)) s

    fun foldr a b s = forceTo
        (fn Nil => b
          | Cons (h, t) => a (h, foldr a b t)) s

    fun foldl a b s = forceTo
        (fn Nil => b
          | Cons (h, t) => foldl a (a (h, b)) t) s

    fun filter f s = delay (filt' f s)
    and filt' f s () =
      case force s of
        Nil => Nil
      | Cons(a, rest) => 
          if f a then Cons(a, filter f rest)
          else filt' f rest ()

    (* don't bother memoizing *)
    fun fromList nil = (fn () => Nil)
      | fromList (h::t) = (fn () => Cons(h, fromList t))

    fun tolist s = foldr op:: nil s

end
