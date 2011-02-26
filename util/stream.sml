
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

    fun unfold f =
        let
            fun s a () =
                case f a of
                    NONE => Nil
                  | SOME x => Cons(x, s x)
        in
            s
        end

    fun countup min NONE =
        let fun s x () = Cons(x, s (x + 1))
        in s min
        end
      | countup min (SOME max) =
        let fun s x () = if x <= max
                         then Cons(x, s (x + 1))
                         else Nil
        in s min
        end

    fun countdown max NONE =
        let fun s x () = Cons(x, s (x - 1))
        in s max
        end
      | countdown max (SOME min) =
        let fun s x () = if x >= min
                         then Cons(x, s (x - 1))
                         else Nil
        in s max
        end


    val empty = fn () => Nil
    fun singleton x () = Cons (x, empty)
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
    fun fromlist nil = (fn () => Nil)
      | fromlist (h::t) = (fn () => Cons(h, fromlist t))

    fun tolist s = foldr op:: nil s

    (* PERF: This can be much more efficient, like by building a kinda
       co-heap. The data structure would be fairly limited though in
       the sense that we can't know what the streams contain beyond
       their horizons, so it's hard to keep it balanced. But at least
       we don't need to compare to a linear (in l) number of elements each
       time. *)
    fun merge_sorted cmp l =
      let
          (* Return a stream consisting of all the elements,
             in sorted order. *)
          fun ms nil () = Nil
            | ms (s :: t) () =
              case force s of
                  (* Skip empty streams. *)
                  Nil => ms t ()
                | Cons (v, ss) =>
                  (* Have a value, but it might not be
                     the smallest one. *)
                  ms_insert v [ss] t

          (* bv is the smallest value so far.
             sg is the list of streams known to contain
             only elements larger than it. *)
          and ms_insert bv sg nil =
              Cons (bv, delay (ms sg))
            | ms_insert bv sg (s :: t) =
              case force s of
                  (* Skip empty streams. *)
                  Nil => ms_insert bv sg t
                | Cons (v, ss) =>
                    case cmp (bv, v) of
                      (* Better. *)
                        GREATER =>
                          (* FYI, adding the singleton list bv is 
                             the real tragedy here. *)
                          ms_insert v (singleton bv :: ss :: sg) t
                      (* Same or worse. Don't take it. *)
                      | _ => ms_insert bv (s :: sg) t
      in
          delay (ms l)
      end


end
