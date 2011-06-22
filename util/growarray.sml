structure GrowArray :> GROWARRAY =
struct

  type 'a growarray = (int * ('a option) Array.array) ref

  val eq = op = : 'a growarray * 'a growarray -> bool

  (* start with 16 cells to avoid doing a lot of doubling
     early. PERF: Could tune this on some set of programs? *)
  fun empty () = ref (0, Array.array(16, NONE))

  fun clear r = r := (0, Array.array(16, NONE))

  fun growarray n i = ref (n, (Array.array(n, SOME i)))

  fun sub (ref (used, a)) n =
    if n < used andalso n >= 0
    then (case Array.sub(a, n) of
            NONE => raise Subscript
          | SOME z => z)
    else raise Subscript

  fun length (ref (l, _)) = l

  fun app f (ref (l, a)) =
      let
          fun ap n = 
              if n < l
              then ((case Array.sub(a, n) of
                        NONE => ()
                      | SOME x => f x);
                    ap (n + 1))
              else ()
      in
          ap 0
      end

  fun appi f (ref (l, a)) =
      let
          fun ap n = 
              if n < l
              then ((case Array.sub(a, n) of
                        NONE => ()
                      | SOME x => f (n, x));
                    ap (n + 1))
              else ()
      in
          ap 0
      end

  (* PERF: These also check the region we know is empty,
     in exchange for using the (fast?) built-ins. *)
  fun all f (ref (l, a)) =
      Array.all (fn NONE => true
                  | SOME x => f x) a
  fun exists f (ref (l, a)) =
      Array.exists (fn NONE => false
                     | SOME x => f x) a

  fun tabulate n f =
      let val a = Array.tabulate (n, SOME o f)
      in  ref (n, a)
      end

  fun copy (ref (n, a)) =
      ref (n, Array.tabulate (n, fn x => Array.sub(a, x)))

  fun has (ref (used, a)) n = 
      if n < 0 then raise Subscript
      else n < used andalso Option.isSome(Array.sub(a, n))

  (* grow to accommodate at least n elements *)
  (* PERF this appears to be one element too conservative *)
  fun accommodate (r as ref(l, a)) n =
    if Array.length a >= (n + 1)
    then ()
    else
      let 
        fun nextpower x = 
          if x >= (n + 1) 
          then x
          else nextpower (x * 2)
        val ns = nextpower (Array.length a)
        val na = Array.tabulate(ns,
                                (fn i =>
                                 if i < l
                                 then Array.sub(a, i)
                                 else NONE))
      in
        r := (l, na)
      end

  fun update r n x =
    if n < 0 then raise Subscript
    else
      let 
        val _ = accommodate r n
        val (l, a) = !r
      in
        Array.update(a, n, SOME x);
        (* also update 'used' *)
        if n >= l
        then r := (n + 1, a)
        else ()
      end

  fun util_for lo hi f =
      if lo > hi then ()
      else (ignore (f lo); util_for (lo + 1) hi f)

  fun insertat (r as ref(n, _)) i x =
    let
      val _ = accommodate r (n + 1)
      val (_, a) = !r

      (* Shift everything over *)
      fun copy j =
          if j = i
          then ()
          else (Array.update (a, j, Array.sub(a, j - 1));
                copy (j - 1))
    in
        copy n;
        Array.update (a, i, SOME x);
        r := (n + 1, a)
    end

  fun append (r as ref(n, _)) x =
    let
      val _ = accommodate r (n + 1)
      val (_, a) = !r
    in
      Array.update(a, n, SOME x);
      r := (n + 1, a)
    end

  fun truncate (r as ref(n, a)) x =
    if x > n
    then raise Subscript
    else r := (x, a)

  fun finalize (ga as (ref (n, a))) =
      let 
          val ret =
            Array.tabulate (n, (fn x => case Array.sub(a, x) of
                                           NONE => raise Subscript
                                         | SOME z => z))
      in
          clear ga;
          ret
      end

  fun vector (ref (n, a)) =
    Vector.tabulate (n, (fn x => case Array.sub(a, x) of
                                    NONE => raise Subscript
                                  | SOME z => z))

  fun tolist (ref (n, a)) =
    let
      fun r i =
          if i = n
          then nil
          else (case Array.sub (a, i) of
                    NONE => raise Subscript
                  | SOME z => z :: r (i + 1))
    in
        r 0
    end

  fun fromlist l =
      let val a = Array.fromList (map SOME l)
      in ref (Array.length a, a)
      end

  (* XXX should probably (?) update n if we are
     erasing the last one. *)
  fun erase (r as ref (n, a)) i = 
      if i >= n
      then raise Subscript
      else
        let 
            fun shrink x =
                if x < 0
                then clear r
                else (case Array.sub (a, x) of
                          NONE => shrink (x - 1)
                        | SOME _ => r := (x + 1, a))
        in
            Array.update(a, i, NONE);
            (* If this is the last element, shrink
               notional size of the array. *)
            if i = n - 1
            then shrink (i - 1)
            else ()
        end

  (* PERF, could keep low water mark as well. *)      
  fun update_next (ga as (ref (n, a))) x =
      let
          fun findy i =
              if i = n
              then (append ga x; i)
              else
                  (case Array.sub(a, i) of
                       NONE => (Array.update (a, i, SOME x); i)
                     | SOME _ => findy (i + 1))
      in
          findy 0
      end

end