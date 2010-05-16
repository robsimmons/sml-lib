
functor GrowMonoArrayFn(structure A : MONO_ARRAY
                        (* Unsafe.MonoArray *)
                        structure U :
                          sig
                            val sub : A.array * int -> A.elem
                            val update : A.array * int * A.elem -> unit
                            val create : int -> A.array
                          end) :> GROWMONOARRAY where type elem = A.elem
                                                  and type monoarray = A.array =
struct

  type elem = A.elem
  type monoarray = A.array

  type growarray = (int * monoarray) ref
  val eq = op =

  fun init n = ref (0, U.create n)

  (* start with 16 cells, why not? *)
  fun empty () = init 16

  fun growarray n i = ref (n, A.tabulate (n, fn x => i))

  (* PERF could use unsafe here *)
  fun sub (ref (used, a)) n =
    if n < used andalso n >= 0
    then A.sub(a, n)
    else raise Subscript

  fun length (ref (l, _)) = l

  (* grow to accommodate at least n elements *)
  (* PERF this appears to be one element too conservative *)
  fun accommodate (r as ref(l, a)) n =
    if A.length a >= (n + 1)
    then ()
    else
      let 
        fun nextpower x = 
          if x >= (n + 1) 
          then x
          else nextpower (x * 2)
        val ns = nextpower (A.length a)
        val na = U.create ns

        fun copy i =
          if i < l
          then 
            let in
              A.update(na, i, A.sub(a, i));
              copy (i + 1)
            end
          else ()
      in
        copy 0;
        r := (l, na)
      end

  fun update r n x =
    let 
      val (l, a) = !r
    in
      A.update(a, n, x)
    end

  fun append (r as ref(n, _)) x =
    let
      val _ = accommodate r (n + 1)
      val (_, a) = !r
    in
      A.update(a, n, x);
      r := (n + 1, a)
    end

  fun truncate (r as ref(n, a)) x =
    if x > n
    then raise Subscript
    else r := (x, a)

  fun finalize (ga as (ref (n, a))) =
      let 
          (* val ret = A.tabulate (n, (fn x => A.sub(a, x))) *)
      in
          clear ga;
          a
      end

end

(* when there is no corresponding array in Unsafe *)
functor GrowMonoArrayFn_Safe(structure A : MONO_ARRAY
                             val default : A.elem) 
                             :> GROWMONOARRAY where type elem = A.elem
                                                and type monoarray = A.array =
struct 
  structure Z = GrowMonoArrayFn(structure A = A
                                structure U =
                                struct
                                  val sub = A.sub
                                  val update = A.update
                                  fun create x = A.array(x, default)
                                end)
  open Z
end

structure GCharArray = GrowMonoArrayFn(structure A = CharArray
                                       structure U = Unsafe.CharArray)

(* XXX etc. *)

