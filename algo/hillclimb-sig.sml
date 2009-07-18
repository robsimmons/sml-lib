(* structure Climbing :> *)
signature CLIMBING =
sig
    
    (* Conditions under which we stop optimizing. *)
    datatype 'a condition =
        (* Ran this many iterations *)
        Iters of int
        (* Successive iterations changed by no more than this amount
           FIXME: This doesn't work and is ignored.
           *)
      | ConvergedWithin of Real64.real
        (* Spent this many *milliseconds* *)
      | TimeSpent of int
        (* Stops as soon as the function returns true on the output of evaluate. *)
      | Condition of 'a -> bool

end

signature CLIMBARG =
sig

    type param 
    type score = real
    val ++ : param * param -> param
    val -- : param * param -> param
    val ** : score * param -> param
    (* Conservative equals. If it returns true, the arguments must
       have the same objective function. *)
    val == : param * param -> bool
    val neg : param -> param
    val shrink : param -> param
    val grow : param -> param
    val goodenough : score -> bool
    val debug : param * score -> unit 

end

(* functor Climb(CLIMBARG) -> CLIMB *)
signature CLIMB =
sig
    type 'a condition (* always Climbing.condition *)
    type param (* functor arg *)
    val climb : { init : param,
                  zero : param,
                  dinit : param list,
                  objective : param -> real * 'a,
                  stop : 'a condition list } -> param * real * 'a

    (* XXX simple interface too *)
end
