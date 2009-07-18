(* Contributed by Jason Reed, modified by Tom 7 *)

structure Climbing :> CLIMBING =
struct
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

functor Climb(M : CLIMBARG) :> CLIMB where type 'a condition = 'a Climbing.condition
                                       and type param = M.param =
struct

  open M
  infix 3 ++ -- ==

  open Climbing

  fun climb { init, objective, dinit, zero, stop } =
      let
	  val starttime = Time.now ()
	  fun why s true =
	      let in
		  (* print ("Stop because " ^ s ^ "\n"); *)
		  true
	      end
	    | why s false = false
	  fun shouldstop { n, a, newp, p, delta } =
	      let
		  fun ss nil = false
		    | ss (Iters m :: t) = why "iters" (n >= m) orelse ss t
		    | ss (ConvergedWithin r :: t) = ss t (* why "delta" (delta <= r) orelse ss t *)
		    | ss (TimeSpent ms :: t) =
		      why "time"
		      (Time.> (Time.- (Time.now (), starttime), 
			       Time.fromMilliseconds (IntInf.fromInt ms)))
		      orelse ss t
		    | ss (Condition f :: t) = why "cond" (f a) orelse ss t
	      in
		  (* why "0 delta" (Real.== (delta, 0.0)) orelse *) ss stop
	      end

	  (*
	  val objective = 
	      (fn arg => 
	       let
		   val () = print "obj:\n"
		   val r = objective arg
	       in
		   print "\n:jbo\n";
		   r
	       end)
	      *)
	  fun go n p dps =
	      let
		  (* Apply all increments and return the new value of the
		     objective function, and the new modified increments. *)
		  fun incrs (dp :: dps) p (ob, a) newdps_rev =
		      let
			  val pplus = p ++ dp
			  val pminus = p -- dp
			  val (obplus, aplus) = 
			      if pplus == p
			      then (ob, a)
			      else objective pplus
			  val (obminus, aminus) = 
			      if pminus == p
			      then (ob, a)
			      else objective pminus
			  val { newdp, new, newob, newa } =
			      case (Real.compare (obplus, ob), Real.compare (obminus, ob)) of
				  (GREATER, _) => { newdp = grow dp, new = pplus, newob = obplus, newa = aplus }
				| (_, GREATER) => { newdp = grow dp, new = pminus, newob = obminus, newa = aminus }
				| (_, _) => { newdp = shrink dp, new = p, newob = ob, newa = a }
		      in
			  (* print "incrs..\n"; *)
			  incrs dps new (newob, newa) (newdp :: newdps_rev)
		      end
		    | incrs nil newp (ob, a) newdps_rev = (newp, ob, a, rev newdps_rev)

		  val (start_ob, start_a) = objective p
		  val (newp, ob, a, newdps) = incrs dps p (start_ob, start_a) []
		  (* XXX this isn't really right. even if there's no change in 
		     score we might be growing or shrinking *)
		  val delta = Real.abs (start_ob - ob)
	      in
		  (* print ("After " ^ Int.toString n ^ " iters score " ^
			 Real.toString ob ^ "\n"); *)
		  if shouldstop { n = n, a = a, newp = newp, p = p, delta = delta }
		  then
		      let in
			  (* print ("Stopped after " ^ Int.toString n ^ " iters\n"); *)
			  (newp, ob, a)
		      end
		  else go (n + 1) newp newdps
	      end

	  (* val () = print "go:\n"; *)
	  val res = go 0 init dinit
      in
	  (* print "\ngone.\n"; *)
	  res
      end     
end
