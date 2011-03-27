
functor MarkovFn(A : NMARKOVARG) :> NMARKOV where type symbol = A.symbol =
struct

  exception NMarkov of string

  open A

  (* [a,b,c] stored as a*radix^2 + b*radix^1 + c*radix^0, etc. *)
  (* Since we'll allocate an array of this size, we don't worry about using large ints or
     intinfs or anything like that. *)
  type state = int

  (* Compute the largest possible state, to make sure we can't overflow. *)
  fun pow n 0 = 1
    | pow n k = pow n (k - 1) * n
  val _ = pow radix n
      handle Overflow => raise NMarkov "radix^n is too large to be represented as an integer."

  val cmp_state = Int.compare

  (* To advance the state, we mod by this. *)
  val modulus = pow radix n

  fun history s =
      let
          fun h 0 i = (i, nil)
            | h m i =
              let val (i, t) = h (m - 1) i
              in (i div radix, fromint (i mod radix) :: t)
              end
          val (_, l) = h n s
      in
          l
      end
  fun state (l : symbol list) =
      let
          fun s i nil = i
            | s i (z :: t) = s (toint z + i * radix) t
      in
          s 0 l
      end

  fun stateonly (symint, x) =
      let
          fun s i 0 = i
            | s i x = s (symint + i * radix) (x - 1)
      in
          s 0 x
      end

  fun advance_state (old : state, sym : symbol) : state =
      (old * radix) mod modulus + (toint sym)

  (* For each (possible) state, we keep an array containing the accumulated
     weight of each possible next symbol, as well as the total weight. *)
  (* PERF: Could make this ragged, on the hypothesis that many states are never reached? 
     But we already have a sparse version in nmarkov-cmp-sig.sml *)
  type chain = { (* Size modulus * radix *)
                 weights : real Array.array,
                 (* Size modulus. *)
                 totals : real Array.array }
  fun chain () = { weights = Array.array (modulus * radix, 0.0),
                   totals = Array.array (modulus, 0.0) }

  fun accum (a, i, v) = Array.update (a, i, Array.sub(a, i) + v)
  fun weight_index (st : state, sym : symbol) = st * radix + toint sym

  fun observe_weighted w { weights, totals } (st : state, sym : symbol) =
    let in
        accum (totals, st, w);
        accum (weights, weight_index (st, sym), w)
    end

  val observe = observe_weighted 1.0

  fun observe_weighted_string { chain, weight, begin_symbol : symbol, end_symbol, string } =
    let
        val start_state = stateonly (toint begin_symbol, n)
        fun eat state (sym :: rest) =
            let 
            in
                observe_weighted weight chain (state, sym);
                eat (advance_state (state, sym)) rest
            end
          | eat state nil = observe_weighted weight chain (state, end_symbol)
    in
        eat start_state string
    end

  fun probability { weights, totals } (state, symbol) =
      let val tot = Array.sub (totals, state)
          val w = Array.sub (weights, weight_index (state, symbol))
      in if tot > 0.0
         then w / tot
         else 0.0
      end

  fun string_probability { chain, begin_symbol, end_symbol, string } : real =
      let
          val start_state = stateonly (toint begin_symbol, n)
          fun prob state p (sym :: rest) =
              let
                  (* Attenuate probability by this transition *)
                  val p = p * probability chain (state, sym)
              in
                  prob (advance_state (state, sym)) p rest
              end
            (* At the end, observe the end symbol *)
            | prob state p nil = p * probability chain (state, end_symbol)
      in
          prob start_state 1.0 string
      end

(*
  fun probabilities chain state =
    case STM.find (!chain, state) of
        NONE => nil
      | SOME { total, next } =>
          SYM.foldri (fn (sym, w, l) => (sym, w / !total) :: l) nil (!next)
*)
  structure S = Stream

  (* It's beautiful!

     At every step we're looking at a series of transitions we might
     make. Each one can either immediately produce an output (because
     it's for the end_symbol) or it can result in a recursive call,
     which also produces output. The end_symbol one can be modeled as
     a stream consisting of a single element. So, look at the head of
     each stream. We need to be careful not to loop forever in doing
     so. So what we'll really do is binary search for answers with
     value greater than some p (strictly less than 1). This can only
     loop in situations where there is a cycle where all transitions
     have probability 1, because all other cycles eventually fall
     below any finite probability.

     XXX decided to make lower bound an argument!

     *)
  fun most_probable_paths { lower_bound : real, 
                            chain = chain as { totals, weights } : chain, 
                            state : state, end_symbol : symbol }
         : { string : symbol list, p : real } Stream.stream =
    let
      val esi = toint end_symbol 
      val total = Array.sub (totals, state)
      fun nexts i =
        if i = radix
        then nil
        else 
          let val sym = fromint i
              (* XXX this can be strength reduced. Don't know if compiler does it. *)
              val r = Array.sub(weights, weight_index (state, sym))
              val p = r / total
          in
              (* No matter what, it must meet the lower bound. *)
              if p < lower_bound
              then nexts (i + 1)
              else if i = esi
                   then (* If it's the end symbol, then there is just one result
                           which is the empty path. *)
                       S.singleton { string = nil, p = p } :: nexts (i + 1)
                   else 
                     (* Otherwise, we have to recurse. Our
                        current lower bound lb will no longer
                        suffice, because whatever the probability
                        of the recursive call, we will multiply
                        it by p and it must not fall below our
                        current lower bound.

                        lb < p * lb' *)
                     let
                         val lb' = lower_bound / p
                         val tails =
                             most_probable_paths { lower_bound = lb', chain = chain,
                                                   state = advance_state (state, sym),
                                                   end_symbol = end_symbol }
                     in
                         (* Now multiply through the probabilities and add the symbol
                            to the head of the strings. *)
                         Stream.map (fn { string = t, p = p' } =>
                                     { string = sym :: t, p = p * p' }) tails ::
                         nexts (i + 1)
                     end
          end

          (* PERF could be less eager *)
          val streams = nexts 0
          (* Comparison to sort by descending probability *)
          fun bysecond_real_descending ({p, string = _}, 
                                        {p = pp, string = _}) = Real.compare (pp, p)
      in
          S.merge_sorted bysecond_real_descending streams
      end

end
