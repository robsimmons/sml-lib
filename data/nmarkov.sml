
functor MarkovFn(A : NMARKOVARG) :> NMARKOV where type symbol = A.symbol =
struct

  exception NMarkov of string

  type symbol = A.symbol
  val cmp_symbol = A.cmp
  val n = A.n

  (* PERF: Could maybe find a more efficient representation for this, but
     it will be isomorphic to this. *)
  (* Representation invariant: always exactly n in length. *)
  type state = symbol list

  (* From Util. *)
  fun lex_list_order _  (nil, nil) = EQUAL
    | lex_list_order _  (nil, _ :: _) = LESS
    | lex_list_order _  (_ :: _, nil) = GREATER
    | lex_list_order oi (a :: al, b :: bl) =
    (case oi (a, b) of
       EQUAL => lex_list_order oi (al, bl)
     | neq => neq)
  val cmp_state = lex_list_order cmp_symbol

  (* No fancy implementation today. *)
  fun history s = s
  fun state s = s

  fun advance_state_nonzero (_ :: rest : state, sym : symbol) : state =
      let
          fun asn nil = [sym]
            | asn (h :: t) = h :: asn t
      in
          asn rest
      end
    | advance_state_nonzero (nil, _) = raise NMarkov "impossible"

  (* More efficient than using e.g. List.nth, but
     doesn't work if n is zero. *)
  val advance_state = 
      if n = 0 
      then (fn (s : state, sy : symbol) => nil)
      else advance_state_nonzero

  structure STM = SplayMapFn(type ord_key = state val compare = cmp_state)
  structure SYM = SplayMapFn(type ord_key = symbol val compare = cmp_symbol)

  (* For each state (that we observed), we keep a list of all next symbols,
     their accumulated weight, and the total weight. *)
  type table = { total : real ref,
                 next : real SYM.map ref }

  type chain = table STM.map ref
  fun chain () = ref STM.empty

  infix +=
  fun r += v = r := !r + (v : real)

  fun observe_weighted w chain (st : state, sym : symbol) =
    let
        val { total, next } =
            case STM.find (!chain, st) of
                NONE => let val r = { total = ref 0.0, next = ref SYM.empty }
                        in chain := STM.insert(!chain, st, r);
                           r
                        end
              | SOME r => r

        val mass = (case SYM.find (!next, sym) of
                        NONE => 0.0
                      | SOME d => d)
    in
        total += w;
        next := SYM.insert (!next, sym, w + mass)
    end

  val observe = observe_weighted 1.0

  fun observe_weighted_string { chain, weight, begin_symbol, end_symbol, string } =
    let
        val start_state = List.tabulate (n, fn _ => begin_symbol)
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

  fun probability chain (state, symbol) =
    case STM.find (!chain, state) of
        NONE => 0.0
      | SOME { total, next } =>
            case SYM.find (!next, symbol) of
                SOME r => r / !total
              | NONE => 0.0

  fun probabilities chain state =
    case STM.find (!chain, state) of
        NONE => nil
      | SOME { total, next } =>
          SYM.foldri (fn (sym, w, l) => (sym, w / !total) :: l) nil (!next)

  structure S = Stream
  structure SU = StreamUtil

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
  fun most_probable_paths { lower_bound : real, chain : chain, 
                            state : state, end_symbol : symbol }
         : { string : symbol list, p : real } Stream.stream =
    case STM.find (!chain, state) of
      NONE => S.empty
    | SOME ({ total, next } : table) =>
      let
          fun onesym (sym, r) =
            let val p = r / !total
            in
              (* No matter what, it must meet the lower bound. *)
              if p < lower_bound
              (* PERF could like use List.mapPartial below and just return
                 straight-up NONE here, to get it out of the merge. *)
              then S.empty
              else
              case cmp_symbol (sym, end_symbol) of
                  EQUAL =>
                      (* If it's the end symbol, then there is just one result
                         which is the empty path. *)
                      S.singleton { string = nil, p = p }
                | _ =>
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
                                     { string = sym :: t, p = p * p' }) tails
                     end
            end

          (* PERF could be less eager *)
          val streams = SYM.foldri (fn (sym, r, l) =>
                                    onesym (sym, r) :: l) nil (!next)
          (* Comparison to sort by descending probability *)
          fun bysecond_real_descending ({p, string = _}, 
                                        {p = pp, string = _}) = Real.compare (pp, p)
      in
          S.merge_sorted bysecond_real_descending streams
      end

end
