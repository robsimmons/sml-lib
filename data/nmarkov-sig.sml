(* Represents imperative sparse n-Markov chains. *)
signature NMARKOV =
sig

  exception NMarkov of string

  (* Symbols make up observations and are the basis of
     states in the chain. Functor argument. *)
  type symbol
  val cmp_symbol : symbol * symbol -> order
  (* Number of symbols of history that are used to index the
     probability table. Classic Markov chain is n=1. n=0 is
     just a frequency table. *)
  val n : int

  (* Type of n-Markov chain. Imperative. *)
  type chain
  (* A state is an n-tuple of observed symbols. Immutable. *)
  type state
  val chain : unit -> chain
  val cmp_state : state * state -> order
  val history : state -> symbol list

  (* Construct a state. The list of symbols must be exactly n in length. *)
  val state : symbol list -> state
  (* Removes the oldest symbol in the state, adds the new one. *)
  val advance_state : state * symbol -> state

  (* Make an observation of a transition: "In this state, we next saw
     the symbol." Modifies the n-Markov chain to account for this
     observation, assuming unity weight. *)
  val observe : chain -> state * symbol -> unit
  (* Same, but with arbitrary positive weight. *)
  val observe_weighted : real -> chain -> state * symbol -> unit

  (* For finite sequences, it is useful to define begin and end symbols,
     with the convention that the begin symbol only appears in
     prefixes of real symbols in states, and that no symbols are
     observed after the end symbol. This function takes a string,
     observes its first symbol after a state of N 'begin' symbols,
     then its second symbol after a state of N-1 'begin' symbols and
     the first symbol of the string, etc. It observes the end symbol
     once, with the length-n tail of the string (or padded with
     'begin' symbols if shorter than n) as the preceding state. The
     same weight is used for each observation. *)
  val observe_weighted_string : { chain : chain,
                                  weight : real,
                                  begin_symbol : symbol,
                                  end_symbol : symbol,
                                  string : symbol list } -> unit

  (* Get the probability of seeing the given symbol after the
     observation. *)
  val probability : chain -> state * symbol -> real
  (* Get all the non-zero symbols and their probabilities for
     the state. *)
  val probabilities : chain -> state -> (symbol * real) list

  (* Produces a stream of the most probable observations ending with a
     certain symbol. The expected way for this to be used is for the
     symbol to be the end symbol sentinel, and the start state to be n
     copies of begin_symbol, following the convention of
     observe_weighted_string. In that case the result is a stream
     (infinite if there are any loops, which is typical) of words,
     ordered from most to least probable, along with their
     probabilities in (0, 1]. The end symbol is not included in the
     list, since it is usually a sentinel and would be included in
     any path anyway.

     If the n-Markov chain contains a cycle where every transition is
     probability 1, then the stream may loop forever without returning
     an answer. This will never be the case if the n-Markov chain was
     constructed only by calls to observe_weighted_string.

     If the chain is modified, then the stream XXX.
     *)
  val most_probable_paths : { lower_bound : real, (* XXX *)
                              chain : chain,
                              state : state,
                              end_symbol : symbol } ->
      { string : symbol list, p : real } Stream.stream

end

signature NMARKOVARG =
sig

  (* As above. *)
  type symbol
  val cmp : symbol * symbol -> order
  val n : int

end
