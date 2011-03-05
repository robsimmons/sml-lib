
structure NMarkovTest =
struct
  exception TestFail of string

  structure M2C : NMARKOVARG =
  struct
    type symbol = char
    val toint = ord
    val fromint = chr
    val n = 2
    val radix = 256
  end

  structure M = MarkovFn(M2C)

  val () =
  let
    val rtos = Real.fmt (StringCvt.FIX (SOME 2))
    val rtos9 = Real.fmt (StringCvt.FIX (SOME 9))

    val chain = M.chain ()
    fun ows s = 
        M.observe_weighted_string { begin_symbol = chr 1,
                                    end_symbol = chr 0,
                                    weight = 1.0,
                                    chain = chain,
                                    string = explode s }

    fun state s = M.state (explode s)

    val h = M.history (state "hi")
    val _ = h = [#"h", #"i"]
        orelse raise TestFail ("history o state not the identity: " ^ implode h)

    val iz = M.history (M.advance_state (state "hi", #"z"))
    val _ = iz = [#"i", #"z"]
        orelse raise TestFail ("advance_state doesn't work: " ^ implode iz)

    val () = ows "hello"
    val () = ows "helpful"

    val _ = rtos (M.probability chain (state "\001\001", #"h")) = "1.00"
        orelse raise TestFail "all words start with h"

    val _ = 
        let val p = rtos (M.probability chain (state "\001h", #"e"))
        in
            p = "1.00" orelse 
            raise TestFail ("after h at the beginning of a word is always e: " ^ p)
        end

    val _ = rtos (M.probability chain (state "he", #"l")) = "1.00"
        orelse raise TestFail "after he is l"

    val _ = rtos (M.probability chain (state "el", #"l")) = "0.50"
        orelse raise TestFail ""
    val _ = rtos (M.probability chain (state "el", #"p")) = "0.50"
        orelse raise TestFail ""

    val () = ows "felafel"

    val _ = rtos (M.probability chain (state "el", #"\000")) = "0.25"
        orelse raise TestFail "might end at felafel, or keep going..."

    val beginstate = state "\001\001"
    val tops = StreamUtil.headn 20 (M.most_probable_paths { lower_bound = 0.000001,
                                                            chain = chain,
                                                            state = beginstate,
                                                            end_symbol = #"\000" })
    val () = print (Int.toString (length tops) ^ " top paths:\n");
    val () = app (fn { string, p } =>
                  print (rtos9 p ^ " " ^ implode string ^ "\n")) tops
    val () = print "(check manually)\n"
  in ()
  end handle (e as (TestFail s)) => (print ("Failed: " ^ s ^ "\n"); raise e)

  val () = print "All N-Markov tests passed.\n"

end
