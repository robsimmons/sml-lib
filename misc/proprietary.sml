(* This is a cryptographically-unsound proprietary hash algorithm.
   It's used to create verifiable signatures by "blessed" official
   binary releases of applications, so that client-generated data
   can have some moderate level of trustworthiness (e.g. high scores).

   This problem doesn't have a satisfying solution, especially for
   an open-source program. If we just use a known cryptographically-
   strong signature algorithm (say, El Gamal) then an adversary can
   inspect the code to learn how it works, and create his own signing
   function. At worst the key is visible in the code and he uses his
   crypto library's implementation of El Gamal to sign whatever
   message he wants. We can avoid having the key visible in the code,
   only providing it at compilation time, but then the key is probably
   neatly visible as a symbol in the compiled binary, and easily
   extracted.

   At some level the signing function must be manifest in the compiled
   code, which means that the adversary will be able to read that code,
   and thus conceivably reproduce the function. The best strategy is
   therefore to make that task as difficult as possible, by making the 
   compiled code incomprehensible.

   This meta-program generates a hash function based on an input
   string; the function is SML (and Aphasia 2) source code. The
   implementation of the function is pointlessly roundabout, dependent
   on the input string, and prone to optimization by the compiler.
   It should be difficult to reverse engineer by looking at the
   compiled code. It is not a design goal that it be difficult to
   discover the secret key from the uncompiled source, but that may
   well be true, too.

   If you're reading this it might be because you're interested in
   tampering, like to fake a high score. Be my guest, I guess, since
   if people like you don't exist then I'm wasting my time right now.
   If you succeed and I notice, then I will just undo your fake data,
   make a few minor changes to this code, and force a new release. I
   think this is less work for me than you.

   The remainder of this code is deliberately unclear and light on
   documentation.
*)

structure Proprietary =
struct

  exception Proprietary of string
  
  structure MT = MersenneTwister

  val keyfile = Params.param ""
      (SOME ("-keyfile", 
             "Specifies the keyfile, which customizes the " ^
             "proprietary hash. If not found, simply prints " ^
             "a warning and uses dummy data.")) "keyfile"

  val format = Params.param "sml"
      (SOME ("-format",
             "Specifies the output format, which can currently " ^
             "only be sml (Standard ML) or aph (Aphasia 2).")) "format"

  val symbol = Params.param "proprietary"
      (SOME ("-symbol",
             "The name of the function to generate, which must " ^
             "be a valid symbol in the target format.")) "symbol"

  fun load_keyfile kf =
    StringUtil.readfile kf handle _ =>
        (TextIO.output (TextIO.stdErr,
                        "\nCouldn't read '" ^
                        kf ^ "'; using dummy data.\n\n");
         "")

  datatype format = SML | APH

  fun generate () =
    let
        val format =
            case !format of
                "sml" => SML
              | "aph" => APH
              | _ => raise Proprietary "unknown format"

        val w32type =
            case format of
                SML => "Word32.word"
              | APH => "int"

        val mt = MT.initstring (load_keyfile (!keyfile))
        fun randword () = MT.random_nat mt 0x7FFFFFFF
        val itos = Int.toString
        fun randwords () =
            case format of
                SML => "0w" ^ itos (randword ())
              | APH => itos (randword ())

        fun permutation_tuple l =
            let
                val a = Array.fromList l
            in
                MT.shuffle mt a;
                "(" ^ StringUtil.delimit ", "
                (Array.foldr op:: nil a) ^ ")"
            end

        fun tuple_abcd () = permutation_tuple ["a", "b", "c", "d"]

        fun xor (a, b) =
            case format of
                SML => "(Word32.xorb((" ^ a ^ "), (" ^ b ^ ")))"
              | APH => "((" ^ a ^ ") xorb (" ^ b ^ "))" (* XXX ?? *)

        fun or (a, b) =
            case format of
                SML => "(Word32.orb((" ^ a ^ "), (" ^ b ^ ")))"
              | APH => "((" ^ a ^ ") orb (" ^ b ^ "))" (* XXX ?? *)

        fun andb (a, b) =
            case format of
                SML => "(Word32.andb((" ^ a ^ "), (" ^ b ^ ")))"
              | APH => "((" ^ a ^ ") andb (" ^ b ^ "))" (* XXX ?? *)

        fun notb a =
            case format of
                SML => "(Word32.notb (" ^ a ^ "))"
              | APH => "(notb (" ^ a ^ "))"

        fun plus (a, b) =
            case format of
                SML => "(Word32.+((" ^ a ^ "), (" ^ b ^ ")))"
              | APH => "((" ^ a ^ ") + (" ^ b ^ "))"

        fun times (a, b) =
            case format of
                SML => "(Word32.*((" ^ a ^ "), (" ^ b ^ ")))"
              | APH => "((" ^ a ^ ") * (" ^ b ^ "))"

        fun rol (e, n) =
            "let val w = " ^ e ^ " in " ^
            (case format of
                 SML => 
                     "Word32.orb(Word32.<<(w, 0w" ^ itos n ^ "), " ^
                     "Word32.>>(w, 0w32 - 0w" ^ itos n ^ "))"
               | APH => 
                     "((w << " ^ itos n ^ ") orb (w >> " ^ itos (32 - n) ^ "))"
                     ) ^ " end"

        fun mix1 e =
            case MT.random_nat mt 8 of
                0 => xor (e, randwords())
              | 1 => xor (randwords(), e)
              | 2 => plus (e, randwords())
              | 3 => plus (randwords(), e)
              | 4 => times (e, randwords())
              | 5 => times (randwords(), e)
              | _ => rol (e, MT.random_nat mt 30)

        fun mix2 (e1, e2) =
            case MT.random_nat mt 2 of
                0 => plus (e1, e2)
              | _ => plus (times (randwords(), e1), e2)

        fun mix3 (e1, e2, e3) =
            case MT.random_nat mt 2 of
                0 => plus (times (e1, e2), e3)
              (* XXX duplicates expression *)
              | _ => or (andb (e1, e2), andb (notb e1, e3))

        fun easy_round () =
            case MT.random_nat mt 8 of
                0 =>
                    print ("val " ^ 
                           tuple_abcd () ^ " = " ^
                           tuple_abcd () ^ "\n")
              | 1 =>
                    print ("val " ^ tuple_abcd () ^ " = (" ^
                           StringUtil.delimit ", "
                           [mix1 "a", mix1 "b", mix1 "d", mix1 "c"] ^
                           ")\n")
              | 2 => print ("val a = " ^ mix2 ("a", "c") ^ "\n")
              | 3 => print ("val b = " ^ mix2 ("d", "b") ^ "\n")
              | 4 => print ("val c = " ^ mix3 ("a", "b", "c") ^ "\n")
              | 5 => print ("val d = " ^ mix3 ("a", "b", "d") ^ "\n")
(*
              | 6 => print ("val a = if (b div 4) mod 1 = 0 then " ^
                            mix2 ("a", "c") ^ " else " ^ mix2 ("d", "a")
                            ^ "\n")
*)
              | _ => ()

        fun sub (str, idx) =
            case format of
                SML => "(Word32.fromInt (ord " ^
                    "(CharVector.sub((" ^ str ^ "), (" ^ idx ^ ")))))"
                    (* XXX ?? *)
              | APH => "(ord (string-sub((" ^ str ^ ", (" ^ idx ^ ")))))"

        fun toword w =
            case format of
                SML => "(Word32.fromInt (" ^ w ^ "))"
              | APH => w

        fun tointinrange (w, r) =
            case format of
                SML => "(Word32.toInt (Word32.mod (" ^ w ^ 
                    ", " ^ toword r ^ ")))"
              (* XXX signedness? *)
              | APH => "((" ^ w ^ ") mod (" ^ r ^ "))"

        fun hard_round () =
            case MT.random_nat mt 2 of
                0 =>
                    print ("val x = ref (" ^ randwords () ^ ")\n" ^
                           "fun go n = if n >= size s then !x " ^
                           "  else (x := " ^ mix2 (sub ("s", "n"), 
                                                   "!x") ^ "; " ^
                           "  go (n + 1))\n" ^
                           "val d = " ^ mix2 ("go 0", "d") ^ "\n")
              | _ =>
                    print ("val y = ref (" ^ randwords () ^ ")\n" ^
                           "fun go n = if n >= size s then !y " ^
                           "  else (y := " ^ 
                           mix2 ("!y", sub ("s",
                                            tointinrange
                                            (times (toword "n", randwords ()),
                                             "size s"))) ^ "; " ^
                           "  go (n + 1))\n" ^
                           "val c = " ^ mix2 ("go 0", "c") ^ "\n")

    in

        (* Print to stdout a function (named after the argument
           symbol) of type string -> word32 * word32 * word32 * word32 *)
        print ("(* Generated function. Do not edit. *)\n");
        print ("fun " ^ !symbol ^ " (s : string) : " ^
               w32type ^ " * " ^
               w32type ^ " * " ^
               w32type ^ " * " ^
               w32type ^ " =\n" ^
               "let\n" ^
               "val " ^ tuple_abcd () ^ " = (" ^ 
               randwords () ^ ", " ^ 
               randwords () ^ ", " ^
               randwords () ^ ", " ^
               randwords () ^ ")\n");
        
        Util.for 0 (12 + MT.random_nat mt 8)
        (fn i =>
         let val easy = MT.random_nat mt 4
         in 
             hard_round ();
             if easy > 0
             then Util.for 0 easy (fn _ => easy_round ())
             else ()
         end);
        
        print ("\nin " ^ tuple_abcd () ^ " end\n")
    end      

end

val () = Params.main0 "This program takes no arguments." Proprietary.generate
