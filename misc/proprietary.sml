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

  (* Set of integers. *)
  type interval = int list
  val interval_0_255 = List.tabulate (256, fn x => x)

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

        fun random_bit_log 0 = 1
          | random_bit_log n =
            if MT.random_bool mt
            then random_bit_log (n - 1) * 2
            else random_bit_log (n - 1)

        fun shuffle_list l =
            let
                val a = Array.fromList l
            in
                MT.shuffle mt a;
                (Array.foldr op:: nil a)
            end

        fun permutation_tuple l =
            "(" ^ StringUtil.delimit ", "
            (shuffle_list l) ^ ")"

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

        fun hasbit (a, bit : int) =
            case format of
                SML => "(Word32.andb(Word32.fromInt (" ^ a ^ ")," ^
                       "Word32.<<(0w1, 0w" ^ Int.toString bit ^ ")) <> 0w0)"
              | APH => "(((" ^ a ^ ") andb (0x" ^ 
                       Word32.toString (Word32.<<(0w1, Word.fromInt bit)) ^
                       ")) > 0)" (* XXX? *)

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

        fun indent z = CharVector.tabulate(z, fn _ => #" ")

        fun nontrivial_predicate [a, b] exp =
            ("((" ^ exp ^ ") = " ^ Int.toString a ^ ")", [a], [b])
          | nontrivial_predicate [_] _ = 
            raise Proprietary "no nontrivial predicates on a singleton"
          | nontrivial_predicate nil _ =
            raise Proprietary "no nontrivial predicates on nil!"
          | nontrivial_predicate l exp =
            case MT.random_nat mt 3 of
                (* XXX more of these *)
                0 =>
                    let val i = ListUtil.median Int.compare l
                        val (t, f) = ListUtil.sift (fn x => x <= i) l
                    in
                        ("((" ^ exp ^ ") <= " ^ Int.toString i ^ ")",
                         t, f)
                    end
              | _ =>
                    let 
                        (* The elements in l are not all the same,
                           so they must differ in at least one bit. *)
                        val words = (map Word32.fromInt l)
                        val w_and = foldl Word32.andb 0wxFFFFFFFF words
                        val w_or = foldl Word32.orb 0w0 words
                        (* Choose any bit that's off in w_and
                           (at least one has a zero) and on in w_or
                           (at least one has a one) *)
                        val w_ok = Word32.andb(Word32.notb w_and, w_or)
                        fun getok 0 l = l
                          | getok n l =
                            if Word32.andb(Word32.<< (0w1, Word.fromInt n),
                                           w_ok) <> 0w0
                            then getok (n - 1) (n :: l)
                            else getok (n - 1) l
                                
                        val okay = getok 31 nil
                        val okay = shuffle_list okay
                        val bit = case okay of
                            h :: _ => h
                          | nil => raise Proprietary "impossible! okbit"

                        fun bitset i = 
                            Word32.andb(Word32.fromInt i,
                                        Word32.<<(0w1,
                                                  Word.fromInt bit)) <> 0w0
                        val (t, f) = ListUtil.sift bitset l
                    in
                        (hasbit (exp, bit), t, f)
                    end

        fun split_math z exp src dst =
            (* Additive works better if they're sorted. *)
            case (ListUtil.sort Int.compare src, ListUtil.sort Int.compare dst) of
                (s :: src, d :: dst) =>
                    let
                        (* try to find a * x + b mod c that explains src -> dst. *)
                        val maxs = foldr Int.max s src
                        val maxd = foldr Int.max d dst
                            (* XXX do it *)
                    in
                        NONE (* XXX *)
                    end
              | _ => NONE

        (* subst_to indentation exp src dst
           Generate an int-valued expression, which has
           value somewhere in dst, assuming exp has value
           somewhere in src. *)
        fun subst_to z _ _ nil = raise Proprietary "bug: subst_to 0"
          | subst_to z _ [s] [d] = Int.toString d
          | subst_to z _ _ [_] = raise Proprietary "bug: subst_to 1"
          | subst_to z exp src dst =
            (* math doesn't always work, but we can always use a predicate
               to split. *)
            case split_math z exp src dst of
                SOME e => e
              | NONE => 
                    let
                        val (ps, srct, srcf) = nontrivial_predicate src exp
                        val dst = shuffle_list dst
                        val (dstt, dstf) = ListUtil.cleave (length srct) dst
                    in
                        "(if " ^ ps ^ "\n" ^ indent z ^
                        " then " ^ subst_to (z + 8) exp srct dstt ^ "\n" ^ indent z ^
                        " else " ^ subst_to (z + 8) exp srcf dstf ^ ")"
                    end

        val subst_to = subst_to 2

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

        fun char_ord c =
            case format of
                SML => "(ord (" ^ c ^ "))"
              | APH => "(ord (" ^ c ^ "))" (* XXX ?? *)

        fun char_chr c =
            case format of
                SML => "(chr (" ^ c ^ "))"
              | APH => "(chr (" ^ c ^ "))" (* XXX ?? *)

        fun tointinrange (w, r) =
            case format of
                SML => "(Word32.toInt (Word32.mod (" ^ w ^ 
                    ", " ^ toword r ^ ")))"
              (* XXX signedness? *)
              | APH => "((" ^ w ^ ") mod (" ^ r ^ "))"

        fun list_map f l =
            case format of
                SML => "(map (" ^ f ^ ") (" ^ l ^"))"
              | APH => "(map (" ^ l ^ ", " ^ f ^"))" (* XXX ? *)

        fun hard_round () =
            case MT.random_nat mt 4 of
                0 =>
                    print ("val x = ref (" ^ randwords () ^ ")\n" ^
                           "fun go n = if n >= size s then !x " ^
                           "  else (x := " ^ mix2 (sub ("s", "n"), 
                                                   "!x") ^ "; " ^
                           "  go (n + 1))\n" ^
                           "val d = " ^ mix2 ("go 0", "d") ^ "\n")
              | 1 =>
                    print ("val ss = size s\n" ^
                           "val s = explode s\n" ^
                           "fun shuffle (l, r, nil, _) = l @ r\n" ^
                           "  | shuffle (l, r, t, 0) =\n" ^
                           "      shuffle (r, l, t, " ^
                           Int.toString (1 + MT.random_nat mt 8) ^ ")\n" ^
                           "  | shuffle (l, r, h :: t, n) =\n" ^
                           "      if 0 = ((" ^ 
                           Int.toString (MT.random_nat mt 12) ^ " + " ^
                           char_ord "h" ^ ") mod " ^
                           Int.toString (2 + MT.random_nat mt 3) ^ ")\n" ^
                           "      then shuffle (l, h :: r, t, n - 1)\n" ^
                           "      else shuffle (h :: l, r, t, n - 1)\n" ^
                           "val s = implode (shuffle (nil, nil, s, " ^
                           "ss div 3 + " ^ 
                           Int.toString (MT.random_nat mt 5) ^ "))\n")

              | 2 =>
                    print ("val s = explode s\n" ^
                           "fun subst c =\n  " ^
                           "  let val n = " ^ char_ord "c" ^ "\n" ^
                           "      val n = " ^
                           subst_to "n" interval_0_255 interval_0_255 ^
                           "  in " ^ char_chr "n" ^ "\n" ^
                           "  end\n" ^
                           "val s = " ^ list_map "subst" "s" ^ "\n" ^
                           "val s = implode s\n")
                                 
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
