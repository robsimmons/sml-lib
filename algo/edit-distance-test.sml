structure Test =
struct
  exception EditDistance of string
open EditDistanceString
fun playback s nil = s
  | playback s (Modify (i, c) :: r) = playback (if i >= size s then raise EditDistance "OUT"
                                                else ();
                                                CharVector.tabulate (size s, 
                                                                     fn x =>
                                                                     if x = i
                                                                     then c
                                                                     else String.sub(s, x))) r
  | playback s (Delete i :: r) = playback (if i >= size s then raise EditDistance "D_OUT"
                                           else ();
                                           CharVector.tabulate (size s - 1,
                                                           fn x =>
                                                           if x < i
                                                           then String.sub(s, x)
                                                           else String.sub(s, x + 1))) r
  | playback s (Insert (i, c) :: r) = playback (if i > size s then raise EditDistance "D_INS"
                                                else ();
                                                CharVector.tabulate
                                                (size s + 1,
                                                 fn x =>
                                                 if x < i
                                                 then String.sub(s, x)
                                                 else if x = i
                                                      then c
                                                      else String.sub(s, x - 1))) r

fun printplan nil = print "END.\n"
  | printplan (Insert (i, c) :: r) = (print ("INSERT " ^ implode[c] ^ " @ " ^ Int.toString i ^ "\n");
                                      printplan r)
  | printplan (Modify (i, c) :: r) = (print ("MODIFY to " ^ implode[c] ^ " @ " ^ Int.toString i ^ "\n");
                                      printplan r)
  | printplan (Delete i :: r) = (print ("DELETE @ " ^ Int.toString i ^ "\n");
                                 printplan r)

fun test (s1, s2) =
    let
        val (d, l) = minedit (s1, s2)
        (* val () = printplan l *)
        val ss = playback s1 l
    in
        (* print ("Distance " ^ Int.toString d ^ ": " ^ ss ^ "\n"); *)
        (if ss <> s2 then raise EditDistance ("XXXXXX OH NO ! should be " ^ s2 ^ "\n") else ());
        (d, l)
    end handle EditDistance s => (print ("Error: " ^ s); raise EditDistance s)

val key = DES.key (0wxEFFE, 0w1)
val seed = ref (0wxF : Word32.word, 0w0 : Word32.word)
fun mrand () =
    (seed := DES.encrypt key (!seed);
     seed := (#2 (!seed) * 0wxABCD, #1 (!seed) + 0w1234567);
     #1 (!seed))

fun randomtest 0 = ()
  | randomtest n = 
    let 
        fun randomstring () =
            let val r = mrand ()
            in
                (* print (Word32.toString r ^ "\n"); *)
                if 0w0 = Word32.andb(r, 0wx3F)
                then ""
                else (* if 0w0 = Word32.andb (r, 0w1) then "X" else "O" *) 
                    Word32.toString r ^ randomstring ()
            end
        val s1 = randomstring ()
        val s2 = randomstring ()
            
        val () = if n mod 1 = 0 then TextIO.output (TextIO.stdErr, "[" ^ Int.toString n ^ "]    Test: " ^ s1 ^ " -> " ^ s2 ^ "\n") else ()
        val (d, _) = test (s1, s2)
    in
        randomtest (n - 1)
    end

  datatype class = C_WS | C_OTHER
  fun class c = if StringUtil.whitespec c then C_WS else C_OTHER

  fun chunkstr s =
    let

      fun chunk start n =
        if n = size s then
           (if start = n then nil
            else [String.substring(s, start, n - start)])
        else 
          if start <> n andalso class (String.sub(s, n - 1)) <>
                                       class (String.sub(s, n))
          then (* new chunk *)
            String.substring(s, start, n - start) :: chunk n n
          else chunk start ( n + 1 )
             
    in
      Vector.fromList (chunk 0 0)
    end

  structure SED = EditDistanceFn(type ch = string
                                 type str = string vector
                                 val eq = op =
                                 val sub = Vector.sub
                                 val len = Vector.length
                                 fun MODIFY_COST (_, ch) = 1 + size ch
                                 fun INSERT_COST ch = 1 + size ch
                                 fun DELETE_COST _ = 1)
  datatype ? = datatype SED.edit

  (* a plan tells us how to get from one unparsed string to the next *)
  type plan = SED.edit list
  (* XXX PERF *)
  fun vplay s nil = s
    | vplay s (Modify (i, c) :: r) = vplay (Vector.tabulate (Vector.length s, 
                                                                   fn x =>
                                                                   if x = i
                                                                   then c
                                                                   else Vector.sub(s, x))) r
    | vplay s (Delete i :: r) = vplay (Vector.tabulate (Vector.length s - 1,
                                                              fn x =>
                                                              if x < i
                                                              then Vector.sub(s, x)
                                                              else Vector.sub(s, x + 1))) r
    | vplay s (Insert (i, c) :: r) = vplay (Vector.tabulate
                                                  (Vector.length s + 1,
                                                   fn x =>
                                                   if x < i
                                                   then Vector.sub(s, x)
                                                   else if x = i
                                                        then c
                                                        else Vector.sub(s, x - 1))) r

  fun vtest (v1, v2) =
    let
      val v1 = chunkstr v1
      val v2 = chunkstr v2
      val (d, plan) = SED.minedit (v1, v2)
      fun pp (Modify (i, c)) = print ("  Modify(" ^ Int.toString i ^ ") -> '" ^ c ^ "'\n")
        | pp (Delete i) = print ("  Delete(" ^ Int.toString i ^ ")\n")
        | pp (Insert (i, c)) = print ("  Insert(" ^ Int.toString i ^ ") @ '" ^ c ^ "'\n")
    in
      print ("Distance: " ^ Int.toString d ^ "\n");
      app pp plan;
      if vplay v1 plan <> v2
      then print ("WRONG RESULT: " ^ String.concat (Vector.foldr op:: nil (vplay v1 plan)) ^ "\n")
      else ()
    end

end

(*val () = Test.randomtest 5000 *)
