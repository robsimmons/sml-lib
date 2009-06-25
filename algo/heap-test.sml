
(* Test the heap structure *)

structure H = HeapFn(type priority = int val compare = Int.compare)

fun ph h =
  case H.min h of
    NONE => "\n"
  | SOME (p, s) => Int.toString p ^ ":" ^ s ^ " " ^ ph h

      (* from listutil *)
    fun alladjacent f nil = true
      | alladjacent f [_] = true
      | alladjacent f (a::(l as (b::_))) = f (a, b) andalso alladjacent f l

    (* su *)
  fun delimit s nil = ""
    | delimit s (h::t) =
        foldl (fn (a, b) => b ^ s ^ a) h t


local
  val ctr = ref 0
  val seed = ref 137
in
  fun reset n = (ctr := n; seed := 137)

  (* val modu = 31337 *)
  val modu = 31337

  fun randint () =
    let in
      ctr := !ctr + 1;
      seed := (!seed + !ctr) mod modu;
      seed := (!seed * 2) mod modu;
      seed := (!seed + 101) mod modu;
      seed := (!seed * 8111) mod modu;
      !seed
    end
end

exception Test of string

fun test x =
  let
    val h = H.empty () : int H.heap

    (* all of these functions maintain the invariant that the 
       heap has elements equal to their priority *)

    (* straight-up insertions *)
    fun ins () =
      let
        val n = randint ()
      in
        (* print ("insert " ^ Int.toString n ^ "\n"); *)
        ignore (H.insert h n n)
        (* H.printheap Int.toString Int.toString h *)
      end
      
    (* insert and delete *)
    fun del () =
      let
        (* might as well use an incorrect
           priority so we can tell if it wasn't deleted *)
        val n = randint ()
        val m = randint ()

        val hand = H.insert h m n
      in
        H.delete h hand;
        if H.valid hand
        then raise Test "failed--good handle after del"
        else ()
      end

    (* get min *)
    fun min () = ignore (H.min h)
    
    (* insert, adjust *)
    fun adj () =
      let
        val n = randint ()
        val m = randint ()

        (*
        val _ =
          print ("insert pri=" ^ Int.toString m ^ " val=" ^ 
                 Int.toString n ^ "\n")
          *)
        val hand = H.insert h m n
          (*
        val _ = print ("Hand after: " ^ H.handtostring hand ^ "\n")
        val _ =
          H.printheap Int.toString Int.toString h
          *)
      in
        ins ();
        ins ();        ins ();        ins ();        ins ();        ins ();
        ins ();        ins ();        ins ();        ins ();        ins ();
        ins ();        ins ();        ins ();        ins ();        ins ();

        del ();        ins ();        del ();        ins ();

        (*
        print ("set (val " ^ Int.toString n ^ ") to pri " ^
               Int.toString n ^ "\n");
        print ("Hand: " ^ H.handtostring hand ^ "\n");
        print ("delete first.\n");
        *)
        H.adjust h hand n;

        if not (H.valid hand)
        then raise Test "failed--bad handle after adjust"
        else ()
      end

    (* check that the heap is okay *)
    fun check () =
      let

        fun build () =
          case H.min h of
            NONE => nil
          | SOME (p,a) => 
              let in
                (* print ("\n\n### got min: " ^ Int.toString p ^ "\n");
                H.printheap Int.toString Int.toString h; *)
                if p <> a
                then raise Test "failed--priority<>a"
                else p :: build ()
              end

        (* val _ = print "CHECK TIME!\n" *)
        
        val l = build ()

      in
        print (Int.toString (length l) ^ " elts.. ");
        if alladjacent op<= l
        then print "ok!\n"
        else raise Test "failed--not sorted"
      end

    val cc = ref 1
    val _ = reset (!cc)

    fun go 0 = print "done.\n"
      | go n =
      let in
        (case randint () mod 25 of
           0 => if randint () mod 4 = 0
                then 
                  let in
                    check ()
                    (* didn't fail... reset *)
                    (* cc := !cc + 1;
                    reset (!cc);
                    print ("now, counter=" ^ Int.toString (!cc) ^ "\n") *)
                  end
                else min ()
         | 1 => ins ()
         | 2 => del ()
         | _ => adj ());

         go (n - 1)
      end

  in
    go x;
    check ()
  end

val _ = test 100000 handle Test s => print (s ^ "\n")