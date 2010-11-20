
(* The LZW implementation is functorized over the alphabet
   that is being compressed. We just need to know what characters
   are and how to convert them into a dense set of integers. *)
signature LZWARG =
sig

  (* characters in the input stream *)
  type ch

  (* number of characters that exist *)
  val radix : int

  (* convert an integer in 0..(radix-1)
     to the corresponding ch *)
  val itoc : int -> ch
  val ctoi : ch -> int

end

(* Most likely you want to compress strings of 8-bit characters. *)
structure LZWCharArg =
struct 
    type ch = char 
    val itoc = chr 
    val ctoi = ord 
    val radix = 256 
end 

functor LZWFn(structure C : LZWARG
              (* LZW has a "special case" (where code = sz + radix) 
                 that improves compression ratios at the expense of
                 complexity in the decoder. If this is false, disable
                 that special case when encoding (producing a slightly
                 longer output) and fail if it is encountered when
                 decoding. *)
              val allow_special : bool
              (* which must be at least C.radix *)
              val tablesize : int option) :> 
              LZW where type ch = C.ch =
struct

  type ch = C.ch

  exception LZW of string

  (* XXX *)
  val () = case tablesize of NONE => () 
          | _ => raise LZW "table limits not yet supported"

  datatype output =
    CODE of int * int
  | RESET

  fun util_for lo hi f =
      if lo > hi then ()
      else (ignore (f lo); util_for (lo + 1) hi f)

  (* imperative streams *)
  type 'a stream = unit -> 'a option
  (* but we want to implement them in a more efficient way *)
  datatype 'a prestream = 
      PS of unit -> ('a * 'a prestream) option

  fun ps_to_stream (PS ps) =
      let
          val r = ref ps
          fun next () =
              (case (!r) () of
                   NONE => 
                       let in
                           r := (fn _ => NONE);
                           NONE
                       end
                 | SOME (a, PS af) =>
                       let in
                           r := af;
                           SOME a
                       end)
      in
          next
      end

  (* PERF how do I compute log_2 of ints?? *)
  (* can we represent it with x bits? n = 2^x *)
  fun needbits m =
    let
      fun l n x = if n >= m
                  then x
                  else l (n * 2) (x + 1)
    in
      l 2 1
    end
  
  (* dictionary for compression/decompression *)
  structure D :> 
  sig
    (* imperative dictionaries *)
    type dict
      
    (* functional cursor into dictionary,
       representing the lookup of a prefix *)
    type cursor

    (* derive cursor for single character string
       from dictionary *)
    val cursor : dict -> ch -> cursor
    (* get current cursor code *)
    val now : cursor -> int

    datatype res =
      Found of cursor
    (* can't extend; return code for old prefix *)
    | Max of int

    (* add character to cursor. *)
    val add : cursor -> ch -> res

    (* initial dictionary *)
    val initial : unit -> dict
    (* reset to initial *)
    val reset : dict -> unit

    val size : dict -> int

    (* number of bits needed to encode codes
       for the dictionary currently *)
    val bits : dict -> int
  end =
  struct

    (* trees *)
    datatype 'a tree = 
      EMPTY
    | NODE of 'a tree ref * int * 'a * 'a tree ref

    datatype node =
      N of { code : int,
             (* might be array0 if totally empty *)
             children : children }
    withtype children = node tree ref

    type dict = { nextcode : int ref,
                  (* of size C.radix *)
                  roots : node Array.array }

    (* 'children' ops *)

    fun c_empty () = ref EMPTY

    datatype findresult =
        FOUND of node
        (* insertion function *)
      | NOTFOUND of (node -> unit)

    fun c_find lastcode c i =
      (case c of
         (ref EMPTY) => 
           NOTFOUND (fn a =>
                     c := NODE(ref EMPTY, i, a, ref EMPTY))
       | (ref (NODE (l, j, a as (N{ code, children = _ }), r))) => 
           case Int.compare (i, j) of
             EQUAL => 
               (* avoid generating the very last code we inserted. *)
               if allow_special
                  orelse code <> (lastcode - 1)
               then
                 let in
                   (* print ("(lastcode = " ^ Int.toString lastcode ^ 
                          ") found code " ^ Int.toString code ^ "\n"); *)
                   FOUND a
                 end
               else 
                 let in
                   (* print ("AVOIDED SPECIAL for code "
                          ^ Int.toString (lastcode) ^ "\n"); *)
                 (* it's really already there, so just throw
                    away the new node (no need for two!) *)

                   NOTFOUND (fn _ => ())
                 end

           | LESS => c_find lastcode l i
           | GREATER => c_find lastcode r i)

    (* store the nextcode so we can generate
       it, and then just point to the node
       at this prefix. *)
    type cursor = int ref * node 

    datatype res =
      Found of cursor
    | Max of int

    (* this always succeeds by invariant *)
    fun cursor { nextcode, roots } ch =
      (nextcode, 
       Array.sub(roots, C.ctoi ch))

    fun now (_, N { code, ...}) = code

    fun add (nc, N{ code, children }) ch =
      case c_find (!nc) children (C.ctoi ch) of
        FOUND n => 
          let in
            Found (nc, n)
          end
      | NOTFOUND inserter => 
          (inserter (N { code = !nc,
                         children = c_empty () });
           nc := !nc + 1;
           Max code)

    fun size { nextcode=ref n, roots=_ } = n

    fun reset { nextcode, roots } = 
      let
      in
        nextcode := C.radix;
        util_for 0 (C.radix - 1)
        (fn i =>
         Array.update(roots, i, 
                      (N { code = i, 
                           children = c_empty () })))
      end

    fun initial () =
      let
        val d = { nextcode = ref 0, 
                  roots = Array.array(C.radix, 
                                      (* XX dummy, will be
                                         overwritten. *)
                                      N { code = 0, 
                                          children = c_empty ()}) }
      in
        reset d;
        d
      end

    fun bits d = needbits (size d - 1)

  end


  fun compress s =
    let
      val d = D.initial ()

        
      (* XXX limit table size *)

      (* encode with existing cursor *)
      fun enc cu () = 
        case s () of
          NONE => 
            (* stream ends. emit cursor *)
            SOME(CODE (D.now cu, D.bits d),
                 PS (fn () => NONE))
        | SOME c =>
            case D.add cu c of
              D.Found cu' => enc cu' ()
            | D.Max code => SOME(CODE (code, D.bits d),
                                 PS (fn () => enc_c c))

      and enc_c c = enc (D.cursor d c) ()

      (* no input *)
      and enc_ni () =
        case s () of
          NONE => NONE
        | SOME c => enc_c c
            
    in

      ps_to_stream (PS enc_ni)
    end


  fun stol f =
    (case f () of 
       NONE => nil
     | SOME c => c :: stol f)

  fun compress_array a =
    let
      val i = ref 0

      val s = 
        compress
        (fn () =>
         if !i >= Array.length a
         then NONE
         else SOME (Array.sub(a, !i)
                    before i := !i + 1))
    in
      Array.fromList (stol s)
    end

  (* decompression indexes the tree in the
     opposite order; it builds an array *)

  structure AS = ArraySlice

  datatype input =
    ICODE of int
  | ORESET

  fun decompressex s =
    let
     
      val no = (~1, 0)

      val sz = ref 0
      (* dictionary *)
      val d = ref (Array.array(1024, no))

      fun add (pos, len) =
        let in
          (* print ("(" ^ Int.toString pos ^ "," ^ Int.toString len ^ ")"); *)

        if !sz < (Array.length(!d) - 1)
        then 
          let in
            (* print ("code: " ^ (Int.toString (!sz)) ^
                      " is " ^ (Int.toString pos) ^ ", " ^
                      Int.toString len ^ "\n"); *)
            Array.update(!d, !sz, (pos, len));
            sz := !sz + 1;
            !sz
          end
        else
          let in
            d := Array.tabulate(Array.length (!d) * 2,
                                (fn x =>
                                 if x < Array.length(!d)
                                 then Array.sub(!d, x)
                                 else no));
            add (pos, len)
          end
        end

      (* output table *)
      val os = ref 0
      val out = ref (Array.array(1024, C.itoc 0))

      (* give me the appropriate arrayslice *)
      fun getstring n =
        if n < C.radix
           (* PERF could be from a constant array of
              all characters; then there's fewer
              allocations *)
        then AS.full (Array.fromList [C.itoc n])
        else let val (pos, len) = Array.sub(!d, n - C.radix)
             in
               AS.slice (!out, pos, SOME len)
             end

      (* add char to output *)
      fun emit c = 
        let in
          (* print (implode [chr (C.ctoi c)]); *)

        if !os < (Array.length(!out) - 1)
        then 
          let in
            (* print ("emit: " ^ implode [chr (C.ctoi c)] ^ "\n"); *)
            Array.update(!out, !os, c);
            os := !os + 1
          end
        else
          let in
            out := Array.tabulate(Array.length (!out) * 2,
                                  (fn x =>
                                   if x < Array.length(!out)
                                   then Array.sub(!out, x)
                                   else C.itoc 0));
            emit c
          end
        end

      fun getcodewithbits b s =
        case s () of
          NONE => NONE
        | SOME f => 
            case f b of
              ICODE i => SOME i
            | IRESET => raise LZW "table reset not implemented"

        
      fun getcode s = getcodewithbits (needbits (C.radix + !sz + 1)) s

      (* call with the start position and length of the
         code we emitted last *)
      fun dec oldstart oldlen =
        case getcode s of 
          NONE => (* done, and nothing to output *) NONE
        | SOME code => 
            if code < !sz + C.radix
            then 
              let 
                val sl = getstring code
              in
                (* emit the string *)
                AS.app emit sl;
                (* add the last string we
                   emitted previously, 
                   plus the first char of 
                   the new string *)
                add (oldstart, oldlen + 1);
                dec (oldstart + oldlen) (AS.length sl)
              end
            else
              if allow_special
              then
              (* "special" case; could check that
                 code is exactly !sz + C.radix *) 
              let 
                val sl = AS.slice(!out, oldstart, SOME oldlen)
                val c = Array.sub(!out, oldstart)
                val here = !os
              in
                AS.app emit sl;
                emit c;
                add (here, AS.length sl + 1);
                dec here (AS.length sl + 1)
              end
              else raise LZW "special not allowed"

      and start () =
        case getcodewithbits (needbits C.radix) s of
          NONE => (* empty input *) NONE
        | SOME code => 
            let in
              (* by invt, is 0..radix-1 *)
              emit (C.itoc code);
              (* the string is at position 0, length 1 *)
              dec 0 1
            end
    in
      start ();
      Array.tabulate(!os, 
                     (fn x => Array.sub(!out, x)))
    end


  fun smap f s () =
    (case s () of
       NONE => NONE
     | SOME c => SOME (f c))

  fun decompress is =
    decompressex (smap (fn i => fn _ => ICODE i) is)

end
