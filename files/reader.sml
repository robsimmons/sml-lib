
(* A reader allows the extraction of vectors of characters from arbitrary
   points. We can think of it as an abstract interface to files on disk or
   in memory. This is intended to work around the bizarre omission in the
   BinIO basis structure of a way to seek arbitrarily in a file.

   The reader type is a record of all of the operations you can perform
   on it. All readers have the same type.

   The reader structure includes functions that generate readers from
   files or CharVector.vectors in memory. Recall that CharVector.vector
   is the same type as string.
*)

structure Reader : READER =
struct

    exception Bounds
    exception Reader of string

    type reader =
        { size  : int,
          seek  : int -> unit,
          pos   : unit -> int,
          char  : unit -> char,
          vec   : int -> CharVector.vector,
          close : unit -> unit }

    infix +=

    fun (r as ref n) += m = r := (n + m)
    fun ++ r = r += 1

    fun fromvec s =
        let
            val cl = ref false
            val loc = ref 0

            fun guard f x = (if !cl then raise Reader "reader is closed"
                             else f x)

            (* we allow a seek one character beyond the end of the file, 
               because pos can return this value *)

            fun seek n = if n < 0 orelse n > CharVector.length s 
                         then raise Bounds 
                         else loc := n
            fun pos () = !loc
            fun char i = CharVector.sub(s, !loc) before ++ loc 
                         handle _ => raise Bounds
            fun vec b = String.substring(s, !loc, b) before (loc += b) 
                        handle _ => raise Bounds
            fun close () = cl := true
        in
            { size  = size s,
              pos   = guard pos,
              seek  = guard seek,
              char  = guard char,
              vec   = guard vec,
              close = guard close }
        end
        
    (* XXX need to rewrite using Posix structure;
       this is missing the point entirely! *)
(* XXX broken on windows, loses newline chars
    fun fromfile name =
        let
            val f = (TextIO.openIn name) handle _ => raise Reader "can't open file"
            val v = TextIO.inputAll f
        in
            TextIO.closeIn f;
            fromvec v
        end 
*)
    (* workaround 3 year-old reported bug in BinIO.inputAll SML/NJ on 
       windows. Ugh. *)
    fun inputall inf =
      let 
          fun rd vs =
              let val v = BinIO.input inf
               in case Word8Vector.length v
                    of 0 => Word8Vector.concat (rev vs)
                       | _ => rd (v :: vs)
              end
       in
         rd nil
      end

    fun fromfile name =
      let
        val f = (BinIO.openIn name) 
            handle _ => raise Reader ("can't open file " ^ name)
        val v = inputall f
      in
        BinIO.closeIn f;
        fromvec (CharVector.tabulate
                 (Word8Vector.length v,
                  (fn x => chr (Word8.toInt (Word8Vector.sub(v, x))))))
      end

    fun saveexcursion f (r : reader) =
      let val p = #pos r ()
      in
        let val a = f r
        in
          #seek r p;
          a
        end handle e =>
          let in
            #seek r p;
            raise e
          end
      end

    fun fromreader r s = 
      saveexcursion (fn ({vec,...} : reader) => fromvec (vec s)) r

    fun makeint nil = 0
      | makeint (h::t) = ord h + (256 * makeint t)

    fun makew32 nil = 0w0
      | makew32 (h::t) = (Word32.fromInt (ord h)) + (0w256 * makew32 t)

    fun rl16 ({vec,...} : reader) = makeint (explode (vec 2))
    fun rl32 ({vec,...} : reader) = makeint (explode (vec 4))

    fun rb16 ({vec,...} : reader) = makeint (rev (explode (vec 2)))
    fun rb32 ({vec,...} : reader) = makeint (rev (explode (vec 4)))

    fun rlw16 ({vec,...} : reader) = makew32 (explode (vec 2))
    fun rlw32 ({vec,...} : reader) = makew32 (explode (vec 4))

    fun rbw16 ({vec,...} : reader) = makew32 (rev (explode (vec 2)))
    fun rbw32 ({vec,...} : reader) = makew32 (rev (explode (vec 4)))

    fun byte ({char,...} : reader) = Word8.fromInt (ord (char ()))

    fun strz (f as {char,...} : reader) =
        case char () of
            #"\000" => ""
          | c => implode [c] ^ strz f

    fun eof ({pos,size,...} : reader) = pos() >= size

    fun rest ({vec,pos,size,...} : reader) = vec (size - pos())

    fun skip (r : reader) n = #seek r (#pos r () + n)

    (* could use save-excursion instead of this *)
    fun vecat ({vec,seek,pos,...} : reader) beg len =
        let val p = pos ()
            (*
            val _ = print ("vecat (...) " ^ Int.toString beg ^ 
                           " " ^ Int.toString len ^ "\n")
            *)
        in  let in seek beg;
            vec len
            before
            seek p
            end handle e => (seek p; raise e)
        end

    fun strzat (f as {vec,seek,pos,...} : reader) beg =
        let val p = pos ()
        in  let in seek beg;
            strz f
            before
            seek p
            end handle e => (seek p; raise e)
        end


    fun line reader =
        if eof reader 
        then NONE
        else SOME (let
                       (* find newline or EOF. *)
                       val start = #pos reader ()
                       fun findn () =
                           let val p = #pos reader ()
                           in
                               if eof reader
                               then (p, p)
                               else
                               (* annoying, since we want to treat
                                  \n, \r\n, and \r (when not followed
                                  by \n) as line terminators. *)
                               case #char reader () of
                                   #"\n" => (p, p + 1)
                                 | #"\r" =>
                                   if eof reader
                                   then (p, p + 1)
                                   else (case #char reader () of
                                             #"\n" => (p, p + 2)
                                           | _ => (p, p + 1))
                                 | _ => findn ()
                           end
                       val (endp, skipp) = findn ()

                       val l = vecat reader start (endp - start)
                   in
                       #seek reader skipp;
                       l
                   end)

    fun token ws reader =
        let
            (* Start by eating characters until ws is false. *)
            fun eat () =
                if eof reader then NONE
                else let val c = #char reader ()
                     in
                        if ws c
                        then eat ()
                        else munch (#pos reader () - 1)
                     end
            and munch start =
                if eof reader 
                then SOME (vecat reader start 
                           (#pos reader () - start))
                else
                    let val c = #char reader ()
                    in
                        if ws c
                        then let val p = #pos reader () - 1
                                 val v = vecat reader start 
                                         (p - start)
                             in
                                 #seek reader p;
                                 SOME v
                             end
                        else munch start
                    end
        in
            eat ()
        end

end