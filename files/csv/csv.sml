structure CSV :> CSV =
struct

    datatype options = TRIM_WHITESPACE | ALLOW_CRLF

    exception CSV of string

    (* PERF could use unsafe *)
    structure CA = GrowMonoArrayFn_Safe(structure A = CharArray
                                        val default = #"\000")

    fun string a =
        CharArray.vector (CA.finalize a)

    fun readex opts r =
        let 
            datatype state = 
                (* Normal: inside unescaped token and have seen some non-whitespace
                   characters (which are in the array). The count is the length of
                   the prefix that we should keep if we discard whitespace from the
                   right. *)
                UNESCAPED of CA.growarray * int
              | INSIDE_QUOTES of CA.growarray
                (* Just saw a quote. It's not in the array. If the bool is
                   true, then it is followed by whiespace and we'd better
                   see the end of line, end of file, or a comma next. *)
              | SAW_QUOTE of CA.growarray * bool

            (* Horizontal whitespace. *)
            val isws = (if List.exists (fn TRIM_WHITESPACE => true | _ => false) opts
                        then (fn #" " => true | #"\t" => true | _ => false)
                        else (fn _ => false))

            val allowing_crlf = List.exists (fn ALLOW_CRLF => true | _ => false) opts

            (* PERF could have streaming interface. *)
            (* We don't translate CRLF inside quotes. *)
            fun nextchar s =
                if Reader.eof r 
                then NONE
                else 
                    let
                        fun ch () =
                            let val c = #char r ()
                            in
                                if c = chr 0
                                then print "0 char??"
                                else ();
                                c
                            end
                        fun single () = SOME (ch ())
                        fun double () =
                            case ch () of
                                #"\r" =>
                                if Reader.eof r
                                then SOME #"\r"
                                else (case ch () of
                                          #"\n" => SOME #"\n"
                                        | _ => (#seek r ~1; SOME #"\r"))
                              | c => SOME c
                    in
                        case (s, allowing_crlf) of
                            (INSIDE_QUOTES _, _) => single ()
                          | (_, false) => single ()
                          | (_, true) => double ()
                    end

            val rowsrev = ref nil
                
            fun fillrows () =
                let
                    val colsrev = ref nil
                    fun push s = colsrev := s :: !colsrev
                    fun fillcols state =
                        let val c = nextchar state
                        in case (state, c) of
                            (UNESCAPED (a, n), SOME #",") => 
                                let in
                                    (* discard trailing whitespace, if any *)
                                    CA.truncate a n;
                                    push (string a); 
                                    fillcols (UNESCAPED (CA.empty(), 0))
                                end

                          | (INSIDE_QUOTES a, SOME (c as #"\"")) => (* " *)
                                (* might be an escaped quote, or
                                   might be end. transition... *)
                                let in
                                    fillcols (SAW_QUOTE (a, false))
                                end

                          | (INSIDE_QUOTES a, SOME c) =>
                                let in
                                    CA.append a c;
                                    fillcols (INSIDE_QUOTES a)
                                end

                          | (INSIDE_QUOTES _, NONE) => 
                                raise CSV "File ended during quotation"

                          | (SAW_QUOTE (a, _), NONE) =>
                                (* OK for file to end, since we saw the
                                   closing quote and then maybe
                                   some whitespace. Done with this
                                   column and file. *)
                                let in
                                    push (string a);
                                    rowsrev := rev (!colsrev) :: !rowsrev
                                end

                          | (SAW_QUOTE (a, _), SOME #",") =>
                                (* Column ends *)
                                let in
                                    push (string a);
                                    fillcols (UNESCAPED (CA.empty(), 0))
                                end

                          | (SAW_QUOTE (a, _), SOME #"\n") =>
                                let in
                                    (* Line ends. *)
                                    push (string a);
                                    rowsrev := rev (!colsrev) :: !rowsrev;
                                    colsrev := nil;
                                    fillcols (UNESCAPED (CA.empty(), 0))
                                end

                          | (SAW_QUOTE (a, false), SOME (c as #"\"")) => (* " *)
                                (* treat as a single double-quote char. *)
                                let in
                                    CA.append a c;
                                    fillcols (INSIDE_QUOTES a)
                                end

                          | (SAW_QUOTE (a, n), SOME c) =>
                                if isws c
                                then
                                    let in
                                        CA.append a c;
                                        fillcols (SAW_QUOTE (a, true))
                                    end
                                else raise CSV "garbage after terminated quotation"

                          | (UNESCAPED (a, 0), SOME #"\"") => (* " *)
                                fillcols (INSIDE_QUOTES a)

                          | (UNESCAPED _, SOME #"\"") => (* " *)
                                raise CSV "Unescaped quote character"

                          | (UNESCAPED (a, n), SOME #"\n") =>
                                let in
                                    CA.truncate a n;
                                    push (string a);
                                    (* done with this column. *)
                                    rowsrev := rev (!colsrev) :: !rowsrev;
                                    colsrev := nil;
                                    fillcols (UNESCAPED (CA.empty(), 0))
                                end
                          | (UNESCAPED (a, n), NONE) =>
                                let in
                                    CA.truncate a n;
                                    push (string a);
                                    rowsrev := rev (!colsrev) :: !rowsrev;
                                    () (* done with this column and file. *)
                                end
                          | (UNESCAPED (a, n), SOME c) =>
                                let in
                                    (* Don't add it if it is whitespace and
                                       we haven't seen any real chars yet. *)
                                    if n > 0 orelse not (isws c)
                                    then CA.append a c
                                    else ();
                                    fillcols (UNESCAPED (a, if isws c
                                                            then n
                                                            else CA.length a))
                                end

                        end
                in
                    fillcols (UNESCAPED (CA.empty(), 0))
                end

        in
            fillrows ();
            rev (!rowsrev)
        end

    fun read f = readex [ALLOW_CRLF] (Reader.fromfile f)

end