
(* read standard MIDI files. *)

structure MIDI :> MIDI =
struct

  val dprint = fn _ => ()

  open Reader

  exception MIDI of string
                            
  fun padreader (r : Reader.reader) s =
    Reader.fromvec (#seek r 0; #vec r (#size r) ^ s)

  val padding = "\000\000\000\000"

  fun pad r = padreader r padding

  fun pad r = r

  (* returns list of chunks:
     (type, subreader) *)
  fun getchunks (r : Reader.reader) =
    let
      fun readchunks r =
        let in

          dprint "@";
          dprint (Int.toString (#pos r ()) ^ "/" ^
                  Int.toString (#size r) ^ "\n");

          if eof r
          then nil
          else 
            let
              val title = #vec r 4
              val _ = dprint (title ^ "...\n");
              val len = rb32 r
              val _ = dprint ("make subreader of size " ^
                              Int.toString len ^ "\n")
              val nr = pad (fromreader r len) 
            in
              dprint "skip...\n";
              skip r len;
              (title, nr) :: readchunks r
            end

        end
    in
      #seek r 0;
      saveexcursion readchunks r
    end

  (* read a variable length quantity from r,
     return the 32 bit word it represents. *)
  fun gvarw (r : reader) = 
    let fun loop result =
      let 
        val b = Word8.fromInt (ord (#char r ()))
        val more = Word8.> (Word8.andb(b, 0wx80), 0w0)
        val rest = Word8.toInt (Word8.andb(b, 0wx7F))
        val res = Word32.orb(Word32.<<(result, 0w7),
                             Word32.fromInt rest)
      in
(*        dprint ("  vari: got " ^ Word8.toString b ^ ", " ^
               "more: " ^ Bool.toString more ^ " now=" ^
               Word32.toString res ^ "\n"); *)
        if more then loop res
        else res
      end

    in
      loop 0w0
    end

  (* good thing ML is the Meta Language *)
  datatype meta =
      END
    | SEQNO of int option
    | TEMPO of int

    | TEXT of string
    | COPY of string
    | NAME of string
    | INST of string
    | LYRIC of string
    | MARK of string
    | CUE of string
    | PROG of string
    | DEV of string

      (* hr mn sec fr subfr *)
    | SMPTE of int * int * int * int * int
      (* numerator log(denominator) clocksperclick bb *)
    | TIME of int * int * int * int

    (* sharps/flats maj/minor *)
    | KEY of int * bool
      
    (* proprietary data, like sysex *)
    | PROP of string
    (* unknown *)
    | OTHER of int * string

  datatype event =
      (* channel note vel *)
      NOTEON of int * int * int
    | NOTEOFF of int * int * int (* vel = 0 ?? *)
    (* channel program *)
    | PCHANGE of int * int
    (* channel controller value *)
    | CONTROL of int * int * int
    (* channel pitch1 pitch2 (??) *)
    | PITCH of int * int * int
    (* sysex data *)
    | SYSEX of CharVector.vector
      
    | META of meta

  val itos = Int.toString

  type track = (int * event) list

  fun merge trl =
      let
          val trl = List.filter (fn nil => false | _ => true) trl
      in
          case trl of
              nil => nil
            | _ => 
                  let
                      (* find the next event. It's the one
                         with the nearest delta time. *)
                      fun mini (a, v, _, nil) = (a, v)
                        | mini (a, v, i, ((dt, _) :: _) :: rest) = 
                          if v <= dt then mini (a, v, i + 1, rest)
                          else mini (i, dt, i + 1, rest)
                        | mini (_, _, _, nil :: rest) = raise MIDI "merge:impossible"
                              
                      val (next, v) = 
                          mini (~1,
                                (* sum of all delta times + 1; must be larger
                                   if midi is well-formed *)
                                foldr (op+) 1 (map (#1 o hd) trl),
                                0, 
                                trl)
                      val () = if next < 0 
                               then raise MIDI "merge:illegal delta time"
                               else ()

                      (* val () = print ("FFWD: " ^ Int.toString v ^ "\n"); *)

                      (* fast forward to that future *)
                      val trl = map (fn ( (dt, e) :: t ) => (dt - v, e) :: t
                                     | _ => raise MIDI "merge:impossible2") trl

                      (* get all events at the head, in order, that have zero delta-time.
                         nb. one of these must be zero-time. 
                         nb. this won't get multiple events from the same track. *)
                      val nowevts = List.mapPartial 
                                      (fn ((0, e) :: t) => SOME e | _ => NONE) trl

                      (* and take those off of the track list *)
                      val trl = map (fn ((0, e) :: t) => t | x => x) trl
                  in
                      (* the first nowevent triggers with the min, delta time
                         the other nowevents are 0 delta from that event.
                         then continue with the rest... *)
                      (v, hd nowevts) :: map (fn e => (0, e)) (tl nowevts) @
                      merge trl
                  end
      end

  fun mergei trl = 
      let 
          fun number _ nil = nil
            | number x (h :: t) = map (fn (a, b) => (a, (x, b))) h :: number (x + 1) t
          val trnl = number 0 trl
      in
          merge trnl
      end

  fun mergea atrl =
      let
          val trl = map (fn (a, b) => map (fn (x, y) => (x, (a, y))) b) atrl
      in
          merge trl
      end

  fun etos evt =
    (case evt of
        NOTEON (ch, n, vel) => "NOTEON " ^ itos ch ^ " " ^ itos n ^ 
                               " " ^ itos vel
      | NOTEOFF (ch, n, vel) => "NOTEOFF " ^ itos ch ^ " " ^ itos n ^ 
                                " " ^ itos vel
      | _ =>  "unimp")

  local exception LastEvent of int
  in
  fun getevents stat r =
    if eof r
    then nil
    else
      let val delta = Word32.toInt (gvarw r)
(*
        val _ = dprint (" Delta: " ^ Int.toString delta ^ "\n")
*)
        val (evt, rest) =
       let 
         val c = Word8.fromInt (ord (#char r ()))
         val status =
           (case (Word8.andb(0wx80, c), stat) of
              (0wx80, _) => c
            | (_, SOME s) => s before skip r ~1
            | _ => raise MIDI "expected status byte")

(*
         val _ = dprint ("  STATUS: " ^ Word8.toString status ^ "\n")
*)
         val this =
         (* (hi, lo) *)
         case (Word8.andb(0wx0f, Word8.>>(status,0w4)),
               Word8.andb(0wx0f, status)) of
           (0wx9, ch) => 
             NOTEON(Word8.toInt ch, 
                    ord (#char r ()),
                    ord (#char r ()))
         | (0wx8, ch) => 
             NOTEOFF(Word8.toInt ch, 
                     ord (#char r ()),
                     ord (#char r ()))
         | (0wxC, ch) => 
             PCHANGE(Word8.toInt ch, 
                     ord (#char r ()))
         | (0wxB, ch) => 
             CONTROL(Word8.toInt ch,
                     ord (#char r ()),
                     ord (#char r ()))
         | (0wxE, ch) => 
             PITCH(Word8.toInt ch,
                   ord (#char r ()),
                   ord (#char r ()))
         | (0wxF, 0wx0) => 
             (* SYSEX *)
             let val num = Word32.toInt (gvarw r)
                 val dat = #vec r num
             in 
(*               dprint "sysex...\n"; *)
               (* some stupid MIDI devices want large SYSEX events
                  split into many packets with delay. We don't support
                  these, because they complicate parsing quite a bit. *)
               CharVector.sub(dat, CharVector.length dat - 1) = (chr 0xF7)
                  orelse raise MIDI "split SYSEX not supported";
               SYSEX dat
             end
         | (0wxF, 0wxF) =>
             (* META *)
             let val b = #char r ()
                 val len = Word32.toInt (gvarw r)
             in
               (case ord b of
                  0 => if len = 2 then META (SEQNO (SOME (rb16 r)))
                       else if len = 0 then META (SEQNO NONE)
                            else raise MIDI "SEQNO must have len 0 or 2"
                | 1 => META (TEXT (#vec r len))
                | 2 => META (COPY (#vec r len))
                | 3 => META (NAME (#vec r len))
                | 4 => META (INST (#vec r len))
                | 5 => META (LYRIC(#vec r len))
                | 6 => 
                    let val s = #vec r len
                    in dprint ("mark: " ^ s ^ "\n");
                       META (MARK s)
                    end
                | 7 => META (CUE  (#vec r len))
                | 8 => META (PROG (#vec r len))
                | 9 => META (DEV  (#vec r len))
                | 0x2f =>
                    if len = 0
                    then raise LastEvent delta
                    else raise MIDI "got meta end but len > 0"
                | 0x51 =>
                    if len = 3
                    then META (TEMPO ((ord (#char r ())) * 65536 + rb16 r))
                    else raise MIDI "got meta tempo but len <> 3"
                | 0x54 => 
                    if len = 5
                    then META (SMPTE (ord (#char r ()),
                                      ord (#char r ()),
                                      ord (#char r ()),
                                      ord (#char r ()),
                                      ord (#char r ())))
                    else raise MIDI "got meta smpte but len <> 5"
                | 0x58 => 
                      if len = 4
                      then META (TIME (ord (#char r ()),
                                       ord (#char r ()),
                                       ord (#char r ()),
                                       ord (#char r ())))
                      else raise MIDI "got meta time but len <> 4"
                | 0x59 =>
                      if len = 2
                      then META (KEY (ord (#char r ()),
                                      ord (#char r ()) > 0))
                      else raise MIDI "got meta key but len <> 2"
                | 0x7F => META (PROP (#vec r len))
                | w => META (OTHER (w, #vec r len)))

             end
         | _ => raise MIDI "unknown status byte"

(*         val _ = dprint ("    event: " ^ etos this ^ "\n") *)

           val rest = getevents (SOME status) r
       in
         (this, rest)
       end
      in
        (delta, evt) :: rest
      end handle LastEvent d => [(d, META END)]
  end

  fun readtrack (r : reader) = 
    let in
(*
      dprint "Track...\n";
      dprint (StringUtil.hexdump (vecat r 0 (#size r)));
      dprint "\n";
*)
      getevents NONE r
    end

  fun readmidi r =
    let
      val r = pad r

(*
      val _ = 
        let in

          dprint "File...\n";
          dprint (StringUtil.hexdump (vecat r 0 (#size r)));
          dprint "\n"

        end
 *)
      val chunks = getchunks r

      (* from listutil *)
      fun find eq nil key = NONE
        | find eq ((a,b)::t) key =
          if eq (a, key) then SOME b
          else find eq t key

    in
      (case find op= chunks "MThd" of
         SOME rdr =>
           let
             val ty = rb16 rdr
             val ntrk = rb16 rdr
             val division = rb16 rdr

             val ts = List.mapPartial 
                        (fn ("MTrk", r) => SOME r | _ => NONE) chunks
           in
             eof rdr orelse raise MIDI "bad header length";
             length ts = ntrk 
               orelse raise MIDI "wrong number of tracks according to header";
             (ty <> 0 orelse ntrk = 1)
               orelse raise MIDI "type 0 must have 1 track";
             (ty, division, map readtrack ts)
           end
       | NONE => raise MIDI "no header")
    end handle (e as MIDI s) =>
      let in
        dprint ("MIDI error: " ^ s ^ "\n");
        raise e
      end

  fun wr f s =
    BinIO.output (f,
                  Word8Vector.tabulate 
                    (size s, fn x => 
                     Word8.fromInt (ord (CharVector.sub(s, x)))))

  fun wb32 n =
    implode (map chr [(n div (256*256*256)) mod 256,
                      (n div (256*256)) mod 256,
                      (n div  256) mod 256,
                       n mod 256])

  fun wb24 n =
    implode (map chr [(n div (256*256)) mod 256,
                      (n div 256) mod 256,
                       n mod 256])

  fun wb16 n =
    implode (map chr [(n div 256) mod 256,
                       n mod 256])

  fun mkchunk typ data = concat[typ, wb32 (size data), data]

  fun wb8 c = implode [chr c]

  (* n >= 0 *)
  fun wvar n = wvar' wb8 n
  and wb8n x = wb8 (128 + x) (* with "next" marker *)
  and wvar' f n = if n < 128
                  then f n
                  else wvar' wb8n (n div 128) ^ f (n mod 128)

  fun mktrack iel =
    let
      fun twob sb ch = Word8.orb(Word8.<<(sb, 0w4),
                                 Word8.fromInt ch)
      fun wstatus status e =
        let val (ns, mandatory) =
          (* Sonar 7 doesn't like omitted status bytes
             for META events. This is non-standard. 
             But we should omit them for other events, 
             like notes... *)
          (case e of
             CONTROL (ch, _, _) => (twob 0wxB ch, false)
           | NOTEOFF (ch, _, _) => (twob 0wx8 ch, false)
           | NOTEON  (ch, _, _) => (twob 0wx9 ch, false)
           | PCHANGE (ch, _)    => (twob 0wxC ch, false)
           | PITCH   (ch, _, _) => (twob 0wxE ch, false)
           | SYSEX _ => (0wxF0, true)
           | META _  => (0wxFF, true))
        in
          (ns, 
           case status of
             SOME os => if os = ns andalso not mandatory
                        then "" 
                        else implode [chr (Word8.toInt ns)]
           | NONE => implode [chr (Word8.toInt ns)])
        end

      fun wevt e =
        (case e of
             CONTROL (_, a, b) => wb8 a ^ wb8 b
           | NOTEOFF (_, a, b) => wb8 a ^ wb8 b
           | NOTEON  (_, a, b) => wb8 a ^ wb8 b
           | PCHANGE (_, a)    => wb8 a
           | PITCH   (_, a, b) => wb8 a ^ wb8 b 
           | SYSEX v => wvar (size v) ^ v
           | META me => 
               let
                 fun raw n v = wb8 n ^ wvar (size v) ^ v
               in
                 case me of
                   SEQNO (SOME r) => wb8 0 ^ wb8 2 ^ wb16 r
                 | SEQNO NONE => wb8 0 ^ wvar 0
                 | TEXT v => raw 1 v
                 | COPY v => raw 2 v
                 | NAME v => raw 3 v
                 | INST v => raw 4 v
                 | LYRIC v => raw 5 v
                 | MARK v => raw 6 v
                 | CUE v => raw 7 v
                 | PROG v => raw 8 v
                 | DEV v => raw 9 v
                 | TEMPO n => wb8 0x51 ^ wb8 3 ^ wb24 n
                 | SMPTE (a, b, c, d, e) => 
                     wb8 0x54 ^ wb8 5 ^
                     concat (map wb8 [a, b, c, d, e])
                 | TIME (a, b, c, d) =>
                     wb8 0x58 ^ wb8 4 ^
                     concat (map wb8 [a, b, c, d])
                 | KEY (a, b) =>
                     wb8 0x59 ^ wb8 2 ^
                     wb8 a ^ (if b then wb8 1 else wb8 0)
                 | PROP v => raw 0x7F v
                 | OTHER (w, v) => raw w v
                 | END => wb8 0x2f ^ wb8 0
               end)

      fun evt status ((delta, e)::rest) =
        let
           val (nstat, sb) = wstatus status e
        in
           wvar delta ::
           sb ::
           wevt e ::
           evt (SOME nstat) rest
        end
        | evt _ _ = nil

    in
      mkchunk "MTrk" (concat (evt NONE iel))
    end


  fun writemidi fname (ty, di, trks) =
    let
      val ts = map mktrack trks
      val ntrk = length ts

      val _ = 
        (ty <> 0 orelse ntrk = 1)
        orelse raise MIDI "type 0 must have 1 track"

      val hdr = mkchunk "MThd" (concat[wb16 ty, 
                                       wb16 ntrk,
                                       wb16 di])

      val f = BinIO.openOut fname
        
    in
      wr f hdr;
      app (wr f) ts;
      BinIO.closeOut f
    end



  fun mtos m =
      case m of
          END => "END"
        | SEQNO NONE => "SEQNO NONE"
        | SEQNO (SOME n) => "SEQNO " ^ itos n
        | TEMPO n => "TEMPO " ^ itos n
        | TEXT s => "TEXT \"" ^ String.toString s ^ "\""
        | COPY s => "COPY \"" ^ String.toString s ^ "\""
        | NAME s => "NAME \"" ^ String.toString s ^ "\""
        | INST s => "INST \"" ^ String.toString s ^ "\""
        | LYRIC s => "LYRIC \"" ^ String.toString s ^ "\""
        | MARK s => "MARK \"" ^ String.toString s ^ "\""
        | CUE s => "CUE \"" ^ String.toString s ^ "\""
        | PROG s => "PROG \"" ^ String.toString s ^ "\""
        | DEV s => "DEV \"" ^ String.toString s ^ "\""
        | PROP s => "PROP \"" ^ String.toString s ^ "\""
        | OTHER (i, s) => "OTHER " ^ itos i ^ "\"" ^ String.toString s ^ "\""
        | SMPTE (hr, mn, sec, fr, sf) => "SMPTE " ^ itos hr ^ ":" ^ itos mn ^
                                         ":" ^ itos sec ^ ":" ^ itos fr ^
                                         ":" ^ itos sf
        | TIME (n, d, cpc, bb) => "TIME " ^ itos n ^ "/" ^ itos d ^
                                      " " ^ itos cpc ^ " " ^ itos bb
        | KEY (i, min) => "KEY " ^ itos i ^ " " ^ (if min then "min" else "maj")

  val digits = "0123456789ABCDEF"
  fun hexdig c = implode [CharVector.sub (digits, ord c div 16),
                          CharVector.sub (digits, ord c mod 16)]

  fun etos e =
      case e of
          META m => "META " ^ mtos m
        | SYSEX s => "SYSEX " ^ (String.concat (map hexdig (String.explode s)))
        | PITCH (p1, p2, p3) => "PITCH " ^ itos p1 ^ " " ^ itos p2 ^ " " ^ itos p3
        | CONTROL (c1, c2, c3) => "CONTROL " ^ itos c1 ^ " " ^ itos c2 ^ " " ^ itos c3
        | PCHANGE (a, b) => "PCHANGE " ^ itos a ^ " " ^ itos b
        | NOTEON (ch, n, v) => "NOTEON " ^ itos ch ^ " " ^ itos n ^ "@" ^ itos v
        | NOTEOFF (ch, n, v) => "NOTEOFF " ^ itos ch ^ " " ^ itos n ^ "@" ^ itos v

end
