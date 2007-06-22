
(* simple tokenizer for text files. attempts to do the "right" thing
   with as little work as possible. If it's not right, you'll have to
   write your own... *)

structure SimpleTok :> SIMPLETOK = 
struct

  open Parsing

  infixr 4 << >>
  infixr 3 &&
  infix  2 -- ##
  infix  2 wth suchthat return guard when
  infixr 1 ||

  (* XX should support other quoting characters *)
  val quotc = #"\"" (* " *)

  datatype charstyle =
      CHSPrefix of char
    | CHSQuoted of char
    | CHSBracketed of string * string
 
  datatype intstyle =
      ISStandard
    | ISHex
    | ISBinary

  datatype floatstyle =
      FSStandard

  datatype stringstyle =
      SSStandard
    | SSMultiline

  datatype commentstyle =
      CSBracketed of string * string
    | CSLine of string

  type 'a tokenizer =
    { tokens : (string * 'a) list,
      other : (string -> 'a),

      int : (int -> 'a),
      (* styles, negator character *)
      inthow : intstyle list * char option,

      string : (string -> 'a),
      stringhow : stringstyle list,

      char : (char -> 'a),
      charhow : charstyle list,

      float : (real -> 'a),
      floathow : floatstyle list,

      issep : char -> bool,
      
      commenthow : commentstyle list
      
      }

  fun litstring s =
    let
      val ss = size s
      fun next n =
        if n >= ss 
        then succeed ()
        else literal (CharVector.sub(s, n)) -- (fn _ => next (n + 1))
    in
      next 0
    end

  exception SimpleTok of string
  fun none _ = raise SimpleTok "not initialized"
  fun K x y = x

  fun empty () = 
    { tokens = nil,
      other = none,
      int = none, inthow = (nil, NONE),
      string = none, stringhow = nil,
      char = none, charhow = nil,
      float = none, floathow = nil,
      issep = K true, commenthow = nil }

  fun default oth i s =
    { tokens = nil,
      other = oth,
      int = i, inthow = ([ISStandard], SOME #"-"),
      string = s, stringhow = [SSStandard],
      char = none, charhow = nil,
      float = none, floathow = nil,
      issep = Char.contains "()[],{};.%",
      commenthow = nil }

  fun setint { tokens, other, int = _, inthow = _, string, stringhow,
               char, charhow, float, floathow, issep, commenthow } ni nih neg =
     { tokens = tokens, other = other, 
       int = ni, inthow = (nih, neg), string = string,
       stringhow = stringhow, char = char, charhow = charhow,
       float = float, floathow = floathow, issep = issep, 
       commenthow = commenthow }

  fun settokens { tokens = _, other, int, inthow, string, stringhow,
                  char, charhow, float, floathow, issep, commenthow } ts =
    { tokens = ts, other = other, 
      int = int, inthow = inthow, string = string,
      stringhow = stringhow, char = char, charhow = charhow,
      float = float, floathow = floathow, issep = issep, 
      commenthow = commenthow }
    
  fun setother { tokens, other = _, int, inthow, string, stringhow,
                 char, charhow, float, floathow, issep, commenthow } other =
    { tokens = tokens, other = other, 
      int = int, inthow = inthow, string = string,
      stringhow = stringhow, char = char, charhow = charhow,
      float = float, floathow = floathow, issep = issep, 
      commenthow = commenthow }
    
  fun setstring { tokens, other, int, inthow, string = _, stringhow = _,
                  char, charhow, float, floathow, issep, commenthow } 
                string stringhow =
    { tokens = tokens, other = other, 
      int = int, inthow = inthow, string = string,
      stringhow = stringhow, char = char, charhow = charhow,
      float = float, floathow = floathow, issep = issep, 
      commenthow = commenthow }
    
  fun setchar { tokens, other, int, inthow, string, stringhow,
                char = _, charhow = _, float, floathow, issep, commenthow } 
              char charhow =
    { tokens = tokens, other = other, 
      int = int, inthow = inthow, string = string,
      stringhow = stringhow, char = char, charhow = charhow,
      float = float, floathow = floathow, issep = issep, 
      commenthow = commenthow }
    
  fun setfloat { tokens, other, int, inthow, string, stringhow,
                 char, charhow, float = _, floathow = _, issep, commenthow } 
               float floathow =
    { tokens = tokens, other = other, 
      int = int, inthow = inthow, string = string,
      stringhow = stringhow, char = char, charhow = charhow,
      float = float, floathow = floathow, issep = issep, 
      commenthow = commenthow }
    
  fun setcomment { tokens, other, int, inthow, string, stringhow,
                   char, charhow, float, floathow, issep, commenthow = _} 
                 commenthow =
    { tokens = tokens, other = other, 
      int = int, inthow = inthow, string = string,
      stringhow = stringhow, char = char, charhow = charhow,
      float = float, floathow = floathow, issep = issep, 
      commenthow = commenthow }
    
  fun setsep { tokens, other, int, inthow, string, stringhow,
               char, charhow, float, floathow, issep = _, commenthow} 
             issep = 
    { tokens = tokens, other = other, 
      int = int, inthow = inthow, string = string,
      stringhow = stringhow, char = char, charhow = charhow,
      float = float, floathow = floathow, issep = issep, 
      commenthow = commenthow }

  fun error s = raise SimpleTok s
  fun perror s _ = error s

  val newline = Char.contains "\n\r"
  val whitespace = Char.contains " \n\r\t"

  (* create the parser corresponding to the tokenizer *)
  fun parser ({tokens, other, 
               int, inthow=(inthow,negator), 
               string = mkstring, stringhow,
               char, charhow,
               float, floathow,
               issep,
               commenthow} : 'a tokenizer) =
    let

      (* since hex and bin have prefixes in ISStandard,
         make sure standard is tried last *)
      val inthow =
        let fun sorty (ISStandard :: rest) =
                        sorty rest @ [ISStandard]
              | sorty (how :: rest) = how :: sorty rest
              | sorty nil = nil
        in sorty inthow
        end

      (* some utils *)
      fun ahead p = lookahead p (fn _ => succeed ())
      fun ignore p = p return ()


      (* FIXME there is no support for floats! *)
      (* integers and floats *)
      local
        val decdigits = repeat1 (satisfy Char.isDigit)
        val hexdigits = repeat1 (satisfy (Char.contains "0123456789abcdefABCDEF"))
        val bindigits = repeat1 (satisfy (Char.contains "01"))


        (* trick: (ch | 4400) % 55 *)
        fun hexvalue ch =  
          SysWord.toInt (SysWord.orb(SysWord.fromInt(ord ch), 
                                     SysWord.fromInt 4400)) mod 55

        fun unhex acc nil = acc
          | unhex acc (h::t) = 
          unhex ((acc * 16) + hexvalue h) t

        fun unbin acc nil = acc
          | unbin acc (#"0"::t) = unbin (acc * 2) t
          | unbin acc (_::t)    = unbin (acc * 2 + 1) t
               
      in
        val floatingpoint = 
            alt (map
                 (fn FSStandard => 
                  decdigits && (literal #"." >> decdigits)
                    wth (fn (ip, dp) => 
                         (* XXX precision won't be perfect
                            if done this way, oh well *)
                         real 
                         (Option.valOf (Int.fromString (implode ip))) +
                         (real 
                          (Option.valOf
                           (Int.fromString
                            (implode dp))) /
                          Math.pow(10.0, real (length dp)))
                         
                         )) floathow)

        val integer =
          ((case negator of
              NONE => succeed NONE
            | SOME c => opt (literal c))
              &&
              alt (map
                   (fn ISStandard => decdigits wth (Option.valOf o 
                                                    Int.fromString o 
                                                    implode)
                     | ISHex => literal #"0" >> literal #"x" >> 
                                      hexdigits wth unhex 0
                     | ISBinary => literal #"0" >> literal #"b" >> 
                                      bindigits wth unbin 0) inthow))
              wth (fn (SOME _, n) => ~n
                    | (_, n) => n)
      end

      (* comments and whitespace *)
      local
        fun nestedcomment () = 
          let
            fun nc (CSBracketed (l, r) :: rest) =
              litstring l >>
              (repeat ($(insidecomment r)) << litstring r
               guard perror "Unterminated comment.")
              ## (fn _ => nc rest)
              | nc (_ :: rest) = nc rest
              | nc nil = fail
          in
            nc commenthow
          end


        (* Either a nested comment or a single character (which is not
           start of a nested comment or the comment terminator). *)

        and insidecomment ending () =
            ignore ($nestedcomment)
               || 
            let fun matchend nil = fail
                  | matchend (c :: t) =
              any -- (fn d =>
                      if c = d 
                      then ahead (matchend t)
                      else succeed ())
            in matchend (explode ending)
            end

        and restofline () =
          repeat (satisfy (not o newline))

        and comment () =
          ignore($nestedcomment) ||
          ignore(alt (List.mapPartial
                      (fn CSLine s =>
                       SOME (string (explode s) &&
                             $restofline)
                    | _ => NONE) commenthow))
      in          
        val skipspace = 
          repeat (ignore ($comment) || ignore (satisfy Char.isSpace))
      end


      (* identifiers and tokens. could be fancier. For instance,
         it's not possible to parse a+b as three tokens right now,
         unless + is a separator character *)
      local 
          
        fun notsep c =
          not (issep c orelse whitespace c)

      in
        val sym = 
          alt [(satisfy issep) wth Char.toString,
               repeat1 (satisfy notsep) wth implode]

        fun tt (nil, s) = other s
          | tt (((h,a)::t), s) = 
          if s = h
          then a
          else tt (t, s)

        fun trytoken l s = tt(l, s)
      end

      (* string literals *)
      local

        (* XXX add more character escapes... *)
        val escapechar = 
          ((literal #"\\" >> literal quotc) ||
           (literal #"\\" >> literal #"\\") ||
           (literal #"\\" && literal #"n" return #"\n"))

        (* get a possibly escaped character *)
        fun getchar nl = 
          (satisfy (fn x => x <> quotc 
                    andalso (nl orelse x <> #"\n") 
                    andalso x <> #"\\") ||
           escapechar)

        fun insidechars nl = repeat (getchar nl) wth implode

        fun strlit nl = middle 
            (literal quotc)
            (insidechars nl)
            (literal quotc)

        fun stringlit () =
          let
            datatype how = No | Std | Multi
            fun findhow a nil = a
              | findhow _ (SSStandard::rest) = findhow Std rest
              | findhow _ (SSMultiline::_) = Multi

            val how = findhow No stringhow
          in
            case how of
              No => fail
            | Std => strlit false
            | Multi => strlit true
          end

      in
        val stringlit = $stringlit

      end

    in
      
      skipspace >>
         alt [floatingpoint wth float,
              integer wth int,
              stringlit wth mkstring,
              sym wth (trytoken tokens)]
    end

  fun tokenize tk s =
    let 
      val pr = parser tk
      val ms = Pos.markstream s
    in
      Stream.tolist (transform pr ms)
    end

  (* XXX these are duplicated from ../util/streamutil *)

  (* converts a string to a char stream *)
  fun stringstream s =
    let
      val ss = size s
      fun next n () = 
        if n >= ss
        then Stream.empty
        else Stream.lcons (CharVector.sub(s, n),
                           next (n + 1))
    in
      Stream.old_delay (next 0)
    end
  

  (* convert a file to a char stream *)
  fun filestream f =
    let
      val ff = BinIO.openIn f
        
      fun rd () =
        case BinIO.input1 ff of
          NONE => (BinIO.closeIn ff; 
                   Stream.empty)
        | SOME c => Stream.lcons(chr (Word8.toInt c), rd)
    in
      Stream.old_delay rd
    end

end
