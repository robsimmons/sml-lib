(*--------------------------------------------------------------------------*)
(* Structure: UtilString                                                    *)
(*--------------------------------------------------------------------------*)
signature UtilString =
   sig
      val quoteString    : char -> string -> string

      val numberNth      : int -> string
      val prependAnA     : string -> string

      val nBlanks        : int -> string
      val padxLeft       : char -> string * int -> string
      val padxRight      : char -> string * int -> string

      val breakLines     : int -> string -> string list

      val toUpperFirst   : string -> string
      val toUpperString  : string -> string

      val Int2String     : int -> string

      val Bool2xString   : string * string -> bool -> string
      val Bool2String    : bool -> string

      val Option2xString : string * (('a -> string) -> 'a -> string) 
	 -> ('a -> string) -> 'a option -> string
      val Option2String0 : ('a -> string) -> 'a option -> string
      val Option2String  : ('a -> string) -> 'a option -> string

      val List2xString   : string * string * string -> ('a -> string) -> 'a list -> string
      val List2String0   : ('a -> string) -> 'a list -> string
      val List2String    : ('a -> string) -> 'a list -> string

      val Vector2xString : string * string * string -> ('a -> string) -> 'a vector -> string
      val Vector2String  : ('a -> string) -> 'a vector -> string
   end

structure UtilString : UtilString =
   struct 
      fun quoteString q s = let val quote = String.implode [q] in quote^s^quote end

      (*--------------------------------------------------------------------*)
      (* generate a string with the ordinal number of n, by appending       *)
      (* "st", "nd", "rd" or "th" to the number.                            *)
      (*--------------------------------------------------------------------*)
      fun numberNth n = 
	 let val suffix = case n mod 9
			    of 1 => "st"
			     | 2 => "nd"
			     | 3 => "rd"
			     | _ => "th"
	 in Int.toString n^suffix
	 end

      (*--------------------------------------------------------------------*)
      (* is the single character c represented by a word starting with a    *)
      (* vocal in the alphabet? (l~ell->true, k~kay->false)                 *)
      (*--------------------------------------------------------------------*)
      fun vocalLetter c =
	 case Char.toLower c 
	   of #"a" => true 
            | #"f" => true 
            | #"h" => true 
            | #"i" => true 
            | #"l" => true 
            | #"m" => true 
            | #"n" => true 
            | #"o" => true 
            | #"r" => true 
            | #"s" => true 
            | #"x" => true 
            | #"8" => true
	    | _    => false
	      
      (*--------------------------------------------------------------------*)
      (* is character c a vocal?                                            *)
      (*--------------------------------------------------------------------*)
      fun isVocal c = 
	 case Char.toLower c
	   of #"a" => true 
            | #"e" => true 
            | #"i" => true 
            | #"o" => true 
            | #"u" => true
	    | _    => false

      (*--------------------------------------------------------------------*)
      (* does a word require "an" as undefinite article? true if:           *)
      (* - it is a single letter that starts with a vocal in the alphabet   *)
      (* - its first two letters are capitals, i.e. it is an abbreviation,  *)
      (*   and its first letter starts with a vocal in the alphabet         *)
      (* - it has more than one letter, is not an abbreviation, and either  *)
      (*   + it starts with a, i or o                                       *)
      (*   + it starts with e and the second letter is not a u (europe)     *)
      (*   + it starts with a u and continues with a vocal (very unlikely,  *)
      (*     only in c.s., like uuencoded or uid                            *)
      (*   + it starts with a u, continues with a consonant not followed by *)
      (*     an i (like in unicode); that is something like un-...          *)
      (* This ruleset is not complete since it does not cover, e.g., the    *)
      (* word uninvented, but sufficient for most cases.                    *)
      (* (Is english pronounciation decidable at all?)                      *)
      (*--------------------------------------------------------------------*)
      fun extendsAtoAn word = 
	 case String.explode word 
	   of nil => false
	    | [c] => vocalLetter c
	    | c1::c2::cs => if not (Char.isLower c1 orelse Char.isLower c2) 
			       then vocalLetter c1
			    else case Char.toLower c1
				   of #"a" => true 
                                    | #"i" => true 
                                    | #"o" => true
				    | #"e" => Char.toLower c2 <> #"u"
				    | #"u" => if isVocal c2 then false
					      else (case cs
						      of nil => true
						       | c3::_ => Char.toLower c3 <> #"i")
				    | _ => false
						 
      (*--------------------------------------------------------------------*)
      (* add an undefinite article to a word.                               *)
      (*--------------------------------------------------------------------*)
      fun prependAnA word = if extendsAtoAn word then "an "^word else "a "^word

      (*--------------------------------------------------------------------*)
      (* generate a list/string of n times character c.                     *)
      (*--------------------------------------------------------------------*)
      fun nCharsC c n = if n>0 then c::nCharsC c (n-1) else nil
      fun nChars c n = String.implode (nCharsC c n)
      val nBlanks = nChars #" "
	 
      (*--------------------------------------------------------------------*)
      (* add a minimal number of characters c to the left/right of a string *)
      (* in order to make its length at least n.                            *)
      (*--------------------------------------------------------------------*)
      fun padxLeft c (s,n) = (nChars c (n-String.size s))^s
      fun padxRight c (s,n) = s^(nChars c (n-String.size s))
      val padLeft  = padxLeft  #" "
      val padRight  = padxRight  #" "
	 
      (*--------------------------------------------------------------------*)
      (* break a string into several lines of length width.                 *)
      (*--------------------------------------------------------------------*)
      fun breakLines width str =
	 let 
	    val tokens = String.tokens (fn c => #" "=c) str
	    fun makeLine(toks,lines) = if null toks then lines 
				       else (String.concat (rev toks))::lines
	    fun doit w (toks,lines) nil = makeLine(toks,lines)
	      | doit w (toks,lines) (one::rest) = 
	       let 
		  val l = String.size one
		  val w1 = w+l
	       in 
		  if w1<width then doit (w1+1) (" "::one::toks,lines) rest
		  else if w1=width then doit 0 (nil,makeLine(one::toks,lines)) rest
		  else if l>=width then doit 0 (nil,one::makeLine(toks,lines)) rest
		       else doit (l+1) ([" ",one],makeLine(toks,lines)) rest
	       end
	 in List.rev (doit 0 (nil,nil) tokens)
	 end

      (*--------------------------------------------------------------------*)
      (* convert the first/all characters of a string to upper case         *)
      (*--------------------------------------------------------------------*)
      fun toUpperFirst str = 
	 case String.explode str
	   of nil => ""
	    | c::cs => String.implode (Char.toUpper c::cs)
      fun toUpperString str = 
	 String.implode(map Char.toUpper (String.explode str))

      (*--------------------------------------------------------------------*)
      (* return a string representation of an int, char or unit.            *)
      (*--------------------------------------------------------------------*)
      val Int2String = Int.toString
      val Char2String = Char.toString
      fun Unit2String() = "()"
	 
      (*--------------------------------------------------------------------*)
      (* return a string representation of a boolean.                       *)
      (*--------------------------------------------------------------------*)
      fun Bool2xString (t,f) b = if b then t else f
      val Bool2String = Bool2xString ("true","false")
	 
      (*--------------------------------------------------------------------*)
      (* return a string representation of an option.                       *)
      (* the first arg is a string for the NONE case, the second a function *)
      (* that converts x to a string, given a function for doing so.        *)
      (*--------------------------------------------------------------------*)
      fun Option2xString (none,Some2String) x2String opt =
	 case opt 
	   of NONE => none
	    | SOME x => Some2String x2String x
      fun Option2String0 x2String = Option2xString ("",fn f => fn x => f x) x2String
      fun Option2String x2String = Option2xString ("NONE",fn f => fn x => "SOME "^f x) x2String

      (*--------------------------------------------------------------------*)
      (* return a string representation of list; start with pre, separate   *)
      (* with sep and finish with post; use X2String for each element.      *)
      (*--------------------------------------------------------------------*)
      fun List2xString (pre,sep,post) X2String nil = pre^post 
	| List2xString (pre,sep,post) X2String l   = 
	  let fun doit nil    _   = [post]
		| doit (x::r) str = str::X2String x::doit r sep
	  in String.concat (doit l pre)
	  end
      fun List2String X2String nil = "[]"
        | List2String X2String l   =
          let fun doit nil    _   = ["]"]
                | doit (x::r) str = str::X2String x::doit r ","
          in String.concat (doit l "[")
          end
      fun List2String0 X2String nil = ""
        | List2String0 X2String l   =
          let fun doit nil    _   =  nil
                | doit (x::r) str = str::X2String x::doit r " "
          in String.concat (doit l "")
          end

      (* a compiler bug in smlnj 110 makes the following uncompilable: *)
      (* fun List2String X2String xs = List2xString ("[",",","]") X2String xs *)
      (* fun List2String0 X2String xs = List2xString (""," ","") X2String xs *)
   
      (*--------------------------------------------------------------------*)
      (* return a string representation of list; start with pre, separate   *)
      (* with sep and finish with post; use X2String for each element.      *)
      (*--------------------------------------------------------------------*)
      fun Vector2xString (pre,sep,post) X2String vec = 
	 if Vector.length vec=0 then pre^post
	 else String.concat
	    (pre::X2String(Vector.sub(vec,0))::
	     VectorSlice.foldri 
	       (fn (_,x,yet) => sep::X2String x::yet) 
	       [post] 
	       (VectorSlice.slice (vec,1,NONE)))
      fun Vector2String X2String vec = Vector2xString ("#[",",","]") X2String vec
   end
