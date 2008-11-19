signature ParseBase = 
   sig 
      include Dfa DtdManager Resolve DfaOptions ParserOptions

      exception NoSuchChar of AppData * State
      exception NoSuchEntity of AppData * State
      exception NotFound of UniChar.Char * AppData * State
      exception SyntaxError of UniChar.Char * AppData * State
      
      val expectedOrEnded : Errors.Expected * Errors.Location -> UniChar.Char -> Errors.Error

      val recoverXml  : UniChar.Char * AppData * State -> UniChar.Char * AppData * State
      val recoverETag : UniChar.Char * AppData * State -> UniChar.Char * AppData * State
      val recoverSTag : UniChar.Char * AppData * State -> bool * (UniChar.Char * AppData * State)
      val recoverDecl : bool -> UniChar.Char * AppData * State -> (UniChar.Char * AppData * State)

      val useParamEnts : unit -> bool
   end

(*--------------------------------------------------------------------------*)
(* Structure: ParseBase                                                     *)
(*--------------------------------------------------------------------------*)
(* This structure provides exceptions for the Parse functions, and strings  *)
(* for error generation (these strings don't really need to reside in their *)
(* own structure, but like this the code is more easier to read).           *)
(*--------------------------------------------------------------------------*)
functor ParseBase (structure Dtd           : Dtd
		   structure Hooks         : Hooks
		   structure Resolve       : Resolve
		   structure ParserOptions : ParserOptions) : ParseBase =
   struct
      structure DfaOptions = ParserOptions.DfaOptions
      structure Dfa = Dfa (structure DfaOptions = DfaOptions)
      structure DtdManager = DtdManager (structure Dtd = Dtd
					 structure Hooks = Hooks
					 structure ParserOptions = ParserOptions)
      open 
	 Base DtdManager DfaOptions Dfa Errors ParserOptions Resolve UniChar
	 
      exception NoSuchChar of AppData * State
      exception NoSuchEntity of AppData * State
      exception NotFound of UniChar.Char * AppData * State
      exception SyntaxError of UniChar.Char * AppData * State
      
      fun expectedOrEnded (exp,ended) c =
	 if c=0wx00 then ERR_ENDED_BY_EE ended
	 else ERR_EXPECTED(exp,[c])

   (*--------------------------------------------------------------------*)
   (* Besides "?>" also recognize ">" as end delimiter, because the typo *)
   (* might be an omitted "?". Also stop on "<"; then the entire "?>"    *)
   (* was omitted; the "<" may not be consumed then.                     *)
   (* Within literals dont recognize ">" and "<", but only "?>"; then    *)
   (* the typo is an omitted quote character.                            *)
   (*--------------------------------------------------------------------*)
   fun recoverXml caq =
      let 
	 fun do_lit ch (c,a,q) =
	    case c
	      of 0wx00 => (c,a,q)
	       | 0wx3F (* #"?" *) => 
		 let val (c1,a1,q1) = getChar (a,q)
		 in if c1=0wx3E (* #">" *) then (c1,a1,q1)
		    else do_lit ch (c1,a1,q1)
		 end
	       | _ => if c=ch then (getChar (a,q))
		      else do_lit ch (getChar (a,q))
	 fun doit (c,a,q) = 
	    case c
	      of 0wx00 => (c,a,q)
	       | 0wx22 (* #""""*) => doit (do_lit c (getChar (a,q)))
	       | 0wx25 (* #"%" *) => (c,a,q)
	       | 0wx26 (* #"&" *) => (c,a,q)
	       | 0wx27 (* #"'" *) => doit (do_lit c (getChar (a,q)))
	       | 0wx3C (* #"<" *) => (c,a,q)
	       | 0wx3E (* #">" *) => (getChar (a,q))
	       | _ => doit (getChar (a,q))
      in 
	 doit caq
      end
   
   fun recoverETag caq =
      let 
	 fun do_lit ch (c,a,q) =
	    case c
	      of 0wx00 => (c,a,q)
	       | _ => if c=ch then (getChar (a,q))
		      else do_lit ch (getChar (a,q))
	 fun doit (c,a,q) = 
	    case c
	      of 0wx00 => (c,a,q)
	       | 0wx22 (* #""""*) => doit (do_lit c (getChar (a,q)))
	       | 0wx26 (* #"&" *) => (c,a,q)
	       | 0wx27 (* #"'" *) => doit (do_lit c (getChar (a,q)))
	       | 0wx3E (* #">" *) => (getChar (a,q))
	       | 0wx3C (* #"<" *) => (c,a,q)
	       | _ => doit (getChar (a,q))
      in 
	 doit caq
      end
   
   fun recoverSTag caq =
      let 
	 fun do_lit ch (c,a,q) =
	    case c
	      of 0wx00 => (c,a,q)
	       | _ => if c=ch then (getChar (a,q))
		      else do_lit ch (getChar (a,q))
	 fun doit (c,a,q) = 
	    case c
	      of 0wx00 => (false,(c,a,q))
	       | 0wx22 (* #""""*) => doit (do_lit c (getChar (a,q)))
	       | 0wx26 (* #"&" *) => (false,(c,a,q))
	       | 0wx27 (* #"'" *) => doit (do_lit c (getChar (a,q)))
	       | 0wx2F (* #"/" *) => let val (c1,a1,q1) = getChar (a,q)
				     in if c1=0wx3E (* #">" *) then (true,(c1,a1,q1))
					else doit (c1,a1,q1)
				     end
	       | 0wx3E (* #">" *) => (false,getChar (a,q))
	       | 0wx3C (* #"<" *) => (false,(c,a,q))
	       | _ => doit (getChar (a,q))
      in 
	 doit caq
      end
   
   fun recoverDecl hasSubset caq = 
      let 
	 fun do_lit ch (c,a,q) =
	    if c=0wx00 then (c,a,q)
	    else if c=ch then getChar (a,q)
		 else do_lit ch (getChar(a,q))
	 fun do_decl (c,a,q) =
	    case c
	      of 0wx00 => (c,a,q)
	       | 0wx22 (* #"\""*) => do_decl (do_lit c (getChar (a,q)))
	       | 0wx27 (* #"'" *) => do_decl (do_lit c (getChar (a,q)))
	       | 0wx3E (* #">" *) => getChar (a,q)
	       | _ => do_decl (getChar (a,q))
         fun do_subset (c,a,q) =
	    case c
	      of 0wx00 => (c,a,q)
	       | 0wx3C (* #"<" *) => do_subset (do_decl (getChar (a,q)))
	       | 0wx5D (* #"]" *) => getChar (a,q)
	       | _ => do_subset (getChar (a,q))
	 fun doit (c,a,q) =
	    case c
	      of 0wx00 => if isSpecial q then (c,a,q) else doit (getChar (a,q))
	       | 0wx22 (* #"\""*) => doit (do_lit c (getChar (a,q)))
	       | 0wx25 (* #"%" *) => if hasSubset then (c,a,q) else doit (getChar (a,q))
	       | 0wx27 (* #"'" *) => doit (do_lit c (getChar (a,q)))
	       | 0wx3C (* #"<" *) => (c,a,q)
	       | 0wx3E (* #">" *) => getChar (a,q)
	       | 0wx5B (* #"[" *) => if hasSubset then doit (do_subset (getChar (a,q)))
				     else doit (getChar (a,q))
	       | _ => doit (getChar (a,q))
      in doit caq
      end

      fun useParamEnts() = !O_VALIDATE orelse !O_INCLUDE_PARAM_ENTS
   end
