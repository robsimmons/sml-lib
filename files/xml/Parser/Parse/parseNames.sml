






signature ParseNames =  
   sig
      include ParseBase

      val parseName    : UniChar.Char * AppData * State 
	 -> UniChar.Data * (UniChar.Char * AppData * State)
      val parseNmtoken : UniChar.Char * AppData * State 
         -> UniChar.Data * (UniChar.Char * AppData * State)

      val parseNameLit : UniChar.Data -> UniChar.Char * AppData * State
         -> UniChar.Data * UniChar.Data * (UniChar.Char * AppData * State)
      val parseEntName : UniChar.Data * UniChar.Data -> UniChar.Char * AppData * State 
         -> bool * UniChar.Data * UniChar.Data * (UniChar.Char * AppData * State)
   end

(*--------------------------------------------------------------------------*)
(* Structure: ParseNames                                                    *)
(*                                                                          *)
(* Exceptions raised by functions in this structure:                        *)
(*   parseEntName  : none                                                   *)
(*   parseName     : NotFound                                               *)
(*   parseNmtoken  : NotFound                                               *)
(*--------------------------------------------------------------------------*)
functor ParseNames (structure ParseBase : ParseBase) 
   : ParseNames = 
struct
   open  
      Errors ParseBase UniClasses
         
      (*--------------------------------------------------------------------*)
      (* parse (the remainder of) a name or nmtoken.                        *)
      (*                                                                    *)
      (*   [5]     Name ::= (Letter | '_' | ':') (NameChar)*                *) 
      (*                                                                    *)
      (* raise NotFound if no name/name start character comes first.        *)   
      (*                                                                    *)
      (* return the name as a list of characters, together with the next    *)
      (* character and the remaining state.                                 *)
      (*--------------------------------------------------------------------*)
      (* XML 1.1                                                            *)
      (*   [5]     Name	::=  NameStartChar (NameChar)*                      *)
      (*--------------------------------------------------------------------*)
      fun parseName' (c,a,q) = 
	 if isName c 
	    then let val (cs,caq1) = parseName'(getChar(a,q))
		 in (c::cs,caq1)
		 end
	 else (nil,(c,a,q))
      fun parseName (c,a,q) = 
         if isNms c 
            then let val (cs,caq1) = parseName'(getChar(a,q))
                 in (c::cs,caq1)
                 end
         else raise NotFound(c,a,q)
      fun parseNmtoken (c,a,q) = 
         if isName c 
            then let val (cs,caq1) = parseName'(getChar(a,q))
                 in (c::cs,caq1)
                 end
         else raise NotFound(c,a,q)

      (*--------------------------------------------------------------------*)
      (* parse a name, additionally accumulating its characters in reverse  *)
      (* order to the first argument.                                       *)
      (*                                                                    *)
      (* raise NotFound if no name/name start character comes first.        *)   
      (*--------------------------------------------------------------------*)
      fun parseNameLit cs (c,a,q) = 
         let fun doit (cs,ns) (c,a,q) =
            if isName c then doit (c::cs,c::ns) (getChar(a,q)) 
            else (cs,rev ns,(c,a,q))
         in 
            if isNms c then doit (c::cs,[c]) (getChar(a,q)) 
            else raise NotFound(c,a,q)
         end
      (*--------------------------------------------------------------------*)
      (* parse a name, accumulating its reverse in the first arg text. This *)
      (* is useful for parsing of entity values, where entity references    *)
      (* are parsed but bypassed, and must thus be accumulated together     *)
      (* the other literal text.                                            *)
      (*                                                                    *)
      (* print an error if no name/name start character comes first.        *)   
      (*                                                                    *)
      (* return a boolean indicating whether a name was found, the reverse  *)
      (* name as a list of characters, concatenated with the text in the    *)
      (* first arg, together with the next character and remaining state.   *)
      (*--------------------------------------------------------------------*)
      fun parseEntName (lit,text) (c,a,q) =
         let 
            fun doit (lit,text) (c,a,q) = 
               if isName c then doit (c::lit,c::text) (getChar (a,q))
               else (true,lit,text,(c,a,q))
         in 
            if isNms c then doit (c::lit,c::text) (getChar (a,q))
            else let val a1 = hookError(a,(getPos q,ERR_EXPECTED(expAnEntName,[c])))  
                 in (false,lit,text,(c,a1,q)) 
                 end
         end
            
   end

