(*--------------------------------------------------------------------------*)
(* Structure: Entities                                                      *)
(*                                                                          *)
(* Exceptions raised by functions in this structure:                        *)
(*   closeAll         : none                                                *)
(*   getChar          : none                                                *)
(*   getEntId         : none                                                *)
(*   getPos           : none                                                *)
(*   inInternalSubset : none                                                *)
(*   isOpenEntity     : none                                                *)
(*   isSpecialEnd     : none                                                *)
(*   Position2String  : none                                                *)
(*   pushDummy        : none                                                *)
(*   pushExtern       : NoSuchFile                                          *)
(*   pushIntern       : none                                                *)
(*   pushSpecial      : NoSuchFile                                          *)
(*   statePos         : none                                                *)
(*--------------------------------------------------------------------------*)
(* This module maintains the entity stack. For each open entity it holds a  *)
(* buffer to read characters from. When the buffer is exceeded, it gets re- *)
(* filled with new characters, depending on the entity's encoding.          *)
(*                                                                          *)
(****************************************************************************)
(* End-of-line handling as specified in 2.11 is performed:                  *)
(* in XML 1.0 ***************************************************************)
(****************************************************************************)
(*   ... To simplify the tasks of applications, wherever an external parsed *)
(*   entity or the literal entity value of an internal parsed entity        *)
(*   contains either the literal two-character sequence "#xD#xA" or a       *)
(*   standalone literal #xD, an XML processor must pass to the application  *)
(*   the single character #xA.                                              *)
(*   (This behavior can conveniently be produced by normalizing all line    *)
(*   breaks to #xA on input, before parsing.)                               *)
(****************************************************************************)
(* in XML 1.1 ***************************************************************)
(****************************************************************************)
(*   To simplify the tasks of applications, the XML processor MUST behave   *)
(*   as if it normalized all line breaks in external parsed entities        *)
(*   (including the document entity) on input, before parsing,              *)
(*   by translating all of the following to a single #xA character:         *)
(*     1. the two-character sequence #xD #xA                                *)
(*     2. the two-character sequence #xD #x85                               *)
(*     3. the single character #x85                                         *)
(*     4. the single character #x2028                                       *)
(*     5. any #xD character that is not immediately followed by #xA or #x85.*)
(*                                                                          *)
(*   The characters #x85 and #x2028 cannot be reliably recognized and       *)
(*   translated until an entity's encoding declaration (if present) has     *)
(*   been read. Therefore, it is a fatal error to use them within the XML   *)
(*   declaration or text declaration.                                       *)
(****************************************************************************)
(*                                                                          *)
(* It also checks for illegal characters, cf. 2.2:                          *)
(*                                                                          *)
(*   [2] Char ::= #x9 | #xA | #xD       /* any Unicode character,           *)
(*                | [#x20-#xD7FF]          excluding the surrogate          *)
(*                | [#xE000-#xFFFD]        blocks, FFFE, and FFFF. */       *)
(*                | [#x10000-#x10FFFF]                                      *)
(*                                                                          *)
(* More precisely, it assumes that all decoded characters are valid Unicode *)
(* characters. It thus only checks for control characters other than #x9,   *)
(* #xA or #xD.                                                              *)
(*--------------------------------------------------------------------------*)
signature Entities =
   sig
      include Hooks
      
      type State 
      eqtype EntId
      datatype Special = DOC_ENTITY | EXT_SUBSET

      exception CantOpenFile of (string * string) * AppData 

      val pushIntern  : State * int * bool * UniChar.Vector -> State
      val pushExtern  : State * int * bool * Uri.Uri -> State * Encoding.Encoding
      val pushSpecial : Special * Uri.Uri option -> State * Encoding.Encoding

      val closeAll    : State -> unit

      val commitAuto  : AppData * State -> AppData * State
      val changeAuto  : AppData * State * string -> AppData * State * Encoding.Encoding

      val getEntId    : State -> EntId
      val getPos      : State -> Errors.Position
      val getUri      : State -> Uri.Uri

      val getChar     : AppData * State -> UniChar.Char * AppData * State
      val getChar11   : AppData * State -> UniChar.Char * AppData * State
      val getCharRef     : (AppData * State -> UniChar.Char * AppData * State) ref
      val ungetChars  : State * UniChar.Data -> State
 
      val isOpen      : int * bool * State -> bool
      val isSpecial   : State -> bool
      val inDocEntity : State -> bool
   end

functor Entities (structure Hooks : Hooks) : Entities = 
   struct
(*
      structure Entities0=Entities0(structure Hooks = Hooks)
      open Entities0
*)
      open UniChar Decode Decode.Error Errors Hooks Uri UtilError

      val THIS_MODULE = "Entities"
      val BUFSIZE = 1024
      type CharBuffer = UniChar.Char array
         
      (*--------------------------------------------------------------------*)
      (* A special entity can not be popped from the stack by getChar, so   *)
      (* it must be popped explicitly. This is for the document entity and  *)
      (* the external subset.                                               *)
      (*--------------------------------------------------------------------*)
      datatype Special = DOC_ENTITY | EXT_SUBSET
      (*--------------------------------------------------------------------*)
      (* In order to distinguish a general entity from a paramter entity,   *)
      (* entity idxs are marked with this datatype.                         *)
      (*--------------------------------------------------------------------*)
      datatype EntId = GENERAL of int | PARAMETER of int

      (*--------------------------------------------------------------------*)
      (* Make an EntId from the entity's index.                             *)
      (*--------------------------------------------------------------------*)
      fun makeEntId(idx,isParam) = 
	 if isParam then PARAMETER idx else GENERAL idx

      (*--------------------------------------------------------------------*)
      (* A non-empty stack is:                                              *)
      (*                                                                    *)
      (* an internal entity INT(buf,size,idx,(id,other)):                   *)
      (* - (vec,idx,size) is a buffer,current index and its size;           *)
      (* - id is the index of the entity's name in the entity table.        *)
      (* - other contains the underlying entities (the rest of the stack).  *)
      (* The components are nested according to access frequency.           *)
      (*                                                                    *)
      (* an external entity has three forms:                                *)
      (* EXT2(buf,size,idx,line,col,break,(dec,err,typ))                    *)
      (* - (buf,size,idx) is a buffer, its size and current index;          *)
      (* - (line,col) are the line and column;                              *)
      (* - break is a boolean indicating whether the last character was a   *)
      (*   carriage return (0xD) (then a succeeding line feed (0xA) must be *)
      (*   supressed);                                                      *)
      (* - err is an option: if it is SOME(f,ee,err) then it indicates that *)
      (*   the array was finished by a decoding error err, with the basic   *)
      (*   file f; f was at end of file if ee is true. Otherwise there was  *)
      (*   no error when loading the array.                                 *)
      (* - dec describies the encoding of the entity and thus, how more     *)
      (*   data can be loaded;                                              *)
      (* - typ is either of the form SPECIAL spec indicating a special      *)
      (*   entity; then this is the only entity on the stack. Otherwise it  *)
      (*   is NORMAL(id,other) for a normal external entity, with:          *)
      (*   + id is the index of the entity's name in the DTD;               *)
      (*   + other is the underlying stack.                                 *)
      (* The components are nested according to access frequency.           *)
      (*                                                                    *)
      (* The second form of an external entity is                           *)
      (* EXT1(dec,line,col,break,typ). This is an unbuffered                *)
      (* entity whose encoding declaration is being read. We may not load   *)
      (* an array of characters as a whole because the encoding might still *)
      (* change. The components have the same meaning as for EXT2.          *)
      (*                                                                    *)
      (* A closed entity remains on the stack until the next getChar, for   *)
      (* purposes of error printing. A closed external entity has the form  *)
      (* CLOSED(dec,l,col,typ); components have the same meaning            *)
      (* as for open external entities. A closed internal entity has the    *)
      (* form ENDED(id,other) with components as above.                     *)
      (*                                                                    *)
      (* Sometimes (for parsing xml/decl declarations) we need a lookahead. *)
      (* LOOKING(cs,q) is a state remembering all chars cs looked ahead up  *)
      (* to state q, in reverse order. LOOKED(cs,q) is an undone lookahead, *)
      (* the looked-ahead chars now in the right order.                     *)
      (*--------------------------------------------------------------------*)
      datatype ExtType = SPECIAL of Special | NORMAL of EntId * State
      and State = 
	  LOOKED  of Data * State
	| ENDED of EntId * State
	| CLOSED of DecFile * int * int * ExtType
	| INT of Vector * int * int * (EntId * State)
	| EXT1 of DecFile * int * int * bool * ExtType
	| EXT2 of CharBuffer * int * int * int * int * bool
	 * (DecFile * DecodeError option * ExtType)

      exception CantOpenFile of (string * string) * AppData

      (*--------------------------------------------------------------------*)
      (* Extract the unique number from a state.                            *)
      (*--------------------------------------------------------------------*)
      fun getExtEntId extType =
	 case extType 
	   of SPECIAL DOC_ENTITY => GENERAL 0
	    | SPECIAL EXT_SUBSET => PARAMETER 0
	    | NORMAL(id,_) => id
      fun getEntId q =
         case q 
	   of LOOKED (_,q) => getEntId q
	    | ENDED(id,_) => id
            | CLOSED(_,_,_,extType) => getExtEntId extType
            | INT(_,_,_,(id,_)) => id
            | EXT1(_,_,_,_,extType) => getExtEntId extType
            | EXT2(_,_,_,_,_,_,(_,_,extType)) => getExtEntId extType
 
      (*--------------------------------------------------------------------*)
      (* Find the nearest enclosing external entity, and return its         *)
      (* filename, line and column number.                                  *)
      (*--------------------------------------------------------------------*)
      fun getPos q =
         case q 
           of ENDED(_,other) => getPos other
            | INT(_,_,_,(_,other)) => getPos other
            | CLOSED(dec,l,col,_) => (decName dec,l,col)
            | EXT1(dec,l,col,_,_) => (decName dec,l,col)
	    | EXT2(_,_,_,l,col,_,(dec,_,_)) => (decName dec,l,col)
            | LOOKED (cs,q) => let val (f,l,c) = getPos q
	                           val k = length cs 
			       in if c>=k then (f,l,c-k) else (f,l,0)
			       end

      (*--------------------------------------------------------------------*)
      (* get the path of the nearest enclosing external entity.             *)
      (*--------------------------------------------------------------------*)
      fun getUri q =
         case q 
           of LOOKED (_,q) => getUri q
	    | ENDED(_,other) => getUri other
            | INT(_,_,_,(_,other)) => getUri other
            | CLOSED(dec,l,col,_) => decUri dec
            | EXT1(dec,l,col,_,_) => decUri dec
            | EXT2(_,_,_,l,col,_,(dec,_,_)) => decUri dec

      (*--------------------------------------------------------------------*)
      (* close all files, return nothing.                                   *)
      (*--------------------------------------------------------------------*)
      fun closeAll q =
	 case q
	   of LOOKED(_,other) => closeAll other
	    | ENDED(_,other) => closeAll other
	    | CLOSED(_,_,_,SPECIAL _) => ()
	    | CLOSED(_,_,_,NORMAL(_,other)) => closeAll other
	    | INT(_,_,_,(_,other)) => closeAll other
	    | EXT1(dec,_,_,_,SPECIAL _) => ignore(decClose dec)
	    | EXT1(dec,_,_,_,NORMAL(_,other)) => (ignore (decClose dec); closeAll other)
	    | EXT2(_,_,_,_,_,_,(dec,_,SPECIAL _)) => ignore(decClose dec)
	    | EXT2(_,_,_,_,_,_,(dec,_,NORMAL(_,other))) => (ignore (decClose dec); closeAll other)

      (*--------------------------------------------------------------------*)
      (* is this entity already on the stack?                               *)
      (*--------------------------------------------------------------------*)
      fun isOpen (idx,isParam,q) =
	 let val id = makeEntId(idx,isParam)
	    fun doit q =
	       case q 
		 of LOOKED (_,other) => doit other
		  | ENDED(id',other) => id=id' orelse doit other
		  | CLOSED(_,_,_,SPECIAL _) => false
		  | CLOSED(_,_,_,NORMAL(id',other)) => id=id' orelse doit other
		  | INT(_,_,_,(id',other)) => id=id' orelse doit other
		  | EXT1(_,_,_,_,SPECIAL _) => false
		  | EXT1(_,_,_,_,NORMAL(id',other)) => id=id' orelse doit other
		  | EXT2(_,_,_,_,_,_,(_,_,SPECIAL _)) => false
		  | EXT2(_,_,_,_,_,_,(_,_,NORMAL(id',other))) => id=id' orelse doit other
	 in doit q
	 end

      (*--------------------------------------------------------------------*)
      (* are we in the internal subset, i.e., in the document entity?       *)
      (* The internal subset can only be in the document entity, since no   *)
      (* parameter entities are declared prior to it. The document entity   *)
      (* is then the only entity on the stack.                              *)
      (*--------------------------------------------------------------------*)
      fun inDocEntity q = 
         case q 
           of LOOKED (_,q) => inDocEntity q
	    | ENDED(_,other) => inDocEntity other 
            | INT(_,_,_,(_,other)) => inDocEntity other 
            | CLOSED(_,_,_,NORMAL _) => false
            | CLOSED(_,_,_,SPECIAL what) => what=DOC_ENTITY
            | EXT1(_,_,_,_,NORMAL _) => false
            | EXT1(_,_,_,_,SPECIAL what) => what=DOC_ENTITY
            | EXT2(_,_,_,_,_,_,(_,_,NORMAL _)) => false
            | EXT2(_,_,_,_,_,_,(_,_,SPECIAL what)) => what=DOC_ENTITY

      (*--------------------------------------------------------------------*)
      (* is this state the document end, i.e., are all entities closed?     *)
      (*--------------------------------------------------------------------*)
      fun isSpecial q = 
         case q 
           of LOOKED (_,q) => isSpecial q
            | CLOSED(_,_,_,SPECIAL _) => true
            | EXT1(_,_,_,_,SPECIAL _) => true
            | EXT2(_,_,_,_,_,_,(_,_,SPECIAL _)) => true
            | _ => false 

      (*--------------------------------------------------------------------*)
      (* Initialize and load a new buffer when opening an external entity.  *)
      (*--------------------------------------------------------------------*)
      fun initArray dec = 
         let 
            val arr = Array.array(BUFSIZE,0wx0)
            val (n,dec1,err) = decGetArray dec arr
         in (arr,n,dec1,err)
         end

      (*--------------------------------------------------------------------*)
      (* Open an external/internal entity.                                  *)
      (*--------------------------------------------------------------------*)
      fun pushIntern(q,id,isParam,vec) = 
	 INT(vec,Vector.length vec,0,(makeEntId(id,isParam),q))
      fun pushExtern(q,id,isParam,uri) = 
         let 
	    val dec = decOpenXml (SOME uri)
	    val auto = decEncoding dec
	    val q1 = EXT1(dec,1,0,false,NORMAL(makeEntId(id,isParam),q))
	 in (q1,auto)
	 end
      fun pushSpecial(what,uri) = 
         let 
	    val dec = decOpenXml uri
	    val auto = decEncoding dec
	    val q = EXT1(dec,1,0,false,SPECIAL what)
	 in (q,auto)
	 end

      (*--------------------------------------------------------------------*)
      (* confirm the autodetected encoding of an external entity.           *) 
      (*--------------------------------------------------------------------*)
      fun commitAuto(a,q) =
	 case q 
	   of EXT1(dec,l,col,brk,typ) => 
	      let
		 val a1 = a before decCommit dec
		    handle DecError(_,_,err) 
		    => hookError(a,(getPos q,ERR_DECODE_ERROR err))
		 val (arr,n,dec1,err) = initArray dec
	      in (a1,EXT2(arr,n,0,l,col,brk,(dec1,err,typ)))
	      end
(*
	      in (a1,EXT1(dec,l,col,brk,typ))
	      end
*)
	    | LOOKED(cs,q1) => let val (a1,q2) = commitAuto (a,q1)
			       in (a1,LOOKED(cs,q2))
			       end
	    | CLOSED _ => (a,q)
	    | _ => raise InternalError(THIS_MODULE,"commitAuto",
				       "entity is neither EXT1 nor CLOSED nor LOOKED")

      (*--------------------------------------------------------------------*)
      (* change from the autodetected encoding to the declared one.         *) 
      (*--------------------------------------------------------------------*) 
      fun changeAuto (a,q,decl) = 
	 case q 
	   of EXT1(dec,l,col,brk,typ) => 
	      let
		 val dec1 = decSwitch(dec,decl)
		    handle DecError(dec,_,err) 
		    => let val a1 = hookError(a,(getPos q,ERR_DECODE_ERROR err))
			   val _ = decClose dec
			   val uri = decName dec
			   val msg = case err 
				       of ERR_UNSUPPORTED_ENC _ => "Unsupported encoding"
					| _ => "Declared encoding incompatible"
					  ^"with auto-detected encoding"
		       in raise CantOpenFile ((uri,msg),a1)
		       end
		 val newEnc = decEncoding dec1
		 val (arr,n,dec2,err) = initArray dec1
	      in (a,EXT2(arr,n,0,l,col,brk,(dec2,err,typ)),newEnc)
	      end
(*
	      in (a,EXT1(dec1,l,col,brk,typ),newEnc)
	      end
*)

	    | LOOKED(cs,q1) => let val (a2,q2,enc2) = changeAuto(a,q1,decl)
			       in (a2,LOOKED(cs,q2),enc2)
			       end
	    | CLOSED(dec,_,_,_) => (a,q,decEncoding dec)
	    | _ => raise InternalError(THIS_MODULE,"changeAuto",
				       "entity is neither EXT1 nor CLOSED nor LOOKED")



      (*--------------------------------------------------------------------*)
      (* Get one character from the current entity. Possibly reload buffer. *)
      (* Return 0wx0 at entity end. Otherwise check whether the character   *)
      (* is valid (cf. 2.2). If the last character was a carriage return    *)
      (* (0xD) supress a line feed (0xA).                                   *)
      (*--------------------------------------------------------------------*)
      fun getChar (a,q) = 
         case q 
           of ENDED(_,other) => getChar(a,other)
            | CLOSED(_,_,_,typ) => 
	      (case typ 
		 of SPECIAL _ => raise InternalError (THIS_MODULE,"getChar",
						      "attempt to read beyond special entity end")
		  | NORMAL(_,other) => getChar(a,other))
            | INT(vec,s,i,io) => 
              if i>=s then (0wx0,a,ENDED io)
	      else (Vector.sub(vec,i),a,INT(vec,s,i+1,io)) 
            | EXT1(dec,l,col,br,typ) => 
              (let 
		  val (c,dec1) = decGetChar dec
	       in 
		  if (* c>=0wx20 orelse c=0wx09 *)
		     c>=0wx0020 
		     andalso (c<=0wxD7FF 
			      orelse c>=0wxE000 andalso (c<=0wxFFFD 
							 orelse c>=0wx10000))
		     orelse c=0wx9
		     then (c,a,EXT1(dec1,l,col+1,false,typ)) 
		  else 
		    if c=0wxA 
		      then if br then getChar(a,EXT1(dec1,l,col,false,typ))
			   else (c,a,EXT1(dec1,l+1,0,false,typ))
		    else (if c=0wxD then (0wxA,a,EXT1(dec1,l+1,0,true,typ))
			  else let val a1 = hookError(a,(getPos q,ERR_NON_XML_CHAR c))
			       in getChar(a1,EXT1(dec1,l,col+1,false,typ))
			       end)
	       end
		  handle DecEof dec => (0wx0,a,CLOSED(dec,l,col,typ))
		       | DecError(dec,eof,err) => 
				       let val err = ERR_DECODE_ERROR err
					   val a1 = hookError(a,(getPos q,err))
				       in if eof then (0wx0,a,CLOSED(dec,l,col,typ))
					  else getChar(a1,EXT1(dec,col,l,br,typ))
				       end)
            | EXT2(arr,s,i,l,col,br,det) => 
              if i<s 
                 then let val c = Array.sub(arr,i)
                      in if (* c>=0wx20 orelse c=0wx09 *)
			 (* c>=0wx0020 andalso c<=0wxD7FF orelse c=0wx9 orelse *)
			 (* c>=0wxE000 andalso c<=0wxFFFD orelse c>=0wx10000 *)
			 c>=0wx0020 
			 andalso (c<=0wxD7FF 
				  orelse c>=0wxE000 andalso (c<=0wxFFFD 
							     orelse c>=0wx10000))
			 orelse c=0wx9
                            then (c,a,EXT2(arr,s,i+1,l,col+1,false,det)) 
                      else if c=0wxA 
                              then if br then getChar(a,EXT2(arr,s,i+1,l,col,false,det))
                                   else (c,a,EXT2(arr,s,i+1,l+1,0,false,det))
                           else (if c=0wxD then (0wxA,a,EXT2(arr,s,i+1,l+1,0,true,det))
                                 else let val a1 = hookError(a,(getPos q,ERR_NON_XML_CHAR c))
                                      in getChar(a1,EXT2(arr,s,i+1,l,col+1,false,det))
                                      end)
                      end
              else let val (dec,err,typ) = det
		       val (a1,(n,dec1,err1)) = 
			  case err 
			    of NONE => if s=BUFSIZE then (a,decGetArray dec arr)
				       else (a,(0,dec,NONE))
			     | SOME err => (hookError(a,(getPos q,ERR_DECODE_ERROR err)),
					    decGetArray dec arr)
                   in if n=0 andalso not (isSome err1)
			 then (0wx0,a1,CLOSED(dec1,l,col,typ))
                      else getChar(a1,EXT2(arr,n,0,l,col,br,(dec1,err1,typ)))
                   end
            | LOOKED(nil,q) => getChar(a,q)
            | LOOKED(c::cs,q) => (c,a,LOOKED(cs,q))

      fun getChar11 (a,q) = 
         case q 
           of ENDED(_,other) => getChar11(a,other)
            | CLOSED(_,_,_,typ) => 
	      (case typ 
		 of SPECIAL _ => raise InternalError (THIS_MODULE,"getChar11",
						      "attempt to read beyond special entity end")
		  | NORMAL(_,other) => getChar11(a,other))
            | INT(vec,s,i,io) => 
              if i>=s then (0wx0,a,ENDED io)
	      else (Vector.sub(vec,i),a,INT(vec,s,i+1,io)) 
            | EXT1(dec,l,col,br,typ) => (* br = whether the previous char was 0wx0D *)
		(let 
		  val (c,dec1) = decGetChar dec
		in 
		  (* cf 2.2 and 2.11 (end-of-line handling) *)
		  if c>=0wx1 
		    andalso (c<=0wxD7FF orelse c>=0wxE000 andalso (c<=0wxFFFD orelse 
								   c>=0wx10000 andalso c<=0wx10FFFF))
		    then 
		      if c=0wx2028 then (0wxA,a,EXT1(dec1,l+1,0,false,typ)) 
		      else 
			if (c=0wxA orelse c=0wx85) then 
			  if br then getChar11(a,EXT1(dec1,l,col,false,typ)) 
			  (* c and 0wxD was previously translated to 0wxA *)
			  else (0wxA,a,EXT1(dec1,l+1,0,false,typ))
			else if c=0wxD then (* whatever follows a 0wxA must be produced (cf. 2.11) *)
			  (0wxA,a,EXT1(dec1,l+1,0,true,typ))
			     else 
			       if c<0wx7F orelse c>0wx9F then (c,a,EXT1(dec1,l,col+1,false,typ))
			       else (* in XML 1.1 the control characters 0wx7F through 0wx9F must appear
				     only as chracter references *)
				 let 
				   val a1 = hookError(a,(getPos q,ERR_MUST_CHARREF c))
				 in 
				   getChar11(a1,EXT1(dec1,l,col+1,false,typ))
				 end
		  else 
		    let 
		      val a1 = hookError(a,(getPos q,ERR_NON_XML_CHAR c))
		    in 
		      getChar11(a1,EXT1(dec1,l,col+1,false,typ))
		    end
	       end
		  handle DecEof dec => (0wx0,a,CLOSED(dec,l,col,typ))
		       | DecError(dec,eof,err) => 
				       let val err = ERR_DECODE_ERROR err
					   val a1 = hookError(a,(getPos q,err))
				       in if eof then (0wx0,a,CLOSED(dec,l,col,typ))
					  else getChar11(a1,EXT1(dec,col,l,br,typ))
				       end)
            | EXT2(arr,s,i,l,col,br,det) => 
              if i<s then 
		let 
		  val c = Array.sub(arr,i)
		in 
		  (* cf 2.2 and 2.11 *)
		  if c>=0wx1 
		    andalso (c<=0wxD7FF orelse c>=0wxE000 andalso (c<=0wxFFFD orelse 
								   c>=0wx10000 andalso c<=0wx10FFFF))
		    then 
		      if c=0wx2028 then (0wxA,a,EXT2(arr,s,i+1,l+1,0,false,det))
		      else 
			if (c=0wxA orelse c=0wx85) then 
			  if br then getChar11(a,EXT2(arr,s,i+1,l,col,false,det)) 
			  (* c and 0wxD was previously translated to 0wxA *)
			  else (0wxA,a,EXT2(arr,s,i+1,l+1,0,false,det))
			else if c=0wxD then (* whatever follows a 0wxA must be produced (cf. 2.11) *)
			  (0wxA,a,EXT2(arr,s,i+1,l+1,0,true,det))
			     else 
			       if c<0wx7F orelse c>0wx9F then (c,a,EXT2(arr,s,i+1,l,col+1,false,det))
			       else (* in XML 1.1 the control characters 0wx7F through 0wx9F must appear
				     only as chracter references *)
				 let 
				   val a1 = hookError(a,(getPos q,ERR_MUST_CHARREF c))
				 in 
				   getChar11(a1,EXT2(arr,s,i+1,l,col+1,false,det))
				 end
		  else 
		    let 
		      val a1 = hookError(a,(getPos q,ERR_NON_XML_CHAR c))
		    in 
		      getChar(a1,EXT2(arr,s,i+1,l,col+1,false,det))
		    end
		end
              else let val (dec,err,typ) = det
		       val (a1,(n,dec1,err1)) = 
			  case err 
			    of NONE => if s=BUFSIZE then (a,decGetArray dec arr)
				       else (a,(0,dec,NONE))
			     | SOME err => (hookError(a,(getPos q,ERR_DECODE_ERROR err)),
					    decGetArray dec arr)
                   in if n=0 andalso not (isSome err1)
			 then (0wx0,a1,CLOSED(dec1,l,col,typ))
                      else getChar11(a1,EXT2(arr,n,0,l,col,br,(dec1,err1,typ)))
                   end
            | LOOKED(nil,q) => getChar11(a,q)
            | LOOKED(c::cs,q) => (c,a,LOOKED(cs,q))

      val getCharRef = ref getChar

      fun getChar x = !getCharRef x


      (*--------------------------------------------------------------------*)
      (* unget a list of characters.                                        *)
      (*--------------------------------------------------------------------*)
      fun ungetChars (q,cs) = LOOKED(cs,q)

   end
