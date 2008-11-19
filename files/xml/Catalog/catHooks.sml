signature CatHooks =
   sig
      type AppData = CatData.CatEntry list 

      val initCatHooks : unit -> AppData
   end

functor CatHooks (structure Params : CatParams
		  structure Dtd    : CatDtd   ) =
   struct
      open 
	 Dtd HookData IgnoreHooks Params UniChar UniClasses Uri UtilList
	 CatData CatError

      type AppData = Dtd * CatEntry list 
      type AppFinal = CatEntry list

      fun initCatHooks dtd = (dtd,nil)

      fun hookError (a,(pos,err)) = a before catError (pos,ERR_XML err)
	 
      fun getAtt dtd (pos,elem,att,trans) atts = 
	 let
	    val cvOpt = findAndMap 
	       (fn (i,ap,_) => if i<>att then NONE
			       else case ap 
				      of AP_DEFAULT(_,cv,_) => SOME cv
				       | AP_PRESENT(_,cv,_) => SOME cv
				       | _ => NONE)
	       atts
	 in case cvOpt 
	      of SOME cv => trans (pos,att) cv
	       | NONE => NONE before catError
		 (pos,ERR_MISSING_ATT(Index2Element dtd elem,Index2AttNot dtd att))
	 end

      fun makePubid dtd (pos,att) cv = 
	 let val (cs,bad) = 
	    Vector.foldr
	    (fn (c,(cs,bad)) => if isPubid c then (Char2char c::cs,bad)
				else (cs,c::bad)) 
	    (nil,nil) cv
	 in if null bad then SOME(String.implode cs)
	    else NONE before catError(pos,ERR_NON_PUBID(Index2AttNot dtd att,bad))
	 end

      fun makeUri (pos,att) cv = SOME cv
  
      fun hookStartTag (a as (dtd,items),((_,pos),elem,atts,_,_)) = 
	 if elem=baseIdx 
	    then let val hrefOpt = getAtt dtd (pos,elem,hrefIdx,makeUri) atts
		 in case hrefOpt
		      of NONE => a
		       | SOME href => (dtd,E_BASE (Vector2Uri href)::items)
		 end
	 else if elem=delegateIdx
		 then let val hrefOpt = getAtt dtd (pos,elem,hrefIdx,makeUri) atts
			  val pubidOpt = getAtt dtd (pos,elem,pubidIdx,makePubid dtd) atts
		      in case (hrefOpt,pubidOpt)
			   of (SOME href,SOME pubid) => 
			      (dtd,E_DELEGATE(pubid,Vector2Uri href)::items)
			    | _ => a
		      end
	 else if elem=extendIdx
		 then let val hrefOpt = getAtt dtd (pos,elem,hrefIdx,makeUri) atts
		      in case hrefOpt
			   of NONE => a
			    | SOME href => (dtd,E_EXTEND (Vector2Uri href)::items)
		      end
	 else if elem=mapIdx
		 then let val hrefOpt = getAtt dtd (pos,elem,hrefIdx,makeUri) atts
			  val pubidOpt = getAtt dtd (pos,elem,pubidIdx,makePubid dtd) atts
		      in case (hrefOpt,pubidOpt)
			   of (SOME href,SOME pubid) => 
			      (dtd,E_MAP(pubid,Vector2Uri href)::items)
			    | _ => a
		      end
	 else if elem=remapIdx
		 then let val hrefOpt = getAtt dtd (pos,elem,hrefIdx,makeUri) atts
			  val sysidOpt = getAtt dtd (pos,elem,sysidIdx,makeUri) atts
		      in case (hrefOpt,sysidOpt)
			   of (SOME href,SOME sysid) => 
			      (dtd,E_REMAP(Vector2Uri sysid,Vector2Uri href)::items)
			    | _ => a
		      end
	      else a
		 
      fun hookFinish (_,items) = rev items
   end
