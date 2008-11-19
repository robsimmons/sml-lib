structure TestClasses =
   struct
      open UniChar UniClasses UniRanges UtilList 

      fun wordUpto(n,m) = if Word.<(m,n) then nil else n::wordUpto(Word.+(n,0w1),m)
      fun inCharRange(c:Char,cr) = List.exists (fn (lo,hi) => lo<=c andalso hi>=c) cr

      fun testClass(className,isClass,classRange,stop) =
	 let 
	    val _ = print ("Testing class "^className^":")
	    fun testOne c = if isClass c = inCharRange(c,classRange) then () 
			    else let val str = String.concat
			       ["TestClasses: ",Char2Uni c,
				if isClass c then " may not " else " must ",
				   "be in this class."]
				     val _ = print (str^"\n")
				 in raise Fail str
				 end
			      handle exn => 
				 let val str = String.concat
				    ["TestClasses: Unexpected exception for ",
				     Chars.toString c,":",exnMessage exn]
				     val _ = print (str^"\n")
				 in raise Fail str
				 end
	    fun debugOne c = if c mod 0wx40 <> 0wx0 then ()
			     else if c mod 0wx1000 <> 0wx00 then print "."
				  else print "\n."
	    fun testAll c = if c>=stop then () 
			    else (debugOne c; testOne c; testAll (c+0w1))
	    val _ = testAll 0w0
	    val _ = print "\n"
	 in ()
	 end

      fun testClasses() =
	 let 
	    val _ = testClass("name start",isNms,nmsRange,0wx10010)
	    val _ = testClass("name",isName,nameRange,0wx10010)
	    val _ = testClass("pubid",isPubid,pubidRange,0wx100)
	    val _ = testClass("enc",isEnc,encRange,0wx100)
	 in ()
	 end

      (* val _ = testClasses() *)
   end
