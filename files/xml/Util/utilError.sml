




signature UtilError = 
   sig
      exception InternalError of string * string * string
      exception NoSuchFile of string * string

      val formatMessage : int * int -> string list -> string 
   end

structure UtilError : UtilError = 
   struct
      open UtilString

      exception InternalError of string * string * string
      exception NoSuchFile of string * string

      fun formatMessage (indentWidth,lineWidth) strs =
	 let 
	    val indent = nBlanks indentWidth
	    val nl = "\n"^indent
	    val blank = " "
	    val dot = "." 

	    fun isSep c = #" "=c orelse #"\n"=c orelse #"\t"=c

	    fun go (w,yet) nil = List.rev ("\n"::yet)
	      | go (w,yet) (x::xs) = 
	       let 
		  val y = if null xs then x^dot else x 
		  val l = String.size y
		  val w1 = w+l
		  val (w2,yet2) = if w1<=lineWidth then (w1,y::yet)
				  else (indentWidth+l,y::nl::yet)
		  val (w3,yet3) = if null xs then (w2,yet2)
				  else (if w2<lineWidth then (w2+1,blank::yet2)
					else (indentWidth,nl::yet2))
	       in go (w3,yet3) xs
	       end
	    
	    val tokens = List.concat (map (String.tokens isSep) strs)
	    val fragments = go (0,nil) tokens
	 in 
	    String.concat fragments
	 end
   end
