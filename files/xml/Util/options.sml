signature Options=
   sig
      datatype Option =
	 OPT_LONG of string * string option
       | OPT_SHORT of char list
       | OPT_NEG of char list
       | OPT_NOOPT
       | OPT_STRING of string
      val parseOptions : string list -> Option list

      datatype UsageItem =
         U_SEP
       | U_TITLE of string
       | U_ITEM  of string list * string
      type Usage = UsageItem list
      val printUsage   : TextIO.outstream -> string -> Usage -> unit
   end

structure Options : Options =
   struct
      exception BadOption of string
	    
      datatype Option =
	 OPT_LONG of string * string option
       | OPT_SHORT of char list
       | OPT_NEG of char list
       | OPT_NOOPT
       | OPT_STRING of string

      datatype UsageItem =
         U_SEP
       | U_TITLE of string
       | U_ITEM  of string list * string
      type Usage = UsageItem list

      fun parseOptions ss =
	 let 
	    fun doOne opt =
	       if String.isPrefix "--" opt 
		  then let val opt1 = Substring.extract(opt,2,NONE) 
			   val (key0,opt2) = Substring.splitl (fn c => #"="<>c) opt1
			   val key = if Substring.isEmpty key0 then raise BadOption opt 
				     else Substring.string key0
			   val valOpt = if Substring.isPrefix "=" opt2
					   then let val val0 = Substring.triml 1 opt2
						in if Substring.isEmpty val0 
						      then raise BadOption opt
						   else SOME(Substring.string val0)
						end
					else NONE
		       in OPT_LONG(key,valOpt)
		       end
		    handle BadOption s => if opt="--" then OPT_NOOPT else OPT_STRING opt
	       else if String.isPrefix "-" opt 
		       then let val chars = tl(String.explode opt)
			    (* val _ = app (fn c => if Char.isAlphaNum c then () 
		                                    else raise BadOption opt) chars *)
			    in case chars
				 of nil => OPT_STRING opt
				  | #"n"::(cs as _::_) => OPT_NEG cs
				  | _ => OPT_SHORT chars
			    end
			 handle BadOption s => OPT_STRING opt
		    else OPT_STRING opt
		       
	    fun doAll nil = nil
	      | doAll (s::ss) = let val opt = doOne s
				in case opt
				     of OPT_NOOPT => opt::map OPT_STRING ss
				      | _ => opt::doAll ss
				end
	 in doAll ss
	 end

      fun printUsage stream prog usage = 
	 let 
            val KEY_WIDTH = 30
            val LINE_WIDTH = 80
            val EMPTY_KEY = UtilString.nBlanks KEY_WIDTH
            fun appendKeys col nil = if col>KEY_WIDTH then "\n"^EMPTY_KEY
                                     else UtilString.nBlanks (KEY_WIDTH-col)
              | appendKeys col [key] = key^" "^appendKeys (col+1+String.size key) nil
              | appendKeys col (key::keys) = let val col1 = col+2+String.size key
                                             in if col1>KEY_WIDTH 
                                                   then key^",\n"^appendKeys 0 keys
                                                else key^", "^appendKeys col1 keys
                                             end
            fun makeKey keylist = appendKeys 0 keylist
            val makeText = UtilString.breakLines(LINE_WIDTH-KEY_WIDTH)
	    fun format (keylist,text) = 
               let val key = makeKey keylist
               in case makeText text
                    of nil => [key]
                     | line::lines => key^line::map (fn line => EMPTY_KEY^line) lines
               end
            val _ = app (fn x => TextIO.output(stream,x))
	       ["Usage: ",prog," [option ...] file\n","where option is one of:\n\n"]
	    val _ = app (fn item => app (fn x => TextIO.output(stream,x^"\n")) 
                         (case item 
                            of U_SEP => [""]
                             | U_TITLE txt => ["",txt]
                             | U_ITEM option => format option)) usage
	 in ()
	 end
   end      
