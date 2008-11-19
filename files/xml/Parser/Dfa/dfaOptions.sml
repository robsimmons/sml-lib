signature DfaOptions = 
   sig
      val O_DFA_INITIAL_WIDTH  : int ref
      val O_DFA_MAX_STATES     : int ref
      val O_DFA_WARN_TOO_LARGE : bool ref
			   
      val setDfaDefaults : unit -> unit
      val setDfaOptions  : Options.Option list * (string -> unit) -> Options.Option list
			   
      val dfaUsage : Options.Usage
   end

functor DfaOptions () : DfaOptions = 
   struct
      open Options UtilInt

      val O_DFA_INITIAL_WIDTH  = ref 4 
      val O_DFA_MAX_STATES     = ref 256
      val O_DFA_WARN_TOO_LARGE = ref true

      fun setDfaDefaults() = 
	 let 
	    val _ = O_DFA_INITIAL_WIDTH   := 4
	    val _ = O_DFA_MAX_STATES      := 256
	    val _ = O_DFA_WARN_TOO_LARGE  := true
	 in ()
	 end

      val dfaUsage = 
	 [U_ITEM(["--dfa-initial-size=n"],"Initial size of DFA transition tables (16)"),
   	  U_ITEM(["--dfa-initial-width=n"],"Same as --dfa-initial-size=2^n (4)"),
	  U_ITEM(["--dfa-max-size=n"],"Maximal size of DFAs for ambiguous content models (256)"),
	  U_ITEM(["--dfa-warn-size[=(yes|no)]"],"Warn about too large DFAs (yes)")
	  ]

      fun setDfaOptions(opts,doError) = 
	 let 
            exception Failed of string option

            fun getNat str = 
	       if str="" then raise Failed NONE
	       else let val cs = String.explode str
		    in foldl (fn (c,n) => if #"0">c orelse #"9"<c then raise Failed NONE
					  else 10*n+ord c-48) 0 cs
		       handle Overflow => raise Failed
			  (SOME("number "^str^" is too large for this system"))
		    end
		 
	    val yesNo = "'yes' or 'no'"
	    fun tooLarge n = String.concat ["number ",n," is too large for this system"]
            fun mustHave key = String.concat ["option --",key," must have an argument"]
	    fun mustBe key what = String.concat 
	       ["the argument to option --",key," must be ",what]

	    fun do_yesno(key,valOpt,flag) =
	       case valOpt 
		 of NONE => flag := true
		  | SOME "yes" => flag := true
		  | SOME "no" => flag := false
		  | SOME s => doError (mustBe key yesNo)

	    fun do_num(key,valOpt,flag) =
	       case valOpt 
		 of NONE => doError (mustHave key)
		  | SOME s => flag := getNat s
		    handle Failed NONE => doError (mustBe key "a number")
			 | Failed (SOME s) => doError s

	    fun do_dfa_ts(key,valOpt,toWidth) =
	       case valOpt 
		 of NONE => doError (mustHave key)
		  | SOME s => O_DFA_INITIAL_WIDTH := toWidth (getNat s)
		    handle Failed NONE => doError (mustBe key "a number")
			 | Failed (SOME s) => doError s

	    fun do_long(key,valOpt) = 
	       case key
		 of "dfa-initial-size" => true before do_dfa_ts(key,valOpt,nextPowerTwo)
		  | "dfa-initial-width" => true before do_dfa_ts(key,valOpt,fn i => i)
		  | "dfa-max-size" => true before do_num(key,valOpt,O_DFA_MAX_STATES)
		  | "dfa-warn-size" => true before do_yesno(key,valOpt,O_DFA_WARN_TOO_LARGE)
		  | _ => false

	    and doit nil = nil
	      | doit (opt::opts) =
	       case opt
		 of OPT_NOOPT => opts
		  | OPT_LONG(key,value) => if do_long(key,value) then doit opts 
					   else opt::doit opts
		  | OPT_NEG _ => opt::doit opts
		  | OPT_SHORT _ => opt::doit opts
		  | OPT_STRING _ => opt::doit opts
	 in doit opts
	 end
   end

      
