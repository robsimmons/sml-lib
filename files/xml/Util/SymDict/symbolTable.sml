





(*--------------------------------------------------------------------------*)
(* Functor: SymbolTable                                                     *)
(*                                                                          *)
(* Exceptions raised by functions in this structure:                        *)
(*   getSymIndex   : Key.InternalError                                      *)
(*   getSymKey     : NoSuchSymbol                                           *)
(*   hasSymIndex   : none                                                   *)
(*   makeSymTable  : none                                                   *)
(*   nullSymTable  : none                                                   *)
(*   printSymTable : none                                                   *)
(*   usedSymbols   : none                                                   *)
(*--------------------------------------------------------------------------*)
(* A symbol table maps Keys to consecutive integers.                        *)
(*--------------------------------------------------------------------------*)
signature SymTable =
   sig
      type Key
      type SymTable
	 
      exception NoSuchSymbol

      val nullSymTable  : string -> SymTable
      val makeSymTable  : string * int -> SymTable
      val clearSymTable : SymTable * int option -> unit
	 
      val hasSymIndex   : SymTable * Key -> int option
      val getSymIndex   : SymTable * Key -> int
      val getSymKey     : SymTable * int -> Key
      val usedSymbols   : SymTable -> int

      val assignSymIndex  : SymTable * Key * int -> unit
      val reserveSymIndex : SymTable -> int
	 
      val extractSymTable : SymTable -> Key vector
      val printSymTable   : SymTable -> unit
   end

functor SymTable (structure Key : Key) : SymTable =
   struct
      open UtilError UtilInt
      
      exception NoSuchSymbol

      type Key = Key.Key

      (*--------------------------------------------------------------------*)
      (* a symbol table can have at most size MAX_WIDTH. This is because    *)
      (* arrays may at most have Array.maxLen elements. We only use powers  *)
      (* of two as sizes, so we are really only interested in the position  *)
      (* of maxLen's highest bit. That would be the maximal width for hash  *)
      (* tables, and thus we must decrease it by one for obtaining the max  *)
      (* table width.                                                       *)
      (*--------------------------------------------------------------------*)
      fun highestBit w = if w=0w0 then 0 else 1+highestBit(Word.>>(w,0w1))
      val MAX_WIDTH = highestBit (Word.fromInt Array.maxLen)-1

      type Bucket = (Key * int) list
      val nullBucket = nil : Bucket

      (*--------------------------------------------------------------------*)
      (* buckets are sorted - though they are probably small.               *)
      (*--------------------------------------------------------------------*)
      fun addToBucket (ni as (key,_),bucket) = 
	 let 
	    fun doit nil = [ni]
	      | doit (nis as (ni' as (key',_))::rest) = 
	       case Key.compare (key',key)
		 of LESS    => ni'::doit rest
		  | EQUAL   => ni::rest 
		  | GREATER => ni::nis
	 in 
	    doit bucket
	 end
      fun searchBucket (key,bucket) = 
	 let 
	    fun doit nil = NONE
	      | doit ((key',i)::rest) = 
	       case Key.compare (key',key)
		 of LESS    => doit rest
		  | EQUAL   => SOME i 
		  | GREATER => NONE
	 in 
	    doit bucket
	 end
		
      (*--------------------------------------------------------------------*)
      (* a symbol table consists of                                         *)
      (* - an array tab holding for each index its key                      *)
      (* - a hash table, i.e. Bucket array, of double size than tab         *)
      (* - a hashFun mapping Key to the range of the hash table             *)
      (* - an integer width for computing table sizes                       *)
      (* - an integer size wich is the size of the value table              *)
      (* - an integer count holding the next free index                     *)
      (*--------------------------------------------------------------------*)
      type SymTable = {desc    : string,
		       tab     : Key array ref,
		       hash    : Bucket array ref,
		       hashFun : (Key -> int) ref,
		       width   : int ref, (* bit width *)
		       size    : int ref, (* tab size=2^width, hash size is double *) 
		       count   : int ref  (* number of entries *)
		       }

      fun nullSymTable desc = {desc    = desc,
			       tab     = ref (Array.array(1,Key.null)),
			       hash    = ref (Array.array(2,nullBucket)),
			       hashFun = ref (fn _ => 0),
			       count   = ref 0,
			       size    = ref 1,
			       width   = ref 0} : SymTable

      (*--------------------------------------------------------------------*)
      (* how many entries are in the symtable?                              *)
      (*--------------------------------------------------------------------*)
      fun usedSymbols ({count,...}:SymTable) = !count
	 
      (*--------------------------------------------------------------------*)
      (* what is the table load, i.e. percentage of number of entries to    *)
      (* hash table size = 100*count/(2*size) = 50*count/size.              *)
      (*--------------------------------------------------------------------*)
      fun hashRatio({count,size,...}:SymTable) = 50 * !count div !size
	 handle Div => 100

      (*--------------------------------------------------------------------*)
      (* this is the hash function. Key.hash hashes data to arbitrary       *)
      (* words, that are mapped to the hash range by this function, where   *)
      (* mask is the bitmask corresponding to the size of the hash table:   *)
      (*   1. square the word produced by Key.hash                          *)
      (*   2. take the width bits from the middle of the square, these are  *)
      (*      the bit-places influenced by all input bit-places:            *)
      (*      - shift to the right by half of the destination width         *)
      (*      - mask out all bits to the left of destination                *)
      (* this is a simple strategy but experiences good results.            *)
      (*--------------------------------------------------------------------*)
      fun square (x:word) = Word.*(x,x)
      fun hashKey(half,mask) x = 
	 Word.toInt(Word.andb(mask,Word.>>(square(Key.hash x),half)))
      fun makeHashFun(size,width) =
	 let 
            val mask = Word.fromInt(2*size-1) 
            val half = Word.fromInt((width+1) div 2)
	 in 
	    hashKey(half,mask)
	 end
	    
      (*--------------------------------------------------------------------*)
      (* create a new symtable for 2^w, but at least 2 and at most 2^m      *)
      (* entries, where m is the value of MAX_WIDTH.                        *)
      (*--------------------------------------------------------------------*)
      fun makeSymTable (desc,w) = 
	 let 
	    val width= Int.min(Int.max(1,w),MAX_WIDTH)
	    val size = Word.toInt(Word.<<(0w1,Word.fromInt(width-1)))
	 in {desc    = desc,
	     tab     = ref (Array.array(size,Key.null)),
	     hash    = ref (Array.array(2*size,nullBucket)),
	     hashFun = ref (makeHashFun(size,width)),
	     width   = ref width,
	     size    = ref size,
	     count   = ref 0}
	 end
      
      (*--------------------------------------------------------------------*)
      (* clear a dictionary. If the 2nd arg is SOME w, use w for resizing.  *)
      (*--------------------------------------------------------------------*)
      fun clearSymTable (symTab:SymTable,widthOpt) = 
	 case widthOpt 
	   of NONE => 
	      let 
		 val {tab=ref tab,hash=ref hash,size,count,...} = symTab
		 val _ = appInterval (fn i => Array.update(tab,i,Key.null)) (0,!count-1)
		 val _ = appInterval (fn i => Array.update(hash,i,nullBucket)) (0,!size*2-1)
	      in 
		 count := 0
	      end
	    | SOME w => 
	      let 
		 val {tab,hash,hashFun,width,size,count,...} = symTab
		 val newWidth = Int.min(Int.max(1,w),MAX_WIDTH)   
		 val newSize  = Word.toInt(Word.<<(0w1,Word.fromInt(newWidth-1)))
		 val _ = tab     := (Array.array(newSize,Key.null))
		 val _ = hash    := (Array.array(2*newSize,nullBucket))
		 val _ = hashFun := (makeHashFun(newSize,newWidth))
		 val _ = width   := newWidth
		 val _ = size    := newSize
	      in
		 count := 0
	      end

      (*--------------------------------------------------------------------*)
      (* grow a symtable to the double size. raise InternalError if the     *)
      (* table already has maximal size.                                    *)
      (*--------------------------------------------------------------------*)
      fun growTable ({desc,tab,hash,hashFun,width,size,count}:SymTable) = 
	 let 
	    val newWidth = if !width < MAX_WIDTH then !width+1 
			   else raise InternalError 
			      ("SymTable","growTable",
			       String.concat ["growing the ",desc," symbol table ",
					      "exceeded the system maximum size of ",
					      Int.toString Array.maxLen," for arrays"]) 
	    val newSize  = !size*2
	       
	    val oldTab = !tab
	    val newTab = Array.array(newSize,Key.null)
	    val newHash = Array.array(2*newSize,nullBucket)
	    val newHashFun = makeHashFun(newSize,newWidth)

	    fun addToNew (inv as (i,key)) = 
	       let 
		  val idx = newHashFun key
		  val _ = Array.update(newHash,idx,addToBucket((key,i),Array.sub(newHash,idx)))
		  val _ = Array.update(newTab,i,key)
	       in ()
	       end
	    val _ = ArraySlice.appi addToNew (ArraySlice.slice (!tab,0,NONE))

	    val _ = tab   := newTab
	    val _ = hash  := newHash
	    val _ = size  := newSize
	    val _ = width := newWidth
	    val _ = hashFun := newHashFun
	 in ()
	 end

      (*--------------------------------------------------------------------*)
      (* lookup the key for an index of the symbol table.                   *)
      (*--------------------------------------------------------------------*)
      fun getSymKey({tab,count,...}:SymTable,idx) =
	 if !count>idx then Array.sub(!tab,idx)
	 else raise NoSuchSymbol

      (*--------------------------------------------------------------------*)
      (* map a Key to its index in the symbol table. if it is not in the    *)
      (* symbol table yet, add a new entry with a new index. grow the table *)
      (* if there is no more free index in the table.                       *)
      (*--------------------------------------------------------------------*)
      fun getSymIndex(st as {tab,hash,hashFun,size,count,...}:SymTable,key) = 
	 let 
	    val idx = !hashFun key
	    val bucket = Array.sub(!hash,idx)
	 in 
	    case searchBucket(key,bucket)
	      of SOME i => i
	       | NONE => let val i = !count
			     val (idx',buck') = if !size>i then (idx,bucket)
						else let val _ = growTable st
							 val idx' = !hashFun key
							 val buck' = Array.sub(!hash,idx')
						     in (idx',buck')
						     end
			     val _ = Array.update(!hash,idx',addToBucket((key,i),buck'))
			     val _ = Array.update(!tab,i,key) 
			     val _ = count := i+1
			 in i
			 end
	 end
      
      (*--------------------------------------------------------------------*)
      (* does a Key have an entry in a symbol table?                        *)
      (*--------------------------------------------------------------------*)
      fun hasSymIndex({hash,hashFun,...}:SymTable,key) = 
	 let 
	    val idx = !hashFun key
	    val buck = Array.sub(!hash,idx)
	 in 
	    searchBucket(key,buck)
	 end

      (*--------------------------------------------------------------------*)
      (* reserve an index for a (yet unknown) key.                          *) 
      (*--------------------------------------------------------------------*)
      fun reserveSymIndex(st as {size,count=count as ref i,...}:SymTable) = 
	 let 
	    val _ = if !size>i then () else growTable st
	    val _ = count := i+1
	 in i
	 end
	 
      (*--------------------------------------------------------------------*)
      (* assign an index to a (previously reserved) index.                  *)
      (*--------------------------------------------------------------------*)
      fun assignSymIndex(st as {count,hash,hashFun,tab,...}:SymTable,key,i) =
	 if !count<=i then raise NoSuchSymbol
	 else let val idx = !hashFun key
		  val buck = Array.sub(!hash,idx)
		  val newBuck = addToBucket((key,i),buck) 
		  val _ = Array.update(!hash,idx,newBuck)
		  val _ = Array.update(!tab,i,key)
	      in ()
	      end
	   
      (*--------------------------------------------------------------------*)
      (* extract the contents of a symbol table to a vector.                *)
      (*--------------------------------------------------------------------*)
      fun extractSymTable({count,tab,...}:SymTable) = 
	 ArraySlice.vector (ArraySlice.slice (!tab,0,SOME(!count)))

      (*--------------------------------------------------------------------*)
      (* print the contents of the symbol table.                            *)
      (*--------------------------------------------------------------------*)
      fun printSymTable ({desc,tab,count,...}:SymTable) = 
	 (print (desc^" table:\n");
	  ArraySlice.appi 
	  (fn (n,key) =>
	   print ("  "^Int.toString n^": "^Key.toString key^"\n")) 
	  (ArraySlice.slice (!tab,0,SOME (!count))))
   end
