(*--------------------------------------------------------------------------*)
(* Functor: Dict                                                            *)
(*                                                                          *)
(* Depends on:                                                              *)
(*   Chars                                                                  *)
(*                                                                          *)
(* Exceptions raised by functions in this functor:                          *)
(*   addByIndex  : NoSuchIndex                                              *)
(*   addByKey    : InternalError                                            *)
(*   getByIndex  : NoSuchIndex                                              *)
(*   getByKey    : InternalError                                            *)
(*   getIndex    : InternalError                                            *)
(*   getKey      : NoSuchIndex                                              *)
(*   hasIndex    : none                                                     *)
(*   makeDict    : none                                                     *)
(*   nullDict    : none                                                     *)
(*   printDict   : none                                                     *)
(*   usedIndices : none                                                     *) 
(*--------------------------------------------------------------------------*)
(* A dictionary maps keys to consecutive integers and additionally holds    *)
(* a value of arbitrary type for each entry.                                *)
(*--------------------------------------------------------------------------*)
signature Dict =
   sig
      type Key
      type 'a Dict
         
      exception NoSuchIndex

      val nullDict   : string * 'a -> 'a Dict
      val makeDict   : string * int * 'a -> 'a Dict
      val clearDict  : 'a Dict * int option -> unit
         
      val hasIndex   : 'a Dict * Key -> int option
      val getIndex   : 'a Dict * Key -> int
      val getKey     : 'a Dict * int -> Key

      val getByIndex : 'a Dict * int -> 'a
      val getByKey   : 'a Dict * Key -> 'a
         
      val setByIndex : 'a Dict * int * 'a -> unit 
      val setByKey   : 'a Dict * Key * 'a -> unit
         
      val usedIndices : 'a Dict -> int

      val extractDict : 'a Dict -> (Key * 'a) array
      val printDict   : ('a -> string) -> 'a Dict -> unit
   end

functor Dict (structure Key : Key) : Dict =
   struct
      open UtilError UtilInt

      type Key = Key.Key
         
      exception NoSuchIndex

      (*--------------------------------------------------------------------*)
      (* a dictionary can have at most size MAX_WIDTH. This is because      *)
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
      (* buckets are unsorted - they are probably small, so comparing the   *)
      (* keys might be overkill.                                            *)
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
      (* a dictionary consists of                                           *)
      (* - a string desc saying what is stored in this dictionary           *)
      (* - an array tab holding for each index its key and value            *)
      (* - a hash table, i.e. Bucket array, of double size than tab         *)
      (* - a hashFun mapping Key to the range of the hash table             *)
      (* - an integer width for computing table sizes                       *)
      (* - an integer size wich is the size of the value table              *)
      (* - an integer count holding the next free index                     *)
      (* - a default value for the value table                              *)
      (*--------------------------------------------------------------------*)
      type 'a Dict = {desc    : string,
                      tab     : (Key * 'a) array ref,
                      hashTab : Bucket array ref,
                      hashFun : (Key -> int) ref,
                      width   : int ref,  (* bit width *)
                      size    : int ref,  (* tab size=2^width, hash size is double *) 
                      count   : int ref,  (* number of entries *)
                      def     : 'a        (* default for values *)
                      }
      fun nullDict (desc,def) = {desc    = desc,
                                 tab     = ref (Array.array(1,(Key.null,def))),
                                 hashTab = ref (Array.array(2,nullBucket)),
                                 hashFun = ref (fn _ => 0),
                                 count   = ref 0,
                                 size    = ref 1,
                                 width   = ref 0,
                                 def     = def}

      (*--------------------------------------------------------------------*)
      (* how many entries are in the dictionary?                            *)
      (*--------------------------------------------------------------------*)
      fun usedIndices ({count,...}:'a Dict) = !count
         
      (*--------------------------------------------------------------------*)
      (* what is the table load, i.e. percentage of number of entries to    *)
      (* hash table size = 100*count/(2*size) = 50*count/size.              *)
      (*--------------------------------------------------------------------*)
      fun hashRatio({count,size,...}:'a Dict) = 50 * !count div !size
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
            val mask = 0w2*Word.fromInt size-0w1 
            val half = Word.fromInt((width+1) div 2)
	 in 
	    hashKey(half,mask)
	 end
	    
      (*--------------------------------------------------------------------*)
      (* create a new dictionary for 2^w, but at least 2 and at most 2^m    *)
      (* entries, where m is the value of MAX_WIDTH.                        *)
      (*--------------------------------------------------------------------*)
      fun makeDict (desc,w,def) = 
         let 
            val width= Int.min(Int.max(1,w),MAX_WIDTH)
            val size = Word.toInt(Word.<<(0w1,Word.fromInt(width-1)))
         in {desc    = desc,
             tab     = ref (Array.array(size,(Key.null,def))),
             hashTab = ref (Array.array(2*size,nullBucket)),
             hashFun = ref (makeHashFun(size,width)),
             width   = ref width,
             size    = ref size,
             count   = ref 0,
             def     = def}
         end
      
      (*--------------------------------------------------------------------*)
      (* clear a dictionary. If the 2nd arg is SOME w, use w for resizing.  *)
      (*--------------------------------------------------------------------*)
      fun clearDict (dict:'a Dict,widthOpt) = 
	 case widthOpt 
	   of NONE => 
	      let 
		 val {tab=ref tab,hashTab=ref hashTab,size,count,def,...} = dict
		 val _ = appInterval (fn i => Array.update(tab,i,(Key.null,def))) (0,!count-1)
		 val _ = appInterval (fn i => Array.update(hashTab,i,nullBucket)) (0,!size*2-1)
	      in 
		 count := 0
	      end
	    | SOME w => 
	      let 
		 val {tab,hashTab,hashFun,width,size,count,def,...} = dict
		 val newWidth = Int.min(Int.max(1,w),MAX_WIDTH)   
		 val newSize  = Word.toInt(Word.<<(0w1,Word.fromInt(newWidth-1)))
		 val _ = tab     := (Array.array(newSize,(Key.null,def)))
		 val _ = hashTab := (Array.array(2*newSize,nullBucket))
		 val _ = hashFun := (makeHashFun(newSize,newWidth))
		 val _ = width   := newWidth
		 val _ = size    := newSize
	      in
		 count := 0
	      end
      
      (*--------------------------------------------------------------------*)
      (* grow a dictionary to the double size. raise InternalError if the   *)
      (* dictionary already has maximal size.                               *)
      (*--------------------------------------------------------------------*)
      fun growDictionary ({desc,tab,hashTab,hashFun,width,size,count,def}:'a Dict) = 
         let 
            val oldTab = !tab
            val _ = if !width < MAX_WIDTH then width := !width+1 
		    else raise InternalError 
		       ("Dict","growDictionary",
			String.concat ["growing the ",desc," dictionary ",
				       "exceeded the system maximum size of ",
				       Int.toString Array.maxLen," for arrays"]) 
            val _ = size := !size*2
            val _ = tab  := Array.array(!size,(Key.null,def))
            val _ = hashTab := Array.array(!size*2,nullBucket)
            val _ = hashFun := makeHashFun(!size,!width)

            fun addTo (i,kv as (key,_)) = 
               let 
                  val idx = !hashFun key
                  val _ = Array.update(!hashTab,idx,addToBucket((key,i),Array.sub(!hashTab,idx)))
                  val _ = Array.update(!tab,i,kv)
               in ()
               end
         in 
	    Array.appi addTo oldTab
         end

      (*--------------------------------------------------------------------*)
      (* lookup the key for an index of the dictionary.                     *)
      (*--------------------------------------------------------------------*)
      fun getKey({tab,count,...}:'a Dict,idx) =
         if !count>idx then #1(Array.sub(!tab,idx))
         else raise NoSuchIndex

      (*--------------------------------------------------------------------*)
      (* map a Key to its index in the dictionary. if it is not in the      *)
      (* dictionary yet, add a new entry with a new index. grow the table   *)
      (* if there is no more free index in the dictionary.                  *)
      (*--------------------------------------------------------------------*)
      fun getIndex(dict as {tab,hashTab,hashFun,size,count,def,...}:'a Dict,key) = 
         let 
            val k = !hashFun key
            val bucket = Array.sub(!hashTab,k)
         in 
            case searchBucket(key,bucket)
              of SOME idx => idx
               | NONE => let val idx = !count
                             val (k',buck') = if !size>idx then (k,bucket)
                                                else let val _ = growDictionary dict
                                                         val k' = !hashFun key
                                                         val buck' = Array.sub(!hashTab,k')
                                                     in (k',buck')
                                                     end
                             val _ = Array.update(!hashTab,k',addToBucket((key,idx),buck'))
                             val _ = Array.update(!tab,idx,(key,def))
                             val _ = count := idx+1
                         in idx
                         end
         end
      
      (*--------------------------------------------------------------------*)
      (* does a Key have an entry in a dictionary?                          *)
      (*--------------------------------------------------------------------*)
      fun hasIndex({hashTab,hashFun,...}:'a Dict,key) = 
         let 
            val idx = !hashFun key
            val bucket = Array.sub(!hashTab,idx)
         in 
            searchBucket(key,bucket)
         end

      (*--------------------------------------------------------------------*)
      (* get the value stored for index idx                                 *)
      (*--------------------------------------------------------------------*)
      fun getByIndex({tab,count,...}:'a Dict,idx) =
         if !count>idx then #2(Array.sub(!tab,idx))
         else raise NoSuchIndex

      (*--------------------------------------------------------------------*)
      (* get the value stored for a key                                     *)
      (*--------------------------------------------------------------------*)
      fun getByKey(dict,key) =
         getByIndex(dict,getIndex(dict,key))
      
      (*--------------------------------------------------------------------*)
      (* enter a value for index idx.                                       *)
      (*--------------------------------------------------------------------*)
      fun setByIndex({tab,count,...}:'a Dict,idx,a) =
         if !count>idx then let val (key,_) = Array.sub(!tab,idx)
                            in Array.update(!tab,idx,(key,a))
                            end
         else raise NoSuchIndex

      (*--------------------------------------------------------------------*)
      (* enter a value for a key.                                           *)
      (*--------------------------------------------------------------------*)
      fun setByKey(dict,key,v) =
         setByIndex(dict,getIndex(dict,key),v)
      
      (*--------------------------------------------------------------------*)
      (* extract the contents of the dictionary to an array.                *)
      (*--------------------------------------------------------------------*)
      fun extractDict({count,tab,...}:'a Dict) = 
	 Array.tabulate(!count,fn i => Array.sub(!tab,i))

      (*--------------------------------------------------------------------*)
      (* print the contents of the dictionary.                              *)
      (*--------------------------------------------------------------------*)
      fun printDict X2String ({desc,tab,count,...}:'a Dict) = 
         (print (desc^" dictionary:\n");
          ArraySlice.appi 
          (fn (n,(key,value)) =>
           print ("  "^Int.toString n^": "^Key.toString key^" = "^X2String value^"\n")) 
          (ArraySlice.slice (!tab,0,SOME (!count))))
   end
