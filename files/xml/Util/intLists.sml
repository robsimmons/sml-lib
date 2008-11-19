signature IntLists =
   sig
      type IntList = int list

      val emptyIntList   : IntList
      val singleIntList  : int -> IntList
      val fullIntList    : int -> IntList

      val isEmptyIntList : IntList -> bool
      val inIntList      : int * IntList -> bool
      val subIntList     : IntList * IntList -> bool

      val compareIntLists: IntList * IntList -> order 
      val hashIntList    : IntList -> word

      val addIntList     : int * IntList -> IntList
      val delIntList     : int * IntList -> IntList

      val cupIntLists    : IntList * IntList -> IntList
      val capIntLists    : IntList * IntList -> IntList
      val diffIntLists   : IntList * IntList -> IntList

      val IntList2String : IntList -> string
   end

structure IntLists : IntLists = 
   struct
      open UtilCompare UtilHash UtilInt UtilList UtilString

      type IntList = int list

      val emptyIntList = nil : IntList

      fun fullIntList n = intervalList(0,n)
      fun singleIntList n = [n]
      val isEmptyIntList = null

      val inIntList = elem Int.compare
      val subIntList = sub Int.compare
      val addIntList = insert Int.compare
      val delIntList = delete Int.compare
      val capIntLists = cap Int.compare
      val cupIntLists = merge Int.compare
      val diffIntLists = diff Int.compare
      val compareIntLists = compareList Int.compare
      val hashIntList = hashList hashInt

      val IntList2String = List2String Int.toString 
   end			      
