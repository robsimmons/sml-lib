







structure KeyString : Key = 
   struct
      type Key = string

      val null    = ""
      val hash    = UtilHash.hashString
      val compare = String.compare

      fun toString str = str
   end

structure StringDict = Dict (structure Key = KeyString) 
