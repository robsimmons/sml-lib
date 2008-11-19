structure KeyVector : Key = 
   struct
      type Key = UniChar.Vector

      val null     = UniChar.nullVector
      val compare  = UniChar.compareVector
      val toString = UniChar.Vector2String
      val hash     = UniChar.hashVector
   end

structure VectorDict = Dict (structure Key = KeyVector) 
