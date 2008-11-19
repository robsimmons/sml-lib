






structure KeyUri : Key = 
   struct
      type Key = Uri.Uri

      val null     = Uri.emptyUri
      val compare  = Uri.compareUri
      val toString = Uri.Uri2String
      val hash     = Uri.hashUri
   end

structure UriDict = Dict (structure Key = KeyUri) 
