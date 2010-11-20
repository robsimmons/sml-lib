functor Rsync32Fn(type elt
                  type vector
                  val word : elt -> Word16.word
                  val sub : vector * int -> elt
                    ) =
struct

  structure R = 
  RsyncFn(type elt = elt
          type vector = vector
          val word = word
          val sub = sub
          structure HalfWord = Word16
          structure FullWord = Word32
          fun extend w = Word32.fromLargeWord (Word16.toLargeWord w)
          fun trunc w  = Word16.fromLargeWord (Word32.toLargeWord w)
              )
  open R

end
