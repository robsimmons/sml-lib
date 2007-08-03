
functor SetUtil(structure S : ORD_SET) : SETUTIL where type set = S.set
                                                   and type item = S.item =
struct
    
  type set = S.set
  type item = S.item

  fun fromlist l = List.foldr S.add' S.empty l
  fun tolist s = S.foldr op:: nil s
  fun mappartial f s =
    let
      fun folder (item, s) =
        case f item of
          NONE => s
        | SOME item' => S.add(s, item')
    in
      S.foldr folder S.empty s
    end

end
