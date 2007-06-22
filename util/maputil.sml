
functor MapUtil(structure M : ORD_MAP) : MAPUTIL where type 'a map = 'a M.map =
struct
    
    type 'a map = 'a M.map

    exception EarlyExit

    fun exists f m =
        (M.app (fn i => if f i then raise EarlyExit else ()) m;
         false) handle EarlyExit => true


end
