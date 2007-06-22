
signature MAPUTIL =
sig
    
    type 'a map
        
    val exists : ('a -> bool) -> 'a map -> bool

end
