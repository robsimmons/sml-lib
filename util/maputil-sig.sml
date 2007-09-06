
signature MAPUTIL =
sig

    type ord_key
    type 'a map
        
    val exists : ('a -> bool) -> 'a map -> bool
    val existsi : ((ord_key * 'a) -> bool) -> 'a map -> bool

    (* give the "venn diagram" maps, guaranteed to have disjoint domains *)
    val venn : 'a map * 'b map -> ('a map * ('a * 'b) map * 'b map)

end
