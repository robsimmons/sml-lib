structure UnionFind :> UNIONFIND =
struct

    exception GrowArray of string

    structure GA = GrowArray

    (* Array and stamp for comparison *)
    type 'a arena = 'a GA.growarray * unit ref

    (* Arena and index into it *)
    type 'a class = 'a arena * int
    fun compare ((a, i), (aa, ii)) =
        if #2 a <> #2 aa
        then raise GrowArray "attempt to compare classes from different arenas"
        else Int.compare (find a i, find aa ii)

    fun arena (a, i) = a

    fun add (a, s) = 

    (* Perform the "find" operation for the index, compressing as we go. *)
    fun find (a, s) i =
        case GrowArray.sub a i of
            ~1 => i
          | n => 
            let val r = find (a, s) n
            in GA.update a i r; r
            end

    fun union ((a, i), (aa, ii)) =
        if #2 a <> #2 aa
        then raise GrowArray "attempt to union classes from different arenas"
        else GrowArray.update a (find a i) (find aa ii)

    fun new () = (GA.empty (), ref ())


end