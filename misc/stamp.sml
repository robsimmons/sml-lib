
structure Stamp =
struct

    type stamp = int

    val eq = op =
    fun is a b = a = b
    val compare = Int.compare

    local 
        val ctr = ref 0
    in
        fun new () =
            let in
                ctr := 1 + !ctr;
                !ctr
            end
    end

    val tostring = Int.toString
end