structure LastNBuffer : LASTNBUFFER =
struct

    (* zero (0 <= zero < size data) is the zero element. *)
    type 'a buffer = { zero : int, data : 'a Array.array } ref

    fun buffer (n, x) = ref { zero = 0, data = Array.array(n, x) }
    fun tabulate (n, f) = ref { zero = 0, data = Array.tabulate(n, f) }
    fun fromList l = ref { zero = 0, data = Array.fromList l }

    fun rotate_left (r as ref { zero, data }) = 
        r := { zero = (zero + 1) mod (Array.length data), data = data }

    fun rotate_right (r as ref { zero, data }) = 
        r := { zero = (zero - 1) mod (Array.length data), data = data }

    (* Last element. Element zero falls off. *)
    fun push_back (r as ref { zero, data }, e) =
        let in
            Array.update(data, zero, e);
            rotate_left r
        end

    fun push_front (r, e) =
        let in
            rotate_right r;
            Array.update(#data (!r), #zero (!r), e)
        end

    fun sub (ref {zero, data}, x) = Array.sub(data, (zero + x) mod Array.length data)
    fun update (ref {zero, data}, x, e) = Array.update(data, (zero + x) mod Array.length data, e)
    fun length (ref {zero = _, data}) = Array.length data

    (* In order from element 0 to sz-1, so can't use Array.app *)
    fun appi f (ref {zero, data}) =
        let
            (* One might be empty; no matter. *)
            val first = ArraySlice.slice (data, zero, NONE)
            val second = ArraySlice.slice (data, 0, SOME zero)
            val offset = ArraySlice.length first
        in
            ArraySlice.appi f first;
            ArraySlice.appi (fn (i, e) => f (i + offset, e)) second
        end

    (* ditto *)
    fun app f (ref { zero, data }) =
        let
            val first = ArraySlice.slice (data, zero, NONE)
            val second = ArraySlice.slice (data, 0, SOME zero)
        in
            ArraySlice.app f first;
            ArraySlice.app f second
        end
  
end