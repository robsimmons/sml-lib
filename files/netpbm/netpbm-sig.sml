signature NETPBM =
sig

    exception NetPBM of string

    type ('radix, 'radixrange) graphic = 
        { width : int,
          height : int,
          max : 'radixrange,
          data : 'radix Array.array }

    (* also, readany that switches on the Pn header and
       returns a datatype... *)

    (* P1, P4 *)
    (* val readpbm : Reader.reader -> bool graphic *)
    (* P2, P5 *)
    val readpgm : Reader.reader -> (int, int) graphic
    (* P3, P6 *)
    (* val readppm : Reader.reader -> (int * int * int) graphic *)

end
