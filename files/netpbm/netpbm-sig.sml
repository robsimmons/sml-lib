signature NETPBM =
sig

    exception NetPBM of string

    type 'radix graphic = { width : int,
                            height : int,
                            data : 'radix Array.array }

    (* also, readany that switches on the Pn header and
       returns a datatype... *)

    (* P1, P4 *)
    (* val readpbm : Reader.reader -> bool graphic *)
    (* P2, P5 *)
    val readpgm : Reader.reader -> int graphic
    (* P3, P6 *)
    (* val readppm : Reader.reader -> (int * int * int) graphic *)

end
