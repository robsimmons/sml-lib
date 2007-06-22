
(* I've attempted to use the same variable names and algorithms
   as Applied Cryptography uses, to make reading this as along
   side that text as easy as possible. *)

structure Prime = 
struct

    infix 6 ++ --
    infix 7 divv modd **
    infix 4 << >> <<= >>=
        
    structure N = IntInf

    val op ++ = N.+
    val op ** = N.*
    val op -- = N.-
    val op divv = N.div
    val op modd = N.mod

    val iton = N.fromInt

    val n0 = iton 0
    val n1 = iton 1
    val n2 = iton 2

    val smallprimes =
        map iton
        [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101,
         103,107,109,113,127,131,137,139,149,151,157,163,167,173,179,181,191,193,197,
         199,211,223,227,229,233,239,241,251,257,263,269,271,277,281,283,293,307,311,
         313,317,331,337,347,349,353,359,367,373,379,383,389,397,401,409,419,421,431,
         433,439,443,449,457,461,463,467,479,487,491,499,503,509,521,523,541,547,557,
         563,569,571,577,587,593,599,601,607,613,617,619,631,641,643,647,653,659,661,
         673,677,683,691,701,709,719,727,733,739,743,751,757,761,769,773,787,797,809,
         811,821,823,827,829,839,853,857,859,863,877,881,883,887,907,911,919,929,937,
         941,947,953,967,971,977,983,991,997,1009,1013,1019,1021,1031,1033,1039,1049,
         1051,1061,1063,1069,1087,1091,1093,1097,1103,1109,1117,1123,1129,1151,1153,
         1163,1171,1181,1187,1193,1201,1213,1217,1223,1229,1231,1237,1249,1259,1277,
         1279,1283,1289,1291,1297,1301,1303,1307,1319,1321,1327,1361,1367,1373,1381,
         1399,1409,1423,1427,1429,1433,1439,1447,1451,1453,1459,1471,1481,1483,1487,
         1489,1493,1499,1511,1523,1531,1543,1549,1553,1559,1567,1571,1579,1583,1597,
         1601,1607,1609,1613,1619,1621,1627,1637,1657,1663,1667,1669,1693,1697,1699,
         1709,1721,1723,1733,1741,1747,1753,1759,1777,1783,1787,1789,1801,1811,1823,
         1831,1847,1861,1867,1871,1873,1877,1879,1889,1901,1907,1913,1931,1933,1949,
         1951,1973,1979,1987,1993,1997,1999]

    fun random_odd bits =
        let
            (* generate n bits, always odd *)
            fun gb acc 1 = acc ** n2 ++ n1
              | gb acc n =
                gb (if CryptRand.bit () then acc ** n2 ++ n1 else acc ** n2) (n - 1)
        in
            gb n1 (bits - 1)
        end

    fun no_smallfactor n =
        List.all (fn q => n modd q <> n0) smallprimes

    (* Miller-Rabin *)
    fun miller_rabin k p =
        let
            (* must be odd, > 2 *)
            fun scale (m, b) = 
                let val (q, r) = Number.quotrem (m, n2)
                in if r = n1 then (m, b)
                   else scale (q, b + 1)
                end

            val (m, b) = scale (p -- n1, 1)


            fun test 0 = true
              | test n =
                let
                    val a = CryptRand.inrange(n0, p)
                    val _ = print ("#")
                    val z = Number.modexp a m p

                    fun iter (z, j) =
                        z ++ n1 = p orelse 
                        (j < b andalso
                         z <> n1 andalso
                         iter ((z ** z) modd p, j+1))
                        
                in
                    (z = n1 orelse 
                     iter (z, 0)) andalso test (n-1)
                end
        in
            print ".";
            no_smallfactor p andalso
            test k
        end

    fun makeprime callback k bits = 
        let
            val pp = random_odd bits
        in
            if miller_rabin k pp then (print "!\n"; pp)
            else (callback (); makeprime callback k bits)
        end

end