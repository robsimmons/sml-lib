
structure PointLocationTest =
struct
    structure MT = MersenneTwister
    structure P = Polygon
    datatype tri = A | B | C | D | E

    (*                   1 1 1 1 1 1 1 1 1
       0 1 2 3 4 5 6 7 9 0 0 1 2 3 4 5 6 7
                     v             w
                      +-----------+          0
                    .  \         / \         1
                  ,     \   C   /   \        2
                .        \     /     \       3
              ,      A    \   /   D   \      4
            .              \ /         \     5
        u ,        ,,,,,,,,,+.....      \    6
         +'''''''''        . x    '''''''+ y 7
          \             ,          ..--`.    8
           \     B    .      ..--``   ,      9
            \       ,     +``       .       10
             \    .      s `  E   ,         11
              \ ,           `   .           12
               +             `+             13
               t               z            14
     *)

    fun make epsilon =
        let
            val mt = MT.init32 0wxBED
            (* random number in epsilon. *)

            fun wr w = Real.fromLargeInt (Word32.toLargeInt x)
            fun rand_eps() = (wr (MT.rand32 mt) / wr 0wxFFFFFFFF) * (epsilon / Math.sqrt 2.0)

            fun perturb (x, y) = (x + rand_eps() - (epsilon / 2.0),
                                  y + rand_eps() - (epsilon / 2.0))

            val u = (1.0, 7.0)
            val v = (7.5, 0.0)
            val w = (13.5, 0.0)
            val y = (17.0, 7.0)
            val x = (10.5, 6.0)
            val s = (10.0, 10.0)
            val t = (4.0, 13.0)
            val z = (11.5, 14.0)

            val a = P.polygon [u, v, x]
            val b = P.polygon [u, t, x]
            val c = P.polygon [v, w, x]
            val d = P.polygon [x, w, y]
            val e = P.polygon [s, z, y]
        in
            Locator.locatorex epsilon [(A, a), (B, b), (C, c), (D, d), (E, e)]
        end
end