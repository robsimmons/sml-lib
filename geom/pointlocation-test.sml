
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

            fun wr w = Real.fromLargeInt (Word32.toLargeInt w)
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

            val a = P.frompoints [perturb u, perturb v, perturb x]
            val b = P.frompoints [perturb u, perturb t, perturb x]
            val c = P.frompoints [perturb v, perturb w, perturb x]
            val d = P.frompoints [perturb x, perturb w, perturb y]
            val e = P.frompoints [perturb s, perturb z, perturb y]

            val normed = 
                PointLocation.normalize epsilon 
                [(A, a), (B, b), (C, c), (D, d), (E, e)]
        in
            (PointLocation.locator normed)
        end

    fun show () = 
        let
            val f = TextIO.openOut "pointlocation-test.svg"
            fun wr s = TextIO.output(f, s)
            val l = make 1.0
        in
            PointLocation.tosvg l wr;
            TextIO.closeOut f
        end
end

structure PL = PointLocation
structure PLT = PointLocationTest
