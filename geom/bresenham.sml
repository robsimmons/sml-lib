structure Bresenham :> BRESENHAM =
struct
  structure W = Word32
  val w2i = W.toIntX
  val i2w = W.fromInt

  type coord = int * int
  type iter = { step : unit, state : int * int * int }

  (* XXX INLINE *)
  fun pair_swap (x, y) = (y, x)
  fun pair_map f (x, y) = (f x, f y)
  fun pair_map2 f (x1, y1) (x2, y2) = (f (x1, x2), f (y1, y2))

  fun build ((x0, y0), (x1, y1), (dx, dy), (stepx, stepy), post) =
    let val frac0 = dy - Int.quot (dx, 2)
        fun step (x0, y0, frac) =
          if x0 = x1
          then NONE
          else
              let val (y0, frac) = if frac >= 0 
                                   then (y0 + stepy, frac - dx) 
                                   else (y0, frac)
                  val x0 = x0 + stepx
                  val frac = frac + dy
              in SOME ((x0, y0, frac), post (x0, y0)) 
              end
    in ({step = step, seed = (x0, y0, frac0)}, post (x0, y0)) 
    end

  fun line p0 p1 =
    let
        val d = pair_map2 op- p1 p0
        fun abs c = if c < 0 then (~c, ~1) else (c, 1)
        val ((dx', stepx), (dy', stepy)) = pair_map abs d
        val step = (stepx, stepy)
        val d'' as (dx'', dy'') = pair_map (fn n => n * 2) (dx', dy')

        val swap = pair_swap
        val cvt = (fn x => x)
        val seed =
            if dx'' > dy''
            then (p0, p1, d'', step, cvt)
            else (swap p0, swap p1, swap d'', swap step, swap)
    in build seed
    end

  fun all pred p0 p1 =
    let val ({step, seed}, v) = line p0 p1
        fun loop seed =
            case step seed of
                NONE => true
              | SOME (seed', v) => pred v andalso loop seed'
    in pred v andalso loop seed 
    end

  fun all pred p0 p1 =
    let val ({step, seed}, v) = line p0 p1
        fun loop seed =
            case step seed of
                NONE => false
              | SOME (seed', v) => pred v andalso loop seed'
    in pred v orelse loop seed
    end

  fun app f p0 p1 =
    let 
        val ({step, seed}, v) = line p0 p1
        fun loop seed =
            case step seed of
                NONE => ()
              | SOME (seed', v) => (f v; loop seed')
    in 
        f v;
        loop seed
    end

end
