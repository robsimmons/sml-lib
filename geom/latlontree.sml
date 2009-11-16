structure LatLonTree :> LATLONTREE =
struct

  structure QArg =
  struct
    type pos = LatLon.pos
    fun pos (x, y) = LatLon.fromdegs { lon = x, lat = y }
    fun xpos pt = #lon (LatLon.todegs pt)
    fun ypos pt = #lat (LatLon.todegs pt)
    type xpos = real (* longitude, degrees *)
    type ypos = real (* latitude, degrees *)
    type dist = real (* great circle distance in meters *)
    val xleq : xpos * xpos -> bool = op <=
    val yleq : ypos * ypos -> bool = op <=
    val xsub : xpos * xpos -> xpos = op -
    val ysub : ypos * ypos -> ypos = op -
    val xzero = 0.0
    val yzero = 0.0
    fun dist p pp = LatLon.dist_meters (p, pp)
        
    (* PERF: Probably a straightforward formula for great circles
       along lines of latitude and longitude, but this also suffices *)
    fun xdist (x1 : xpos, x2 : xpos) = dist (pos (x1, yzero)) (pos (x2, yzero))
    fun ydist (y1, y2) = dist (pos (xzero, y1)) (pos (xzero, y2))

    val dleq : dist * dist -> bool = op <=
  end
  structure Q = QuadtreeFn(QArg)

  (* Two hemispheres. The west hemisphere is longitude [-180.0, 0.0);
     the east [0.0, +180.0). *)
  type 'a tree = { w : 'a Q.tree, e : 'a Q.tree }

  type pos = LatLon.pos
  type dist = real

  val empty = { w = Q.empty, e = Q.empty }
  fun insert { w, e } a p =
      let
          val { lon, lat } = LatLon.todegs p
      in
          if lon < 0.0
          then { w = Q.insert w a p,
                 e = e }
          else { w = w,
                 e = Q.insert e a p }
      end

  fun lookuppoint { w, e } p d =
      let val { lon = x, lat = y } = LatLon.todegs p
      in
          (* If it's near latitude 0.0 or 180.0, we need to check both. *)
          (if QArg.dleq (QArg.dist p (QArg.pos (0.0, y)), d) orelse
              QArg.dleq (QArg.dist p (QArg.pos (180.0, y)), d)
           then (* Slow case *)
               Q.lookuppoint w p d @ Q.lookuppoint e p d
           else (* Faster case *)
               if x < 0.0
               then Q.lookuppoint w p d
               else Q.lookuppoint e p d)
      end

  (* PERF could manually deforest this. It'd be easy and save
     two maps plus point conversions. *)
  fun lookup q p d = map #1 (lookuppoint q p d)
  fun map f { w, e } = { w = Q.map f w, e = Q.map f e }
  fun app f { w, e } = (Q.app f w; Q.app f e)

  fun apppoint (f : ('a * pos) -> unit) { w, e } : unit =
      (Q.apppoint f w; Q.apppoint f e)

  fun closestpoint { w, e } p =
      case (Q.closestpoint w p, Q.closestpoint e p) of
          (SOME r, NONE) => SOME r
        | (NONE, SOME r) => SOME r
        | (NONE, NONE) => NONE
        | (SOME (a as (_, da)), SOME (b as (_, db))) =>
              SOME (if da < db
                    then a
                    else b)

  fun tosvg { w, e } d west print =
      let
          (* XXX don't hard code. Get from tree. But, we can't just look
             at the root; need some kind of function in QuadTree for getting
             the root or centroid; or, hack it using app *)
          val home = LatLon.fromdegs { lat = 40.452911, lon = ~79.936313 }

          val t = if west then w else e
          fun proj p = LatLon.gnomonic home p
      in
          Q.tosvg t d proj print
      end

end
