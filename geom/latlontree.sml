structure LatLonTree :> LATLONTREE =
struct

  structure QArg =
  struct
    type xpos = real (* longitude, degrees *)
    type ypos = real (* latitude, degrees *)
    type dist = real (* great circle distance in meters *)
    val xleq : xpos * xpos -> bool = op <=
    val yleq : ypos * ypos -> bool = op <=
    val xsub : xpos * xpos -> xpos = op -
    val ysub : ypos * ypos -> ypos = op -
    val xzero = 0.0
    val yzero = 0.0
    fun dist (x, y) (xx, yy) = 
        LatLon.dist_meters (LatLon.fromdegs { lon = x, lat = y },
                            LatLon.fromdegs { lon = xx, lat = yy })
        
    (* PERF: Probably a straightforward formula for great circles
       along lines of latitude and longitude, but this also suffices *)
    fun xdist (x1, x2) = dist (x1, yzero) (x2, yzero)
    fun ydist (y1, y2) = dist (xzero, y1) (xzero, y2)

    val dleq : dist * dist -> bool = op <=
  end
  structure Q = QuadtreeFn(QArg)

  (* Two hemispheres. The west hemisphere is longitude [-180.0, 0.0);
     the east [0.0, +180.0). *)
  type 'a latlontree = { w : 'a Q.quadtree,
                         e : 'a Q.quadtree }

  val empty = { w = Q.empty, e = Q.empty }
  fun insert { w, e } a p =
      let
          val { lon, lat } = LatLon.todegs p
      in
          if lon < 0.0
          then { w = Q.insert w a (lon, lat),
                 e = e }
          else { w = w,
                 e = Q.insert e a (lon, lat) }
      end

  fun lookuppoint { w, e } p d =
      let val { lon = x, lat = y } = LatLon.todegs p
          fun makepts (a, lon, lat) = (a, LatLon.fromdegs { lat = lat, lon = lon })
      in
          map makepts
          (* If it's near latitude 0.0 or 180.0, we need to check both. *)
          (if QArg.dleq (QArg.dist (x, y) (0.0, y), d) orelse
              QArg.dleq (QArg.dist (x, y) (180.0, y), d)
           then (* Slow case *)
               Q.lookuppoint w (x, y) d @ Q.lookuppoint e (x, y) d
           else (* Faster case *)
               if x < 0.0
               then Q.lookuppoint w (x, y) d
               else Q.lookuppoint e (x, y) d)
      end

  (* PERF could manually deforest this. It'd be easy and save
     two maps plus point conversions. *)
  fun lookup q p d = map #1 (lookuppoint q p d)
  fun map f { w, e } = { w = Q.map f w, e = Q.map f e }
  
end
