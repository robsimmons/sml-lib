(* For the distance calculations, I found out that numerical stability
   issues have a big effect, especially for very short distances. This
   file contains a few implementations of the distance calculation,
   but only the iterative solution is live code. I recommend it unless
   you need very fast approximations. *)
structure LatLon :> LATLON =
struct

    type pos = { lat : real, lon : real }
    type projection = pos -> real * real

    exception LatLon of string

    structure LR = LargeReal
    structure LRM = LargeReal.Math

    (* Return the unique x = d + r*max 
       where x is in [-max, +max). Used to put
       degrees in the range [-90, +90), for example,
       since some computations cause them to 
       "wrap around". *)
       
    fun plusminus_mod (d, max) =
        (* PERF there may be a cleverer way to do this?
           There's no equivalent of "mod" that works for
           negative inputs as far as I can see in Real. *)
        let
            val twomax = 2.0 * max
            val z = d + max
        in
            Real.rem(z, twomax) - max
        end

    fun fromdegs { lat, lon } =
        { lat = plusminus_mod (lat, 90.0),
          lon = plusminus_mod (lon, 180.0) }

    fun todegs d = d

    (* This is not as accurate for short distances. *)
    fun dist_rads_fast ({ lat = lat1, lon = lon1 },
                        { lat = lat2, lon = lon2 }) =
        let
            val a = LRM.pi / 180.0
            val lat1 = Real.toLarge lat1 * a
            val lon1 = Real.toLarge lon1 * a
            val lat2 = Real.toLarge lat2 * a
            val lon2 = Real.toLarge lon2 * a

            val s = LRM.sin lat1 * LRM.sin lat2
            val c = LRM.cos lat1 * LRM.cos lat2
            val l = LRM.cos(lon1 - lon2)
            val t = s + (c * l)
        in
            LRM.atan(~t / LRM.sqrt(~t * t + 1.0)) + 2.0 * LRM.atan 1.0
        end

    (*
    fun dist_nautical_miles (p, q) = dist_rads_fast (p, q) * 3437.73877
    fun dist_miles (p, q) = (dist_nautical_miles (p, q) * 57875.0) / 50292.0
    fun dist_meters (p, q) = dist_nautical_miles (p, q) * 1852.0
    fun dist_feet (p, q) = dist_meters (p, q) / 0.30480061
    *)

    fun torad x = Real.toLarge x * (LRM.pi / 180.0)

    (* Using the Haversine formula, which has better numerical stability
       for small distances (down to a meter or so). Treats the Earth
       as a sphere, however. *)
    fun dist_km_haversine ({ lat = lat1, lon = lon1 },
                           { lat = lat2, lon = lon2 }) =
        let 
            val r = 6371.0
            val dlat = torad (lat2 - lat1)
            val dlon = torad (lon2 - lon1)
            val lat1 = torad lat1
            val lat2 = torad lat2

            val a = 
                LRM.sin(dlat / 2.0) * LRM.sin(dlat / 2.0) +
                LRM.cos lat1 * LRM.cos lat2 *
                LRM.sin(dlon / 2.0) * LRM.sin(dlon / 2.0)
            val c = 2.0 * LRM.atan2(LRM.sqrt a,
                                    LRM.sqrt (1.0 - a))
        in
            (* mean radius of Earth in km *)
            6371.0 * c
        end

    (*
    fun dist_meters (p, q) = dist_km (p, q) * 1000.0
    fun dist_miles (p, q) = dist_km (p, q) * 0.621371192
    fun dist_feet (p, q) = dist_miles (p, q) * 5280.0
    fun dist_nautical_miles (p, q) = dist_km (p, q) * 0.539956803
    *)


    fun lrm_abs (f : LR.real) = if f < 0.0 then ~f else f

    (* Based on Chris Veness's JavaScript LGPL implementation of the Vincenty inverse
       solution on an ellipsoid model of the Earth. ((c) Chris Veness 2002-2008)
       http://www.movable-type.co.uk/scripts/latlong-vincenty.html *)
    exception Return of real
    fun dist_meters_vincenty ({ lat = lat1, lon = lon1 },
                              { lat = lat2, lon = lon2 }) =
        let
            val a = 6378137.0
            val b = 6356752.3142
            val f = 1.0 / 298.257223563
            val L = torad(lon2 - lon1)
            val U1 = LRM.atan((1.0 - f) * LRM.tan(torad lat1))
            val U2 = LRM.atan((1.0 - f) * LRM.tan(torad lat2))
            val sinU1 = LRM.sin U1
            val cosU1 = LRM.cos U1
            val sinU2 = LRM.sin U2
            val cosU2 = LRM.cos U2

                                                (* Is there really no NaN constant? *)
            fun loop (lambda, 0) = raise Return (LargeReal.posInf / LargeReal.posInf)
              | loop (lambda, itersleft) =
                let
                    val sinLambda = LRM.sin lambda
                    val cosLambda = LRM.cos lambda
                    val sinSigma = LRM.sqrt((cosU2 * sinLambda) * (cosU2 * sinLambda) +
                                            (cosU1 * sinU2 - sinU1 * cosU2 * cosLambda) *
                                            (cosU1 * sinU2 - sinU1 * cosU2 * cosLambda))
                    val () = if LR.==(sinSigma, 0.0) then raise Return 0.0 else ()
                    val cosSigma = sinU1 * sinU2 + cosU1 * cosU2 * cosLambda
                    val sigma = LRM.atan2(sinSigma, cosSigma)
                    val sinAlpha = cosU1 * cosU2 * sinLambda / sinSigma
                    val cosSqAlpha = 1.0 - sinAlpha * sinAlpha
                    val cos2SigmaM = cosSigma - 2.0 * sinU1 * sinU2 / cosSqAlpha
                    val cos2SigmaM =
                        if LR.isNan cos2SigmaM
                        then 0.0
                        else cos2SigmaM
                    val C = f / 16.0 * cosSqAlpha * (4.0 + f * (4.0 - 3.0 * cosSqAlpha))
                    val lambdaP = lambda
                    val lambda = L + (1.0 - C) * f * sinAlpha *
                        (sigma + C * sinSigma * 
                         (cos2SigmaM + C * cosSigma * (~1.0 + 2.0 * cos2SigmaM * cos2SigmaM)))
                in
                    if lrm_abs(lambda - lambdaP) > 1e~12
                    then loop(lambda, itersleft - 1)
                    else (lambda, cosSqAlpha, cos2SigmaM, sinSigma, cosSigma, sigma)
                end

            val (lambda, cosSqAlpha, cos2SigmaM, sinSigma, cosSigma, sigma) = loop(L, 20)

            val uSq = cosSqAlpha * (a * a - b * b) / (b * b)
            val A = 1.0 + uSq / 16384.0 * (4096.0 + uSq * (~768.0 + uSq * (320.0 - 175.0 * uSq)))
            val B = uSq / 1024.0 * (256.0 + uSq * (~128.0 + uSq * (74.0 - 47.0 * uSq)))
            val deltaSigma = B * sinSigma * (cos2SigmaM + B / 4.0 * 
                                             (cosSigma * (~1.0 + 2.0 * cos2SigmaM * cos2SigmaM) -
                                              B / 6.0 * cos2SigmaM * (~3.0 + 4.0 * sinSigma * sinSigma) *
                                              (~3.0 + 4.0 * cos2SigmaM * cos2SigmaM)))
        in
            b * A * (sigma - deltaSigma)
        end handle Return f => f

    val dist_meters = dist_meters_vincenty
    fun dist_km (p, q) = dist_meters (p, q) / 1000.0
    fun dist_miles (p, q) = dist_km (p, q) * 0.621371192
    fun dist_feet (p, q) = dist_miles (p, q) * 5280.0
    fun dist_nautical_miles (p, q) = dist_km (p, q) * 0.539956803

    (* Projections. *)

    fun mercator lambda0 ({ lat = phi, lon = lambda } : pos) =
        let
            (* Shift longitude to arrange for new central meridian, and make
               sure the result has range ~180.0--180.0. *)
            val lambda = Real.rem(lambda + 180.0 - lambda0, 360.0) - 180.0
            (* Convert from degrees to radians. *)
            val lambda = torad lambda
            val phi = torad lambda

            val sinphi = LRM.sin phi
        in
            (* This is probably slower *)
            (* Math.ln(Math.tan(Math.pi / 4.0 + phi / 2.0)) *)
            (lambda, 0.5 * LRM.ln((1.0 + sinphi) / (1.0 - sinphi)))
        end

    val prime_mercator = mercator 0.0

    fun equirectangular phi1 { lat = phi, lon = lambda } =
        (torad lambda * Math.cos(torad phi1), torad phi)

    (* Same as equirectangular 0.0, but saves the degenerate cos and multiply *)
    fun plate_carree { lat = phi, lon = lambda } = (torad lambda, torad phi)

    (* http://mathworld.wolfram.com/GnomonicProjection.html *)
    fun gnomonic { lat = phi1, lon = lambda0 } { lat = phi, lon = lambda } =
        let
            val phi1 = torad phi1
            val lambda0 = torad lambda0
            val phi = torad phi
            val lambda = torad lambda

            val cosc = LRM.sin phi1 * LRM.sin phi + LRM.cos phi1 * LRM.cos phi * LRM.cos (lambda - lambda0)
        in
            ((LRM.cos phi * LRM.sin (lambda - lambda0)) / cosc,
             (LRM.cos phi1 * LRM.sin phi -
              LRM.sin phi1 * LRM.cos phi * LRM.cos (lambda - lambda0)) / cosc)
        end


    (* Use gnomonic projection. In it, all great circles are straight lines. *)
    fun angle (src, dst) =
        let
            val proj = gnomonic src
            (* Think this should always be zero? *)
            val (srcx, srcy) = proj src
            val (dstx, dsty) = proj dst
            val x = dstx - srcx
            val y = dsty - srcy

            val lambda = LRM.sqrt(x * x + y * y)

            val normx = LR.checkFloat (x / lambda)
            val normy = LR.checkFloat (y / lambda)
        in
            SOME (LR.checkFloat (LRM.atan2 (normx, normy)))
        end handle Overflow => NONE
                 | Div => NONE

end
