structure LatLon :> LATLON =
struct

    type pos = { lat : real, lon : real }
    exception LatLon of string

    (* XXX should check range of degrees *)
    fun fromdegs d = d

    fun dist_rads ({ lat = lat1, lon = lon1 },
                   { lat = lat2, lon = lon2 }) =
        let
            val a = LargeReal.Math.pi / 180.0
            val lat1 = Real.toLarge lat1 * a
            val lon1 = Real.toLarge lon1 * a
            val lat2 = Real.toLarge lat2 * a
            val lon2 = Real.toLarge lon2 * a

            val s = LargeReal.Math.sin lat1 * LargeReal.Math.sin lat2
            val c = LargeReal.Math.cos lat1 * LargeReal.Math.cos lat2
            val l = LargeReal.Math.cos(lon1 - lon2)
            val t = s + (c * l)
        in
            LargeReal.Math.atan(~t / LargeReal.Math.sqrt(~t * t + 1.0)) + 2.0 * LargeReal.Math.atan 1.0
        end

    fun dist_nautical_miles (p, q) = dist_rads (p, q) * 3437.73877
    fun dist_miles (p, q) = (dist_nautical_miles (p, q) * 57875.0) / 50292.0
    fun dist_meters (p, q) = dist_nautical_miles (p, q) * 1852.0
    fun dist_feet (p, q) = dist_meters (p, q) / 0.30480061

end
