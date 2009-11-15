signature LATLON =
sig

    (* A position on the Earth. *)
    type pos
    exception LatLon of string

    (* latitude measured in degrees. Positive is north and negative is south.
       longitude measured in degrees. Positive is east and negative is west. 

       The inputs are treated modularly, so that +100 latitude is the
       same as -80. *)
    val fromdegs : { lat : real, lon : real } -> pos

    (* latitude always in [-90, +90),
       longitude always in [-180, +180). *)
    val todegs : pos -> { lat : real, lon : real }

    (* XXX val torads : pos -> { lat : real, lon : real } *)
    (* XXX from DMS, string, etc... *)

    (* Angle of a vector, in radians, where 0 is to the East and pi/2 is
       to the north. The angle is undefined when the points are the same
       or antipodes, in which case NONE is returned.
       nb. I don't think this works if the source is the North or South
       pole. *)
    val angle : pos * pos -> real option

        
    (* Great circle distances between positions. *)
    val dist_meters : pos * pos -> real
    val dist_feet : pos * pos -> real
    val dist_miles : pos * pos -> real
    val dist_nautical_miles : pos * pos -> real
    val dist_km : pos * pos -> real

    (* A projection maps positions to x,y coordinates. Many projections
       have infinite extent along one dimension, so we also specify the
       ranges of the output coordinates. *)
    type projection = pos -> real * real

    (* Produces x coordinates [-pi to +pi] and y coordinates -inf to +inf.
       Mercator distorts area near the poles badly.
       Give the meridian that shall be the longitudinal center of the map,
       in degrees. *)
    val mercator : real -> projection

    (* Uses the prime meridian as the longitudinal center of the map. *)
    val prime_mercator : projection

    (* Very simple projection where meridians and parallels are equally
       spaced straight lines. Does not preserve much of anything, but decent
       for local maps. Argument is the parallel (in degrees) where the scale
       is not distorted. For local maps, set this to a parallel within that
       region.
       Range is x: [-pi to +pi] and y: [-pi/2 to +pi/2].
       *)
    val equirectangular : real -> projection

    (* equirectangular when the equator is the standard parallel. *)
    val plate_carree : projection

    (* Gnomonic (rectilinear) projection centered on the given tangent point.
       This projects both hemispheres atop one another (the center point and
       its antipode will both be projected to 0,0). The map is highly distorted
       in area except near the tangent point and its antipode, but all great
       circles are straight lines.
       Range is x: [-inf to +inf] and y: [-inf to +inf].
       *)
    val gnomonic : pos -> projection

    (* XXX: Other useful projections:
       transverse mercator (uses parallel rather than meridian)
       
       Actually it would be useful to be able to set a reference point
       and then behave as though that is 0,0 in projections.
       *)

end