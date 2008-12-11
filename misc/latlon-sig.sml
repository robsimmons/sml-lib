signature LATLON =
sig

    (* A position on the Earth. *)
    type pos
    exception LatLon of string

    (* latitude measured in degrees. Positive is north and negative is south.
       longitude measured in degrees. Positive is east and negative is west. *)
    val fromdegs : { lat : real, lon : real } -> pos

    (* XXX from DMS, etc... *)
        
    (* Great circle distance, in meters *)
    val dist_meters : pos * pos -> real
    val dist_feet : pos * pos -> real
    val dist_miles : pos * pos -> real
    val dist_nautical_miles : pos * pos -> real
    val dist_km : pos * pos -> real

    (* ? *)
    val dist_rads : pos * pos -> real
end