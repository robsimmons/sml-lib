structure Bounds :> BOUNDS =
struct

  exception Empty

  type bounds = { empty : bool ref,
                  maxx : real ref, minx : real ref,
                  maxy : real ref, miny : real ref }

  (* Initialize to infinite bounds *)
  fun nobounds () = { empty = ref true,
                      maxx = ref (~1.0 / 0.0),
                      minx = ref (1.0 / 0.0),
                      maxy = ref (~1.0 / 0.0),
                      miny = ref (1.0 / 0.0) }

  fun offsetx { empty = ref true, ... } _ = raise Empty
    | offsetx { minx = ref r, ... } x = x - r

  fun offsety { empty = ref true, ... } _ = raise Empty
    | offsety { miny = ref r, ... } y = y - r

  fun width (bounds : bounds) = offsetx bounds (! (#maxx bounds))
  fun height (bounds : bounds) = offsety bounds (! (#maxy bounds))   

  fun getbounds { empty, maxx, minx, maxy, miny } =
      if !empty
      then raise Empty
      else { maxx = !maxx, minx = !minx,
             maxy = !maxy, miny = !miny }

  fun boundpoint ({ empty, maxx, minx, maxy, miny } : bounds) (x, y) =
      let
          fun bound p min max =
              let in
                  if p < !min
                  then min := p
                  else ();
                  if p > !max
                  then max := p
                  else ()
              end
      in
          bound x minx maxx;
          bound y miny maxy;
          empty := false
      end

  fun union b { empty = ref true, ... } = ()
    | union b { maxx, minx, maxy, miny, empty = _ } =
      let in
          boundpoint b (!minx, !miny);
          boundpoint b (!minx, !maxy);
          boundpoint b (!maxx, !miny);
          boundpoint b (!maxx, !maxy)
      end
end
