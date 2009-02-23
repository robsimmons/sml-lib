(* This is a naive implementation. For every point test,
   we test it against each polygon's axis-aligned bounding
   box, then do a point inclusion test. For maps without
   a lot of regions, this is reasonable. For large inputs and
   some contrived cases it is atrocious. There are log(n)
   time, O(n) space algorithms that should be implemented in
   place of this one. For a survey,

   http://en.wikipedia.org/wiki/Point_location
*)

structure PointLocation :> POINTLOCATION =
struct

  exception PointLocation of string
  type polygon = Polygon.polygon

  type 'a locator = ('a * { topx : real, topy : real,
                            botx : real, boty : real} * polygon) Vector.vector

  fun location v (x, y) =
      case Vector.find (fn (a, {topx, topy, botx, boty}, p) =>
                        x >= topx andalso y >= topy andalso
                        x <= botx andalso y <= boty andalso
                        Polygon.pointinside p (x, y)) of
          SOME (a, _, _) => SOME a
        | NONE => NONE

  fun locatorex eps polys =
      let
          (* This is so that we can 
          val polys = Vector.fromList (map #2 polys)
      in

      end

  (* Not really reasonable to force some notion of "small" on the client,
     so never merge points. *)
  fun locator polys = locatorex 0.0 polys
      

end