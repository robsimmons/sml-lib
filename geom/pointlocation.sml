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

  (* PERF could use something like a quadtree (would need to generalize
     it a bit) to sort the bounding boxes. *)
  fun location v (x, y) =
      case Vector.find (fn (a, {topx, topy, botx, boty}, p) =>
                        x >= topx andalso y >= topy andalso
                        x <= botx andalso y <= boty andalso
                        Polygon.pointinside p (x, y)) of
          SOME (a, _, _) => SOME a
        | NONE => NONE

  (* The only thing tricky is treating epsilon; in order to make sure
     that there are no sliver-sized gaps (or overlaps) between
     polygons we want to merge their vertices when they are close. We
     also want to be careful that the result is not influenced by any
     arbitrary decision (such as the order in which we process points
     or polygons) in the input.

     In particular, we want "within-epsilon" to be an equivalence
     relation on vertices. If we are not careful, we have a situation
     like
     
        * -e- * -e- * -e- *
        A     B     C     D

     Where A is epsilon away from B, B epsilon away from C, etc., but
     A is more than epsilon away from D. Depending on the order we
     insert these points and canonicalize them, we could get several
     different arrangements. Instead, we start by building equivalence
     classes of points. Each point starts in its own equivalence
     class. We then iterate over every point point p, enumerate the
     points p' close to p (but in different polygons), then force p
     and p' into the same equivalence class. Oh urg, this has a
     problem. Suppose we have



        A -e- B

     where A and B are in different polygons. So we union them.

        A =e= B

     and then we also have

        C -e- D

     which we union

        A =e= B
          \
           e
            \
        C =e= D

     oh check it out A and D are also within epsilon.

        A =e= B
          \\
           e
            \\
        C =e= D

     Oh oops, turns out A and C are vertices on the same polygon. Now
     A and C have been collapsed to the same point. If we want this to
     be insensitive to insertion order we really are forced to do
     that. OK so we do that and then if polygons become degenerate or
     non-simple then we raise an exception afterwards, fine.

     Once we've normalized to equivalence classes, we just pick a
     canonical coordinate for each class, and use that when we store
     the polygons. Done.
  *)
  fun locatorex eps polys =
      let
          

          (* This is so that we can *)
          val polys = Vector.fromList (map #2 polys)
      in

      end

  (* Not really reasonable to force some notion of "small" on the client,
     so never merge points. *)
  fun locator polys = locatorex 0.0 polys
      

end