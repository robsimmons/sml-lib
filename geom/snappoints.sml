functor SnapPoints(structure A : SNAPPOINTSARG
                   structure Q : QUADTREE where type pos = A.pos and type dist = real) : SNAPPOINTS where type poly = A.poly =
struct

  type poly = A.poly
  type dist = real

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

     Here's an alternative: Take all pairs of points A and B on
     different polygons, where the distance from A to B is less than
     epsilon and strictly greater than 0. Sort the pairs. Merge the
     smallest distance (move in a globally constant direction). Repeat
     until there are no candidate points. It is not obvious that this
     terminates: Though we remove the pair from immediate consideration
     by making the points distance 0, this may cause A or B to become
     closer to a different point, and then merged with it. After merging
     we should fix both points permanently so that we never move them
     again (only move other nodes TO them). This is very similar now to
     the algorithm above, except that we are forced a certain order
     for A,B and C,D based on their distance (and ties broken by some
     global criteria) and refuse to merge two equivalence classes when
     the classes contain points from the same polygon.

     Actually, simpler still. Start with singleton equivalence
     classes. Merge compatible classes whose canonical representatives
     are within epsilon, in order sorted by distance (this includes
     zero-distances), and with ties broken by some global criteria
     (lexicographically, y coordinate then x coordinate). Set the
     canonical representative to the centroid of all merged points.
     (Or just to the smallest element?) This clearly always terminates
     because each step reduces the finite number of classes by one.
  *)
  fun snap epsilon (polys : ('a * poly) list) =
    let
        (* We're going to be messing with the points in the polygons.
           Remember each polygon's identity, by putting them in a
           vector. From now on the index into this vector will be
           the polygon's identity. *)
        val polys = Vector.fromList polys

        (* Each point's location is either native (because it is the
           canonical representative of its equivalence class) or it
           is in some other point's equivalence class. If that's the
           case, give the index of that other point. *)
        datatype loc =
            Native of A.pos
          | Via of int

        (* We also need the point locations. Each point needs to know its home
           polygon (as int), its winding number in that polygon (so
           that we can reconstruct it later; as an int), and its
           equivalence class (datatype loc).
           We ignore the associated data, which we zip back in later.
           *)
        val points =
            Vector.fromList
            (Vector.foldli (fn (polynum, (_, pts), b) =>
                            ListUtil.foldli (fn (pointnum, pt, bb) =>
                                             { loc = ref (Native pt),
                                               winding = pointnum,
                                               poly = polynum } :: bb) nil
                            (A.points pts)
                            @ b)
             nil polys)

        (* PERF path compression *)
        fun getloc { loc = ref (Native pt), ...} = pt
          | getloc { loc = ref (Via i), ... } = getloc (Vector.sub(points, i))

        fun canonical { loc = ref (Native _), winding = _, poly = _ } = true
          | canonical _ = false

        fun point i = Vector.sub(points, i)

        (* PERF: The kd-tree never changes, but we ignore points that are
           not canonical in the output. It would be faster if we could
           remove them. Maybe a second filtering pass once we know the
           canonicity? *)

        (* Insert in randomized order, since worst case is a series of
           close-by horizontal or vertical points. *)
        local
            val all_points = ref nil
            val () = Vector.appi
                (fn (idx, pt) => all_points := (idx, getloc pt) :: !all_points) points
            val all_points = Array.fromList (!all_points)
            val mt = MersenneTwister.init32 0wxCAFEBABE
            val () = MersenneTwister.shuffle mt all_points
        in
            val kdtree = Array.foldl (fn ((idx, pt), t) => Q.insert t idx pt) Q.empty all_points
        end

        (* Compare ordered pairs so that we a deterministic for any set of
           points, regardless of insertion order. This is a
           lexicographic order.
           First, the pair with shorter distance is smaller.
           For the same distance, the pair with the smaller first point
           is smaller. If that is the same point, then the pair with the
           smaller second point.
           *)
        fun comparepair ((a, b), (aa, bb)) =
        (* No need to take roots, because we're just comparing *)
            case Real.compare (A.dist (a, b), A.dist (aa, bb)) of
                EQUAL => (case A.compare (a, aa) of
                              EQUAL => A.compare (b, bb)
                            | order => order)
              | order => order

        fun comparepairidx ((i, j), (ii, jj)) =
            comparepair ((getloc (point i), getloc (point j)),
                         (getloc (point ii), getloc (point jj)))

        (* Modifies equivalence class refs until we're done. *)
        (* XXX It's not clear that this always preserves the well-formedness 
           of the polygons (like keeps them from being non-self-intersecting).
           Should at least check that the output is reasonable and raise an
           error if not. *)
        fun mergeloop () =
          let
              (* Start by getting all pairs. We only consider canonical
                 points in this algorithm, so the seeds are just those. *)
              val seeds = Vector.foldli
                  (fn (idx, pt, seeds) =>
                   if canonical pt
                   then (idx, pt) :: seeds
                   else seeds) nil points

              fun itspairs ((seed, { loc = ref (Native pt), poly, winding = _ }), b) =
                  let
                      val close = Q.lookup kdtree pt epsilon
                  in
                      (* Eliminate any that are not native. Also eliminate 
                         any that are in the same polygon. This will include
                         the point itself. *)
                      List.mapPartial (fn i =>
                                       let val pt = Vector.sub(points, i)
                                       in
                                           if canonical pt andalso
                                              #poly pt <> poly
                                           then SOME (seed, i)
                                           else NONE
                                       end) close @ b
                  end
                | itspairs (_, b) = b
          in
              (* Might be done? *)
              case foldr itspairs nil seeds of
                  nil => ()
                | pairs => 
                      let 
                          (*
                          val () = print "All pairs:\n";
                          fun printidx i =
                              let 
                                  val p as { poly, winding, ... } = point i
                                  val (x, y) = getloc p
                              in
                                  print (Int.toString i ^ ": " ^
                                         Real.fmt (StringCvt.FIX (SOME 3)) x ^ ", " ^
                                         Real.fmt (StringCvt.FIX (SOME 3)) y ^ ", poly " ^
                                         Int.toString poly ^ "." ^
                                         Int.toString winding)
                              end
                          val () = List.app (fn (i, ii) =>
                                             let in                                          
                                                 print "  ";
                                                 printidx i;
                                                 printidx ii;
                                                 print "\n"
                                             end) pairs
                          *)

                          val (p1, p2) = ListUtil.min comparepairidx pairs
                          (* Always merge to the left. *)
                          val (pl, pr) = 
                              case A.compare (getloc (point p1), getloc (point p2)) of
                                  LESS => (p1, p2)
                                | _ => (p2, p1)
                      in
                          (* print "merge.\n";*)
                          #loc (point pr) := Via pl;
                          mergeloop ()
                      end
          end

        (* After this, all points are merged. *)
        val () = mergeloop ()

        (* Now we reassemble the polygons. *)
        val points = Vector.foldr (fn (p as { poly, winding, ... }, b) =>
                                   (poly, (winding, getloc p)) :: b) nil points

        (* Chunk them by polygon again, and replace polygon index with original
           data. *)
        val polys : ('a * (int * A.pos) list) list =
            map (fn (idx, poly) =>
                 (#1 (Vector.sub(polys, idx)), poly))
            (ListUtil.stratify Int.compare points)

        (* Now sort by winding number *)
        val polys : ('a * (int * A.pos) list) list = 
            ListUtil.mapsecond (ListUtil.sort (ListUtil.byfirst Int.compare)) polys
        (* Discard that too *)
        val polys : ('a * A.poly) list =
            ListUtil.mapsecond (A.poly o map #2) polys

    in
        polys
    end

end

structure SnapPoly =
SnapPoints(structure A =
           struct
               type poly = Polygon.polygon
               type pos = real * real
               type dist = real
               val points = Polygon.points
               val poly = Polygon.frompoints

               (* Lexicographically. This biases one axis over the other
                  in a possibly strange way. Could consider first taking
                  the distance from the origin or something (?). *)
               fun compare ((x, y), (xx, yy)) =
                   case Real.compare (x, xx) of
                       EQUAL => Real.compare (y, yy)
                     | order => order

               fun dist ((x, y), (x', y')) =
                   let val dx = x - x'
                       val dy = y - y'
                   in
                       Math.sqrt (dx * dx + dy * dy)
                   end
           end : SNAPPOINTSARG
           structure Q = Quadtree)

structure SnapLatLon =
SnapPoints(structure A =
           struct
               type poly = LatLon.pos list
               type pos = LatLon.pos
               type dist = real
               fun points p = p
               fun poly p = p

               (* ? *)
               fun compare (p, pp) =
                   let val { lat = x, lon = y } = LatLon.todegs p
                       val { lat = xx, lon = yy } = LatLon.todegs p
                   in
                       case Real.compare (x, xx) of
                           EQUAL => Real.compare (y, yy)
                         | order => order
                   end
               val dist = LatLon.dist_meters
           end
           structure Q = LatLonTree)
