(* "Quadtree" doesn't seem to describe one specific data structure. I
   use the term suggestively here. The data structure is my own design
   (though I think obvious). (XXX I found out this is basically a
   kd-tree.) There are two kinds of trees: horizontally- and
   vertically-split. A node in each kind of tree is either empty or a
   point. If it is a point, then it has two children. For
   horizontally-split trees, the left child is a vertically-split tree
   with all points therein being to the west of (or colinear) with the
   point at this node; and the right child with all points (strictly)
   to the east. Vertically-split nodes have horizontal trees as
   children, but of course split into north and south parts. (I'm
   describing a y axis where north trends towards negative infinity,
   as is common in computer graphics. This doesn't affect the client
   interface in any way.)

   This structure doesn't attempt to be smart about the splitting
   direction; it is wasteful when all the points lie on a line, or
   when their horizontal and vertical dispersion is not the same.
   Also, like other naive binary trees, it is not necessarily balanced
   and so certain sequences of insertions will produce trees that have
   linear-time lookups. 

   XXX: It would be easy to generalize this to n-dimensions, though
   keeping the specialized two-dimensional one around is probably good
   for simplicity and efficiency.
*)
functor QuadtreeFn(Q : QUADTREEARG) :> 
sig
    include QUADTREE 
    where type pos = Q.pos
          and type dist = Q.dist 

    (* tosvg tree maxdepth proj print
       For visualization purposes. *)
    val tosvg : 'a tree -> int -> (pos -> real * real) -> (string -> unit) -> unit
end =
struct

    open Q

    datatype 'a horiztree =
        (* left, data, x, y, right *)
        HPoint of 'a verttree * 'a * xpos * ypos * 'a verttree
      | HEmpty
    and 'a verttree =
        (* up, data, x, y, down *)
        VPoint of 'a horiztree * 'a * xpos * ypos * 'a horiztree
      | VEmpty

    (* choice of horiz or vertical is basically arbitrary. *)
    type 'a tree = 'a horiztree
    val empty = HEmpty

    fun insert' HEmpty a (x, y) = HPoint(VEmpty, a, x, y, VEmpty)
      | insert' (HPoint (ll, aa, xx, yy, rr)) a (x, y) =
        if xleq(x, xx)
        then HPoint(vinsert' ll a (x, y), aa, xx, yy, rr)
        else HPoint(ll, aa, xx, yy, vinsert' rr a (x, y))

    and vinsert' VEmpty a (x, y) = VPoint(HEmpty, a, x, y, HEmpty)
      | vinsert' (VPoint (uu, aa, xx, yy, dd)) a (x, y) =
        if yleq(y, yy)
        then VPoint(insert' uu a (x, y), aa, xx, yy, dd)
        else VPoint(uu, aa, xx, yy, insert' dd a (x, y))

    fun insert t a pt = insert' t a (xpos pt, ypos pt)

    (* The thing that makes this tricky is that the distance d forces us
       to look on both sides of some splits, when the target
       point is close to the split axis. It's real easy to
       test for this case though. *)
    fun lookuppoint' HEmpty _ _ = nil
      | lookuppoint' (HPoint (ll, aa, xx, yy, rr)) (x : xpos, y : ypos) (d : dist) =
        (* Be a little smart. First do the axis test, which we have to do no
           matter what. If the query point is not close to the axis,
           then we don't need to do the closeness test for the tree
           point, because it will never succeed. *)
        let val dx : xpos = xsub(x, xx)
            fun close () =
                let val pt = pos (x, y)
                    val ppt = pos (xx, yy)
                    val res = vlookuppoint' ll (x, y) d @ vlookuppoint' rr (x, y) d 
                in
                    if dleq (dist pt ppt, d)
                    then (aa, ppt) :: res
                    else res
                end
        in
            if xleq(dx, xzero)
            then 
                (* on the left, or colinear *)
                (if dleq(xdist(dx, xzero), d)
                 then close ()
                 else vlookuppoint' ll (x, y) d)
            else 
                (* on the right *)
                (if dleq(xdist(dx, xzero), d)
                 then close ()
                 else vlookuppoint' rr (x, y) d)
        end

    and vlookuppoint' VEmpty _ _ = nil
      | vlookuppoint' (VPoint (uu, aa, xx, yy, dd)) (x, y) d =
        let val dy : ypos = ysub(y, yy)
            fun close () =
                let val pt = pos (x, y)
                    val ppt = pos (xx, yy)
                    val res = lookuppoint' uu (x, y) d @ lookuppoint' dd (x, y) d 
                in
                    if dleq (dist pt ppt, d)
                    then (aa, ppt) :: res
                    else res
                end
        in
            if yleq(dy, yzero)
            then 
               (* above, or colinear *)
                (if dleq(ydist(dy, yzero), d)
                 then close ()
                 else lookuppoint' uu (x, y) d)
            else 
               (* below *)
                (if dleq(ydist(dy, yzero), d)
                 then close ()
                 else lookuppoint' dd (x, y) d)
        end

    fun lookuppoint q p d = lookuppoint' q (xpos p, ypos p) d

    fun lookup q p d = map #1 (lookuppoint q p d)

    fun map (f : 'a -> 'b) q =
        let
            fun mhoriz HEmpty = HEmpty
              | mhoriz (HPoint (l, a, x, y, r)) = HPoint (mvert l, f a, x, y, mvert r)
            and mvert VEmpty = VEmpty
              | mvert (VPoint (l, a, x, y, r)) = VPoint (mhoriz l, f a, x, y, mhoriz r)
        in
            mhoriz q
        end

    fun app (f : 'a -> unit) q : unit =
        let
            fun mhoriz HEmpty = ()
              | mhoriz (HPoint (l, a, _, _, r)) = (mvert l; f a; mvert r)
            and mvert VEmpty = ()
              | mvert (VPoint (l, a, _, _, r)) = (mhoriz l; f a; mhoriz r)
        in
            mhoriz q
        end

    fun apppoint (f : ('a * pos) -> unit) q : unit =
        let
            fun mhoriz HEmpty = ()
              | mhoriz (HPoint (l, a, x, y, r)) = (mvert l; f (a, pos (x, y)); mvert r)
            and mvert VEmpty = ()
              | mvert (VPoint (l, a, x, y, r)) = (mhoriz l; f (a, pos (x, y)); mhoriz r)
        in
            mhoriz q
        end

    (* PERF!!! This can obviously be made much faster.
       When we have a candidate point and distance, we only want to
       consider things within that radius.

       Also: This should be generalized to n-closest. Accumulate all points
       until we have n. Then, only consider those points that are closer
       than the maximum distance in our current set of n; recurse. *)
    fun closestpoint HEmpty _ = NONE
      | closestpoint (HPoint (l, a, x, y, r)) targpt =
        let
            val (targx, targy) = (xpos targpt, ypos targpt)
            fun hclosest (cand, cand_dist) HEmpty = (cand, cand_dist)
              | hclosest (cand, cand_dist) (HPoint (l, a, x, y, r)) =
                let 
                    val newdist = dist (pos (x, y)) (pos (targx, targy))
                    val best = 
                        if dleq (newdist, cand_dist)
                        then (a, newdist)
                        else (cand, cand_dist)

                    val best = vclosest best l
                    val best = vclosest best r
                in
                    best
                end
            and vclosest (cand, cand_dist) VEmpty = (cand, cand_dist)
              | vclosest (cand, cand_dist) (VPoint (l, a, x, y, r)) =
                let 
                    val newdist = dist (pos (x, y)) (pos (targx, targy))
                    val best = 
                        if dleq (newdist, cand_dist)
                        then (a, newdist)
                        else (cand, cand_dist)

                    val best = hclosest best l
                    val best = hclosest best r
                in
                    best
                end
                
            val best = (a, dist (pos (x, y)) (pos (targx, targy)))

            val best = vclosest best l
            val best = vclosest best r
        in
            SOME best
        end

    (* Emits SVG for debugging/visualization. Superfluous except for that
       purpose. *)
    fun tosvg h maxdepth (proj : pos -> real * real) print =
        let
            (* choose the stroke width based on the depth remaining. *)
            fun depthwidth depthleft =
                let val pct = real depthleft / real maxdepth
                    val MAXWIDTH = 2.0
                    val MINWIDTH = 0.1
                    val w = MINWIDTH + (pct * (MAXWIDTH - MINWIDTH))
                in
                    Real.fmt (StringCvt.FIX (SOME 3)) w
                end

            (* No exponential notation *)
            fun ertos r = if (r > ~0.000001 andalso r < 0.000001) 
                          then "0.0" 
                          else (Real.fmt (StringCvt.FIX (SOME 6)) r)

            (* Don't use SML's dumb ~ *)
            fun rtos r = if r < 0.0 
                         then "-" ^ ertos (0.0 - r)
                         else ertos r


            (* First get scale so that all reals are between 0 and 1000. *)
            val (bounding_box as { minx, miny, maxx, maxy }) =
                let
                    val maxx = ref (~1.0 / 0.0)
                    val minx = ref (1.0 / 0.0)
                    val maxy = ref (~1.0 / 0.0)
                    val miny = ref (1.0 / 0.0)
              
                    fun bound p min max =
                        let in
                            if p < !min then min := p else ();
                            if p > !max then max := p else ()
                        end
                    
                    fun onepoint (a, pt) =
                        let val (xx, yy) = proj pt
                        in bound xx minx maxx;
                           bound yy miny maxy
                        end
                in
                    apppoint onepoint h;
                    { minx = !minx, maxx = !maxx, miny = !miny, maxy = !maxy }
                end

                
            val xoffset = ~minx
            val yoffset = ~miny
            val scale = if (maxx - minx) > (maxy - miny)
                        then 1000.0 / (maxx - minx)
                        else 1000.0 / (maxy - miny)

            fun projmap pt =
                let val (x, y) = proj pt
                in ((x + xoffset) * scale,
                    (y + yoffset) * scale)
                end

            val FACTOR = 0.95
            fun hrec _ _ HEmpty = ()
              | hrec 0 _ _ = ()
              | hrec depth { minx, miny, maxx, maxy } (HPoint (l, a, x, y, r)) =
                let 
                    val (x, y) = projmap (pos (x, y))
                    val actual_len = maxy - miny
                    val margin = ((1.0 - FACTOR) / 2.0) * actual_len
                in
                    (* draw vertical axis, at 95% of its actual height. *)
                    print ("<polyline fill=\"none\" opacity = \"0.9\" " ^
                           "stroke=\"#AA0000\" stroke-width=\"" ^
                           depthwidth depth ^ "\" points=\""); (* " *)
                    print (rtos x ^ "," ^ rtos (miny + margin) ^ " " ^
                           rtos x ^ "," ^ rtos (maxy - margin));
                    print "\"/>\n" (* " *);
                    vrec (depth - 1) { minx = minx, maxx = x, miny = miny, maxy = maxy } l;
                    vrec (depth - 1) { minx = x, maxx = maxx, miny = miny, maxy = maxy } r
                end

            and vrec _ _ VEmpty = ()
              | vrec 0 _ _ = ()
              | vrec depth { minx, miny, maxx, maxy } (VPoint (u, a, x, y, d)) =
                let 
                    val (x, y) = projmap (pos (x, y))
                    val actual_len = maxx - minx
                    val margin = ((1.0 - FACTOR) / 2.0) * actual_len
                in
                    (* draw horizontal axis, at 95% of its actual width. *)
                    print ("<polyline fill=\"none\" opacity = \"0.9\" " ^
                           "stroke=\"#00AA00\" stroke-width=\"" ^ 
                           depthwidth depth ^ "\" points=\""); (* " *)
                    print (rtos (minx + margin) ^ "," ^ rtos y ^ " " ^
                           rtos (maxx - margin) ^ "," ^ rtos y);
                    print "\"/>\n" (* " *);
                    hrec (depth - 1) { minx = minx, maxx = maxx, miny = miny, maxy = y } u;
                    hrec (depth - 1) { minx = minx, maxx = maxx, miny = y, maxy = maxy } d
                end


        in
            print "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n";
            print "<!-- Generator: pointlocation.sml -->\n";
            print ("<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" " ^
                   "\"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\" [\n");
            print "<!ENTITY ns_flows \"http://ns.adobe.com/Flows/1.0/\">\n";
            print "]>\n";
            print "<svg version=\"1.1\"\n";
            print (" xmlns=\"http://www.w3.org/2000/svg\" " ^
                   "xmlns:xlink=\"http://www.w3.org/1999/xlink\"\n");
            print " xmlns:a=\"http://ns.adobe.com/AdobeSVGViewerExtensions/3.0/\"\n";
            (* XXX derive doc coordinates from input *)
            print " x=\"0px\" y=\"0px\" width=\"1000px\" height=\"1000px\"\n";
            print " xml:space=\"preserve\">\n";
            hrec maxdepth bounding_box h;
            print "</svg>\n"
        end
end

structure Quadtree = QuadtreeFn(
  type xpos = real
  type ypos = real
  type pos = xpos * ypos
  type dist = real
  fun xpos (x, _) = x
  fun ypos (_, y) = y
  fun pos pt = pt
  val xleq : xpos * xpos -> bool = op <=
  val yleq : ypos * ypos -> bool = op <=
  val xsub : xpos * xpos -> xpos = op -
  val ysub : ypos * ypos -> ypos = op -
  val xzero = 0.0
  val yzero = 0.0
  fun dist (x, y) (xx, yy) = 
      Math.sqrt ((x - xx) * (x - xx) + (y - yy) * (y - yy))
  fun xdist (x1, x2) = Real.abs(x1 - x2)
  fun ydist (y1, y2) = Real.abs(y1 - y2)
  val dleq : dist * dist -> bool = op <=)
