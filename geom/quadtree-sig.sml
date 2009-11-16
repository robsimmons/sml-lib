
(* Set of points in the 2d plane that allows retrieval of all points
   that are within some distance of a query point. *)
signature QUADTREE =
sig

    type pos
    type dist

    (* Functional set of points with associated data of type 'a *)
    type 'a tree
        
    (* No points. *)
    val empty : 'a tree

    val insert : 'a tree -> 'a -> pos -> 'a tree

    val lookup : 'a tree -> pos -> dist -> 'a list
    val lookuppoint : 'a tree -> pos -> dist -> ('a * pos) list

    (* Gets a single point with minimum distance (there may be many points
       that are equidistant). Only fails if the tree is empty.
       NOTE: Currently, linear time! *)
    val closestpoint : 'a tree -> pos -> ('a * dist) option

    (* val bounds : 'a tree -> { minx : xpos, maxx : xpos, miny : ypos, maxy : ypos } *)

    val map : ('a -> 'b) -> 'a tree -> 'b tree
    val app : ('a -> unit) -> 'a tree -> unit
    val apppoint : ('a * pos -> unit) -> 'a tree -> unit

end

(* QuadtreeFn is parameterized over point positions and a metric
   between them.

   The Quadtree structure instantiates that functor with regular
   Euclidean distances (xpos = ypos = dist = real), since that is
   usually what you want. *)
signature QUADTREEARG =
sig

    (* Describe the position of a point; each point has an x position and
       a y position. For example, real numbers. *)
    type pos
    type xpos 
    type ypos
    val xpos : pos -> xpos
    val ypos : pos -> ypos
    val pos : xpos * ypos -> pos

    (* Distance between points. Must be a metric. For example, real numbers. *)
    type dist

    val xleq : xpos * xpos -> bool
    val yleq : ypos * ypos -> bool

    (* Subtract positions, e.g. subtraction on reals. *)
    val xsub : xpos * xpos -> xpos
    val ysub : ypos * ypos -> ypos

    (* Zero position, such that xsub(x, xzero) = x for all x. *)
    val xzero : xpos
    val yzero : ypos

    (* Must satisfy the triangle inequality. For example, Euclidean distance. *)
    val dist : pos -> pos -> dist

    (* xdist x1 x2
       Should be the same as dist (x1, yzero) (x2, yzero), but may be
       more efficient than calling dist. For example, (abs o -) on real
       numbers. *)
    val xdist : xpos * xpos -> dist
    val ydist : ypos * ypos -> dist
 
    (* dist_leq d1 d2 
       true if d1 is less than or equal to d2. 
       For example, <= on reals. *)
    val dleq : dist * dist -> bool

end