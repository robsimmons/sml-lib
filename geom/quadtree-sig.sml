
(* Set of points in the 2d plane that allows retrieval of all points
   that are within some distance of a query point. *)
signature QUADTREE =
sig

    type xpos
    type ypos
    type dist

    (* Functional set of points with associated data of type 'a *)
    type 'a quadtree
        
    (* No points. *)
    val empty : 'a quadtree

    val insert : 'a quadtree -> 'a -> xpos * ypos -> 'a quadtree

    val lookup : 'a quadtree -> xpos * ypos -> dist -> 'a list
    val lookuppoint : 'a quadtree -> xpos * ypos -> dist -> ('a * xpos * ypos) list

    val map : ('a -> 'b) -> 'a quadtree -> 'b quadtree

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
    type xpos 
    type ypos

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
    val dist : (xpos * ypos) -> (xpos * ypos) -> dist

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