
structure Polygon :> POLYGON =
struct

  exception Polygon of string

  (* invt: always 3 or more points *)
  type polygon = (real * real) vector

  fun points v = Vector.foldr op:: nil v

  fun frompoints l =
      let
          val v = Vector.fromList l
      in
          if Vector.length v < 3
          then raise Polygon "Need at least points to make a polygon."
          else v
      end

  (* The point-inside-polygon algorithm is a reimplementation of a short
     piece of C code covered by the following license (BSD-style). I
     (Tom) do not claim any copyright, or if you prefer, relinquish
     any rights to the public domain. If you think that this
     provenance makes it a derivative work, then you ought to abide by
     its terms:

     Copyright (c) 1970-2003, Wm. Randolph Franklin

     Permission is hereby granted, free of charge, to any person
     obtaining a copy of this software and associated documentation
     files (the "Software"), to deal in the Software without
     restriction, including without limitation the rights to use,
     copy, modify, merge, publish, distribute, sublicense, and/or sell
     copies of the Software, and to permit persons to whom the
     Software is furnished to do so, subject to the following
     conditions:

     1. Redistributions of source code must retain the above copyright
        notice, this list of conditions and the following disclaimers.

     2. Redistributions in binary form must reproduce the above
        copyright notice in the documentation and/or other materials
        provided with the distribution.

     3. The name of W. Randolph Franklin may not be used to endorse or
        promote products derived from this Software without specific
        prior written permission.

     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
     EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
     OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
     NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
     HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
     WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
     FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
     OTHER DEALINGS IN THE SOFTWARE. *)
   fun pointinside (p : polygon) (x, y) =
       let
           fun xcoord i = #1 (Vector.sub(p, i))
           fun ycoord i = #2 (Vector.sub(p, i))
           val nvert = Vector.length p
           fun loop odd idx jdx =
               if idx = nvert
               then odd
               else loop (if ((ycoord idx > y) <> (ycoord jdx > y)) andalso
                              (x < ((xcoord jdx - xcoord idx) * (y - ycoord idx) / 
                                    (ycoord jdx - ycoord idx) + xcoord idx))
                          then not odd
                          else odd) (idx + 1) idx
       in
           loop false 0 (nvert - 1)
       end

   fun boundingbox p =
       Vector.foldl (fn ((x, y), { topx, topy, botx, boty }) => 
                     { topx = Real.min (topx, x),
                       topy = Real.min (topy, y),
                       botx = Real.max (botx, x),
                       boty = Real.max (boty, y) })
       (* This would be an illegal result, but we're guaranteed to
          fix it once we see a point, which the polygon must have
          at least 3 of. *)
       { topx = Real.posInf, topy = Real.posInf,
         botx = Real.negInf, boty = Real.negInf } p

end