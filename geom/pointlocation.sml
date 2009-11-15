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
                        Polygon.pointinside p (x, y)) v of
          SOME (a, _, _) => SOME a
        | NONE => NONE


  (* Lexicographically. This biases one axis over the other
     in a possibly strange way. Could consider first taking
     the distance from the origin or something (?). *)
  fun comparepoint ((x, y), (xx, yy)) =
      case Real.compare (x, xx) of
          EQUAL => Real.compare (y, yy)
        | order => order

  fun dist ((x, y), (x', y')) =
      let val dx = x - x'
          val dy = y - y'
      in
          Math.sqrt (dx * dx + dy * dy)
      end

  fun locator polys =
      Vector.fromList
      (map (fn (data, poly) => (data, Polygon.boundingbox poly, poly)) polys)

  fun interior _ = raise PointLocation "unimplemented"

  (* All of the following is for rendering the data structure as SVG. It
     is superfluous except for debugging purposes. *)
  local 
      (* No exponential notation *)
      fun ertos r = if (r > ~0.000001 andalso r < 0.000001) 
                    then "0.0" 
                    else (Real.fmt (StringCvt.FIX (SOME 4)) r)

      (* Don't use SML's dumb ~ *)
      fun rtos r = if r < 0.0 
                   then "-" ^ ertos (0.0 - r)
                   else ertos r

      datatype 'a set = Empty | Node of 'a set * 'a * 'a set
      fun fromlist nil = Empty
        | fromlist (h :: t) =
          let val (l, r) = List.partition (fn x => case comparepoint (x, h) of 
                                           LESS => true
                                         | _ => false) t
          in Node (fromlist l, h, fromlist r)
          end
      fun count Empty x = 0
        | count (Node (l, y, r)) x = 
          case comparepoint (x, y) of
              (* PERF only one of these is necessary, I think. *)
              EQUAL => 1 + count l x + count r x
            | LESS => count l x
            | GREATER => count r x
  in
      fun tosvg locator print =
          let 
              val set = fromlist (Vector.foldr (fn ((_, _, poly), b) =>
                                                Polygon.points poly @ b) nil locator)
              fun printpolygon (_, _, poly) =
                  let val pts = Polygon.points poly
                      val pts = pts @ [hd pts]
                  in
                      print ("<polyline fill=\"none\" opacity=\"0.6\" stroke=\"#000000\" stroke-width=\"0.5\" points=\""); (* " *)
                      List.app (fn (x, y) =>
                                print (rtos x ^ "," ^ rtos y ^ " ")) pts;
                      print "\"/>\n" (* " *)
                  end

              fun printboxes (_, { topx, topy, botx, boty }, _) =
                  let
                  in
                      print ("<polyline fill=\"none\" opacity = \"0.4\" stroke=\"#AA0000\" stroke-width=\"0.5\" points=\""); (* " *)
                      print (rtos topx ^ "," ^ rtos topy ^ " " ^
                             rtos botx ^ "," ^ rtos topy ^ " " ^
                             rtos botx ^ "," ^ rtos boty ^ " " ^
                             rtos topx ^ "," ^ rtos boty ^ " " ^
                             rtos topx ^ "," ^ rtos topy);
                      print "\"/>\n" (* " *)
                  end

          in
              print "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n";
              print "<!-- Generator: pointlocation.sml -->\n";
              print ("<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"" ^
                     " \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\" [\n");
              print "<!ENTITY ns_flows \"http://ns.adobe.com/Flows/1.0/\">\n";
              print "]>\n";
              print "<svg version=\"1.1\"\n";
              print (" xmlns=\"http://www.w3.org/2000/svg\"" ^
                     " xmlns:xlink=\"http://www.w3.org/1999/xlink\"\n");
              print " xmlns:a=\"http://ns.adobe.com/AdobeSVGViewerExtensions/3.0/\"\n";
              (* XXX derive doc coordinates from input *)
              print " x=\"0px\" y=\"0px\" width=\"100px\" height=\"100px\"\n";
              print " xml:space=\"preserve\">\n";
              Vector.app printpolygon locator;
              Vector.app printboxes locator;
              print "</svg>\n"
          end

  end

end