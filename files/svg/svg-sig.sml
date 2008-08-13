(* This is a very incomplete implementation of the SVG format.
   Specs and information are at the W3C site:
   http://www.w3.org/Graphics/SVG/

   Because SVG is an XML-based format, a proper implementation should
   be based around a proper XML parser. I don't have one of those in
   sml-lib yet, so until that exists all I'm implementing here are
   parsers for the string-literal parts of the SVG spec. So far this
   is just the strings that are used to represent paths. *)
signature SVG =
sig

    (* Raw path command. *)
    datatype pathcommand =
        (* Moveto (and subsequently, lineto) *)
        PC_M of (real * real) list
      | PC_m of (real * real) list
        (* Closepath *)
      | PC_z (* nb, Z is exactly equivalent; parsed to PC_z *)
        (* Lineto *)
      | PC_L of (real * real) list
      | PC_l of (real * real) list
        (* Horizontal and vertical lineto shortcuts *)
      | PC_H of real list
      | PC_h of real list
      | PC_V of real list
      | PC_v of real list
      (* Cubic Bezier. *)
      | PC_C of { x1 : real, y1 : real, x2 : real, y2 : real, x : real, y : real } list
      | PC_c of { x1 : real, y1 : real, x2 : real, y2 : real, x : real, y : real } list
      (* Smooth cubic Bezier shortcut *)
      | PC_S of { x2 : real, y2 : real, x : real, y : real } list
      | PC_s of { x2 : real, y2 : real, x : real, y : real } list
      (* Quadratic Bezier. *)
      | PC_Q of { x1 : real, y1 : real, x : real, y : real } list
      | PC_q of { x1 : real, y1 : real, x : real, y : real } list
      (* Smooth quadratic Bezier shortcut *)
      | PC_T of { x : real, y : real } list
      | PC_t of { x : real, y : real } list
      (* Elliptical arc *)
      | PC_A of { rx : real, ry : real, rot : real, large : bool, sweep : bool, 
                  x : real, y : real } list
      | PC_a of { rx : real, ry : real, rot : real, large : bool, sweep : bool, 
                  x : real, y : real } list

end