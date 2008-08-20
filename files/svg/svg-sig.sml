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

    exception SVG of string

    (* Raw path command. Uppercase means absolute, lowercase means relative. *)
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
        
    (* Path commands are designed to compactly represent drawing intent as
       text strings. For programs, it's better to only have to
       implement a compact set of primitives. This normalizes commands
       so that they are all in relative coordinates (except the first required
       moveto command, which if it is absolute we have nothing to relativize it
       with respect to.), do not have repetitions, and use the minimal set of
       primitives. *)
    datatype normalizedcommand =
        PC_Move of real * real
      | PC_Line of real * real
      | PC_Close
      | PC_Cubic of { x1 : real, y1 : real, x2 : real, y2 : real, x : real, y : real }
      | PC_Quad of { x1 : real, y1 : real, x : real, y : real }
      | PC_Arc of { rx : real, ry : real, rot : real, large : bool, sweep : bool, 
                    x : real, y : real }

    datatype normalizedpath =
        P_Empty
        (* Either way, the normalized commands are all relative to the first
           moveto. *)
      | P_Absolute of real * real * normalizedcommand list
      | P_Relative of real * real * normalizedcommand list

    val parsepath : (pathcommand list, char) Parsing.parser
    val parsepathstring : string -> pathcommand list option
end