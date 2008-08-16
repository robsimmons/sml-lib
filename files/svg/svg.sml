structure SVG =
struct

  exception SVG of string
  open Parsing

  infixr 4 << >>
  infixr 3 &&
  infix  2 -- ##
  infix  2 wth suchthat return guard when
  infixr 1 ||

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

  structure Path =
  struct

    fun ch c = satisfy (fn x => x = c)

    (* This parser is written to follow the BNF in the spec as closely as I can bear,
       which often means copying some really long names. *)
    val flag = ch #"0" return false || ch #"1" return true
    val sign = satisfy (Char.contains "+-")
    val opt_sign = ch #"+" || ch #"-" || succeed #"+"
    val digit = satisfy (Char.contains "0123456789")
    val digit_sequence = repeat1 digit
    val opt_digit_sequence = digit_sequence || succeed [#"0"]
    val exponent = (ch #"e" || ch #"E") >> opt_sign && digit_sequence
    val wsp = satisfy (Char.contains (implode (map chr [0x20, 0x9, 0xD, 0xA])))
    val comma = ch #","
    val comma_wsp = repeat1 wsp && opt comma && repeati wsp return ()
                 || comma && repeati wsp return ()
    val comma_wspq = opt comma_wsp
    val fractional_constant =
        (* PERF looks like this can backtrack *)
        alt [opt_digit_sequence && (ch #"." >> digit_sequence),
             digit_sequence && (ch #"." >> succeed [#"0"])]

    val floating_point_constant =
        alt [fractional_constant && opt exponent,
             (digit_sequence && succeed [#"0"]) && (exponent wth SOME)]
        (* iii.fffEeee *)
        -- (fn ((i, f), e) => 
            let val s = implode i ^ "." ^ implode f
                val s = case e of 
                    NONE => s 
                  | SOME (sgn, pow) => s ^ "e" ^ implode [sgn] ^ implode pow
            in
                case Real.fromString s of
                    NONE => fail
                  | SOME r => succeed r
            end)

    val integer_constant = digit_sequence -- (fn l =>
                                              case Int.fromString (implode l) of
                                                  NONE => fail
                                                | SOME i => succeed i)

    val number = opt_sign && integer_constant wth (fn (#"-", i) => real (~i)
                                                    | (_,    i) => real i)
              || opt_sign && floating_point_constant wth (fn (#"-", r) => ~r
                                                           | (_,    r) => r)

    val nonnegative_number = integer_constant wth real || floating_point_constant
    val coordinate = number
    val coordinate_pair = (coordinate << comma_wspq) && coordinate

    val elliptical_arc_argument =
        (nonnegative_number << comma_wspq) &&
        (nonnegative_number << comma_wspq) &&
        (number << comma_wsp) &&
        (flag << comma_wsp) &&
        (flag << comma_wsp) &&
        coordinate_pair
        wth (fn (rx, (ry, (rot, (large, (sweep, (x, y)))))) =>
             { rx = rx, ry = ry, rot = rot, large = large, sweep = sweep,
               x = x, y = y })
    val elliptical_arc_argument_sequence = separate elliptical_arc_argument comma_wspq
    val elliptical_arc = 
        alt [ch #"A" >> repeati wsp >> elliptical_arc_argument_sequence wth PC_A,
             ch #"a" >> repeati wsp >> elliptical_arc_argument_sequence wth PC_a]

    val smooth_quadratic_bezier_curveto_argument_sequence = 
        separate (coordinate_pair wth (fn (x, y) => { x = x, y = y })) comma_wspq
    val smooth_quadratic_bezier_curveto =
        alt [ch #"T" >> repeati wsp >> 
             smooth_quadratic_bezier_curveto_argument_sequence wth PC_T,
             ch #"t" >> repeati wsp >> 
             smooth_quadratic_bezier_curveto_argument_sequence wth PC_t]

    val quadratic_bezier_curveto_argument =
        (coordinate_pair << comma_wspq) && coordinate_pair
        wth (fn ((a, b), (c, d)) =>
             { x1 = a, y1 = b, x = c, y = d })
    val quadratic_bezier_curveto_argument_sequence = 
        separate quadratic_bezier_curveto_argument comma_wspq
    val quadratic_bezier_curveto =
        alt [ch #"Q" >> repeati wsp >> quadratic_bezier_curveto_argument_sequence wth PC_Q,
             ch #"Q" >> repeati wsp >> quadratic_bezier_curveto_argument_sequence wth PC_q]

    val smooth_curveto_argument =
        (coordinate_pair << comma_wspq) && coordinate_pair
        wth (fn ((a, b), (c, d)) =>
             { x2 = a, y2 = b, x = c, y = d })
    val smooth_curveto_argument_sequence = separate smooth_curveto_argument comma_wspq
    val smooth_curveto =
        alt [ch #"S" >> repeati wsp >> smooth_curveto_argument_sequence wth PC_S,
             ch #"s" >> repeati wsp >> smooth_curveto_argument_sequence wth PC_s]

    val curveto_argument =
        (coordinate_pair << comma_wspq) &&
        (coordinate_pair << comma_wspq) &&
        coordinate_pair wth (fn ((x1, y1), ((x2, y2), (x, y))) =>
                             { x1 = x1, y1 = y1, x2 = x2, y2 = y2, x = x, y = y })
    val curveto_argument_sequence = separate curveto_argument comma_wspq
    val curveto =
        alt [ch #"C" >> repeati wsp >> curveto_argument_sequence wth PC_C,
             ch #"c" >> repeati wsp >> curveto_argument_sequence wth PC_c]

    val vertical_lineto_argument_sequence = separate coordinate comma_wspq
    val vertical_lineto = 
        alt [ch #"V" >> repeati wsp >> vertical_lineto_argument_sequence wth PC_V,
             ch #"v" >> repeati wsp >> vertical_lineto_argument_sequence wth PC_v]

    val horizontal_lineto_argument_sequence = separate coordinate comma_wspq
    val horizontal_lineto = 
        alt [ch #"H" >> repeati wsp >> horizontal_lineto_argument_sequence wth PC_H,
             ch #"h" >> repeati wsp >> horizontal_lineto_argument_sequence wth PC_h]

    val lineto_argument_sequence = separate coordinate_pair comma_wspq
    val lineto =
        alt [ch #"L" >> repeati wsp >> lineto_argument_sequence wth PC_L,
             ch #"l" >> repeati wsp >> lineto_argument_sequence wth PC_l]

    val closepath = (ch #"Z" || ch #"z") return PC_z

    val moveto_argument_sequence = separate coordinate_pair comma_wspq
    val moveto =
        alt [ch #"M" >> repeati wsp >> moveto_argument_sequence wth PC_M,
             ch #"m" >> repeati wsp >> moveto_argument_sequence wth PC_m]

    val drawto_command =
        alt [closepath, lineto, horizontal_lineto, vertical_lineto, curveto,
             smooth_curveto, quadratic_bezier_curveto, 
             smooth_quadratic_bezier_curveto, elliptical_arc]

    val drawto_commandsq = separate0 drawto_command (repeati wsp)
    val moveto_drawto_command_group = (moveto << repeati wsp) && drawto_commandsq wth op::

    val svg_path_prefix = 
        repeati wsp >> (separate0 moveto_drawto_command_group (repeati wsp)) << repeati wsp
    val svg_path = 
        svg_path_prefix << done()

  end

  val parsepath = Path.svg_path

  fun stringstream s =
    let
      val ss = size s
      fun next n () = 
        if n >= ss
        then Stream.empty
        else Stream.lcons (CharVector.sub(s, n),
                           next (n + 1))
    in
      Stream.old_delay (next 0)
    end

  fun parsepathstring s = Parsing.parse parsepath (Pos.markstream (stringstream s))


        
end
