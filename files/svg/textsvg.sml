
structure TextSVG :> TEXTSVG =
struct

  datatype tree = datatype XML.tree
  exception TextSVG of string

  type svggraphic = { (* XML re-rendered *)
                      body : string,
                      (* from <svg> tag *)
                      width : real,
                      height : real }

  (* Trailing zeroes don't change the nature of decimal numbers *)
  fun striptail s =
      if CharVector.exists (StringUtil.ischar #".") s
      then StringUtil.losespecr (StringUtil.ischar #"0") s
      else s

  (* No exponential notation *)
  fun ertos r = if (r > ~0.000001 andalso r < 0.000001) 
                then "0.0" 
                    (* XXX using 8 for kml, was 4 for svg *)
                else (striptail (Real.fmt (StringCvt.FIX (SOME 8)) r))

  (* Don't use SML's dumb ~ *)
  fun rtos r = if r < 0.0 
               then "-" ^ ertos (0.0 - r)
               else ertos r

  fun loadgraphic file =
      let 
          val losewhites = StringUtil.losespecsides StringUtil.whitespec
          fun process (Elem (("svg", attrs), body)) =
              let
                  fun getpx s =
                    case ListUtil.Alist.find op= attrs s of
                        NONE => raise TextSVG ("expected to find a '" ^ s ^ 
                                              "' attr")
                      | SOME px =>
                         let val px = losewhites px
                         in
                             if StringUtil.matchtail "px" px
                             then (case Real.fromString px of
                                       NONE => raise TextSVG "non-numeric px dimension?"
                                     | SOME f => f)
                             else raise TextSVG "can only parse dimensions in px form"
                         end

                  val (width, height) = (getpx "width", getpx "height")
                  val (x, y) = (getpx "x", getpx "y")
                  val _ = (Real.== (x, 0.0) andalso Real.== (y, 0.0)) orelse
                      raise TextSVG "For now, only x=0 and y=0 svgs are supported"

                  (* re-render as a string. *)
                  val body =
                      case body of
                          [one] => XML.tostring one
                        (* PERF don't need to create a group, since we'll place
                           the graphic inside a group when we render. Could just
                           concat all the subtree strings? *)
                        | many => XML.tostring (Elem(("g", nil), body))
              in
                  { width = width, height = height, body = body }
              end
            | process _ = 
              raise TextSVG "expected SVG file to be a single <svg> tag."

          val x = XML.parsefile file
              handle XML.XML s => 
                  raise TextSVG ("Couldn't parse " ^ file ^ "'s xml: " ^ s)

      in
          process x
      end

  fun graphicsize { body = _, width, height } = (width, height)

  fun placegraphic { graphic = {body, width, height}, x, y, scale, rotate } =
      let
          fun doscale NONE s = s
            | doscale (SOME r) s = "<g transform=\"scale(" ^ rtos r ^ ")\">" ^ s ^ "</g>"
          fun dorotate NONE s = s
            | dorotate (SOME r) s = "<g transform=\"rotate(" ^ rtos r ^ ")\">" ^ s ^ "</g>"
          fun dotranslate (x, y) s = 
              "<g transform=\"translate(" ^ rtos x ^ " " ^ rtos y ^ ")\">" ^ s ^ "</g>"
      in
          dotranslate (x, y) (doscale scale (dorotate rotate body))
      end

  fun svgheader { x, y, width, height, generator } =
      let in
          "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n" ^
          (if generator = ""
           then ""
           else "<!-- Generator: " ^ generator ^ " -->\n") ^

          "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" " ^
          "\"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\" [\n" ^

          "<!ENTITY ns_flows \"http://ns.adobe.com/Flows/1.0/\">\n]>\n" ^
          
          "<svg version=\"1.1\"\n" ^
          " xmlns=\"http://www.w3.org/2000/svg\"" ^
          " xmlns:xlink=\"http://www.w3.org/1999/xlink\"\n" ^
          " xmlns:a=\"http://ns.adobe.com/AdobeSVGViewerExtensions/3.0/\"\n" ^
          " x=\"" ^ Int.toString x ^ "px\"" ^
          " y=\"" ^ Int.toString y ^ "px\"" ^
          " width=\"" ^ Int.toString width ^ "px\"" ^
          " height=\"" ^ Int.toString height ^ "px\"\n" ^
          " xml:space=\"preserve\">\n"
      end

  fun svgfooter () = "</svg>\n"
      
  type svgtext = (string * string) list
  fun svgtext { x, y, face, size = fontsize, text } = 
      let 
          fun onetext (color, s) =
              (* XXX need to escape for SVG. *)
              "<tspan fill=\"" ^ color ^ "\">" ^ s ^ "</tspan>"
      in
          "<text x=\"" ^ rtos x ^ "\" y=\"" ^ rtos y ^ "\" font-family=\"" ^
          face ^ "\" font-size=\"" ^ rtos fontsize ^ "\">" ^
          String.concat (map onetext text) ^
          "</text>"
      end
end