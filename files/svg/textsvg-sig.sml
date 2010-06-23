(* Often you just don't want to deal with all the heavyweight stuff in
   SVG. This is a dead-simple string-based SVG writer, which allows
   you to paste together fly-by-night SVG files. *)
signature TEXTSVG =
sig

  exception TextSVG of string

  (* Render a real as a string compatible with SVG syntax. Doesn't use ~ for
     negative numbers, and avoids expontential notation for very small numbers. 
     Can lose precision. *)
  val rtos : real -> string

  (* Loads SVG graphic from disk. Only supports an unspecified subset of SVG;
     recommended "save for web" in Illustrator with a conservative settings. *)
  type svggraphic
  val loadgraphic : string -> svggraphic
  (* Optionally scale it (as multiplicative factor), and rotate (as degrees). *)
  val placegraphic : { graphic : svggraphic, x : real, y : real, 
                       scale : real option, rotate : real option } -> string
  val graphicsize : svggraphic -> real * real


  (* Give viewport. Generator is a freeform html-safe string; ignored if blank. *)
  val svgheader : { x : int, y : int, 
                    width : int, height : int, 
                    generator : string } -> string
  val svgfooter : unit -> string


  (* e.g. [("#FFFFFF", "this text is "), ("#0000FF", "blue")] *)
  type svgtext = (string * string) list
  val svgtext : { x : real, y : real, face : string, size : real, 
                  text : svgtext } -> string

end