(* Implements PEC files, a common Brother/Babylock/Bernina embroidery
   format. Only supports stitch data.

   The file parsing is a fairly straightforward translation of Nathan
   Crawford's C# code (for PES files, which works by just finding the
   embedded PEC file) into SML. That code is licensed under the GPL,
   so this code is, too. See the file COPYING in the root of sml-lib.
   Nathan's Embroidery Reader software can be found at
    http://www.njcrawford.com/programs/embroidery-reader/
*)

structure PEC :> PEC =
struct

  exception PEC of string
  open Reader
  structure GA = GrowArray
      
  type stitchblock =
      { colorindex : int,
        stitches : (int * int) vector }

  type pecfile = 
      (* Could parse the name too, which comes in a fixed-length field right
         after the magic. But I can't figure out what the deal is with
         that part of the format: If the name is long, it appears that
         it actually jumps over the color block, and that some
         characters can be lost. Or maybe this is just a bug in the
         writer? *)
      { blocks : stitchblock vector }

  (* Call with the reader positioned at the start of the PEC data,
     which is usually the name of the design. *)
  (* Port note: C# BinaryReader is all little-endian. *)
  fun readpecbody (r : reader) : pecfile =
    let
      val pecstart = #pos r ()
      val () = #seek r (pecstart + 48)
          handle Bounds => raise PEC "PEC color section out of bounds?"

      val numcolors = r8 r + 1
      val colorlist = Vector.tabulate (numcolors, (fn _ => r8 r))

      val () = #seek r (pecstart + 532)
          handle Bounds => raise PEC "PEC stitch block out of bounds?"

      val blocks = GA.empty()
      fun addblock (stitches, colornum) =
          let
              val colorindex = 
                  Vector.sub(colorlist, colornum)
                  handle _ => raise PEC ("more stitch blocks than " ^
                                         "colors in header!")
          in
              GA.append blocks { stitches = stitches, colorindex = colorindex }
          end

      val stitches = GA.empty ()
      (* nb, current needle position (prevx, prevy) is maintained
         across blocks! *)
      fun readblock { prevx, prevy, colornum } =
          let

            fun word7_tointx w =
                let val w = Word8.toInt w
                in if w > 63 
                   then w - 128
                   else w
                end

            fun word13_tointx (hi, lo) =
                let val w =
                    Word32.fromInt (Word8.toInt 
                                    (Word8.andb (hi, 0w15))) * 0w256 +
                    Word32.fromInt (Word8.toInt lo)
                in
                    if Word32.andb (w, 0w2048) = 0w2048
                    then Word32.toInt w - 4096
                    else Word32.toInt w
                end

          in
            case (byte r, byte r) of
                (* end of stitches *)
                (0w255, 0w0) => 
                let
                in
                    addblock (GA.vector stitches, colornum);
                    GA.clear stitches
                    (* done. *)
                end

              | (0w254, 0w176) => (* color switch, start a new block *)
                let
                in
                    addblock (GA.vector stitches, colornum);
                    GA.clear stitches;
                    (* "useless byte" *)
                    ignore (byte r);
                    readblock { prevx = prevx, prevy = prevy,
                                colornum = colornum + 1 }
                end

              | (val1, val2) => (* stitch *)
                let

                    fun getdeltay value =
                        if (Word8.andb (value, 0w128) = 0w128)
                        then (* jump stitch *)
                            word13_tointx (value, byte r)
                        else (* normal *)
                            word7_tointx value

                    val (deltax, deltay) =
                        if (Word8.andb (val1, 0w128) = 0w128)
                        then (* jump stitch. need another byte *)
                            (word13_tointx (val1, val2), 
                             getdeltay (byte r))
                        else (* normal *)
                            (word7_tointx val1, getdeltay val2)

                    val prevx = prevx + deltax
                    val prevy = prevy + deltay
                in
                    GA.append stitches (prevx, prevy);
                    readblock { prevx = prevx, prevy = prevy,
                                colornum = colornum }
                end
          end

      val _ = 
          readblock { prevx = 0, prevy = 0, colornum = 0 }
          handle Bounds => 
              raise PEC ("file ended while reading stitches after " ^
                         Int.toString (GA.length blocks) ^ 
                         " complete blocks and " ^
                         Int.toString (GA.length stitches) ^
                         " stitches in the current one.")
    in
        { blocks = GA.vector blocks }
    end

  fun readpec (r : reader) : pecfile =
    let
      val magic = #vec r 8 
          handle Bounds => raise PEC "not a PEC file (<8 bytes)"
      val _ = StringUtil.matchhead "#PEC" magic
          orelse raise PEC "not a PEC file (bad magic)"
    in
      (* Body of PEC starts right after magic, unsurprisingly. *)
      readpecbody r
    end

end