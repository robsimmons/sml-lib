(* Implements PES files, a common Brother/Babylock/Bernina embroidery
   format. Only supports stitch data.

   The file parsing is a fairly straightforward translation of Nathan
   Crawford's C# code into SML. That code is licensed under the GPL,
   so this code is, too. See the file COPYING in the root of sml-lib.
   Nathan's Embroidery Reader software can be found at
    http://www.njcrawford.com/programs/embroidery-reader/
*)

structure PES (* :> PES XXX *) =
struct

  exception PES of string
  open Reader
  structure GA = GrowArray
      
  type stitchblock =
      { colorindex : int,
        stitches : (int * int) vector }

  type pesfile =
      { pesheader : Word64.word vector,
        (* All of these ints are 16 bit
           XXX should these just be charvector? *)
        emboneheader : int vector,
        sewsegheader : int vector,
        embpunchheader : int vector,
        sewfigsegheader : int vector,
        blocks : stitchblock vector }

  (* Port note: C# BinaryReader is all little-endian. *)
  fun readpes (r : reader) =
    let
      val magic = #vec r 8 
          handle Bounds => raise PES "not a PES file (<8 bytes)"
      val _ = StringUtil.matchhead "#PES" magic
          orelse raise PES "not a PES file (bad magic)"

      val pecstart = rl32 r
      val () = #seek r (pecstart + 48)
          handle Bounds => raise PES "PEC color section out of bounds?"

      val numcolors = r8 r + 1
      val colorlist = Vector.tabulate (numcolors, (fn _ => r8 r))

      val () = #seek r (pecstart + 532)
          handle Bounds => raise PES "PEC stitch block out of bounds?"

      val blocks = GA.empty()
      fun addblock (stitches, colornum) =
          let
              val colorindex = 
                  Vector.sub(colorlist, colornum)
                  handle _ => raise PES ("more stitch blocks than " ^
                                         "colors in header!")
          in
              GA.append blocks (stitches, colorindex) (* XXX *)
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
                    print "end blocks\n";
                    addblock (GA.vector stitches, colornum);
                    GA.clear stitches
                    (* done. *)
                end

              | (0w254, 0w176) => (* color switch, start a new block *)
                let
                in
                    print ("stitch block (" ^
                           Int.toString (GA.length stitches) ^
                           " stitches)\n");
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
              raise PES ("file ended while reading stitches after " ^
                         Int.toString (GA.length blocks) ^ 
                         " complete blocks and " ^
                         Int.toString (GA.length stitches) ^
                         " stitches in the current one.")
    in
      raise PES "unimplemented"
    end

end