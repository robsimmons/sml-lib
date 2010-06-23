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

  (* RGB. See sml-lib/misc/color-sig.sml for some utilities. *)
  type color = Word8.word * Word8.word * Word8.word
  (* 65 standard colors are defined, probably because there
     is a limited variety of colored thread available. I'm just
     guessing that zero is white, since it doesn't appear in the
     list otherwise and PesFile returns white for any unknown
     index. *)
  val colors = Vector.fromList
      [ (0w255, 0w255, 0w255),
        (0w14, 0w31, 0w124),
        (0w10, 0w85, 0w163),
        (0w48, 0w135, 0w119),
        (0w75, 0w107, 0w175),
        (0w237, 0w23, 0w31),
        (0w209, 0w92, 0w0),
        (0w145, 0w54, 0w151),
        (0w228, 0w154, 0w203),
        (0w145, 0w95, 0w172),
        (0w157, 0w214, 0w125),
        (0w232, 0w169, 0w0),
        (0w254, 0w186, 0w53),
        (0w255, 0w255, 0w0),
        (0w112, 0w188, 0w31),
        (0w186, 0w152, 0w0),
        (0w168, 0w168, 0w168),
        (0w123, 0w111, 0w0),
        (0w255, 0w255, 0w179),
        (0w79, 0w85, 0w86),
        (0w0, 0w0, 0w0),
        (0w11, 0w61, 0w145),
        (0w119, 0w1, 0w118),
        (0w41, 0w49, 0w51),
        (0w42, 0w19, 0w1),
        (0w246, 0w74, 0w138),
        (0w178, 0w118, 0w36),
        (0w252, 0w187, 0w196),
        (0w254, 0w55, 0w15),
        (0w240, 0w240, 0w240),
        (0w106, 0w28, 0w138),
        (0w168, 0w221, 0w196),
        (0w37, 0w132, 0w187),
        (0w254, 0w179, 0w67),
        (0w255, 0w240, 0w141),
        (0w208, 0w166, 0w96),
        (0w209, 0w84, 0w0),
        (0w102, 0w186, 0w73),
        (0w19, 0w74, 0w70),
        (0w135, 0w135, 0w135),
        (0w216, 0w202, 0w198),
        (0w67, 0w86, 0w7),
        (0w254, 0w227, 0w197),
        (0w249, 0w147, 0w188),
        (0w0, 0w56, 0w34),
        (0w178, 0w175, 0w212),
        (0w104, 0w106, 0w176),
        (0w239, 0w227, 0w185),
        (0w247, 0w56, 0w102),
        (0w181, 0w76, 0w100),
        (0w19, 0w43, 0w26),
        (0w199, 0w1, 0w85),
        (0w254, 0w158, 0w50),
        (0w168, 0w222, 0w235),
        (0w0, 0w103, 0w26),
        (0w78, 0w41, 0w144),
        (0w47, 0w126, 0w32),
        (0w253, 0w217, 0w222),
        (0w255, 0w217, 0w17),
        (0w9, 0w91, 0w166),
        (0w240, 0w249, 0w112),
        (0w227, 0w243, 0w91),
        (0w255, 0w200, 0w100),
        (0w255, 0w200, 0w150),
        (0w255, 0w200, 0w200) ] : color Vector.vector

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

            fun word12_tointx (hi, lo) =
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
                    (* ignore (byte r); *)
                    (* XXX restore the ignored read. note side effect *)
                    TextIO.output(TextIO.stdErr,
                                  ("Useless: " ^ Word8.toString
                                   (byte r) ^ "\n"));

                    readblock { prevx = prevx, prevy = prevy,
                                colornum = colornum + 1 }
                end

              | (val1, val2) => (* stitch *)
                let

                    fun getdeltay value =
                        if (Word8.andb (value, 0w128) = 0w128)
                        then (* jump stitch *)
                            word12_tointx (value, byte r)
                        else (* normal *)
                            word7_tointx value

                    val (deltax, deltay) =
                        if (Word8.andb (val1, 0w128) = 0w128)
                        then (* jump stitch. need another byte *)
                            (word12_tointx (val1, val2), 
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

  fun writefile filename ({ blocks } : pecfile) =
    let
        val _ = Vector.length blocks > 256 andalso
            raise PEC "Can't write more than 255 stitchblocks."

        val f = Writer.fromfile filename

        (* There are two kinds of stitches: "normal" and "jump". For 
           deltas of -64 to +63, we use 7 signed bits; a normal stitch. 
           For deltas of -2048 to +2047 we set the high bit of the first
           word, then use its lowest 4 bits plus all 8 bits of the
           next byte to represent a signed 12-bit number. For longer
           stitches (i.e. over two meters!) we fail. *)
        fun encode n =
            if n <= 63 andalso n >= ~64
            then 
                (if n < 0
                 then #byte f (Word8.fromInt (n + 128))
                 else #byte f (Word8.fromInt n))
            else
            if n <= 2047 andalso n >= ~2048
            then
                (let val twoc = 
                     if n < 0
                     then n + 2048
                     else n
                 in
                     #byte f (Word8.orb 
                              (0w128, Word8.fromInt (twoc div 256)));
                     #byte f (Word8.fromInt (twoc mod 256))
                 end)
            else raise PEC ("enencodable delta of " ^ Int.toString n)

        (* After the header is taken care of, we write a
           series of blocks. The points are given in relative
           coordinates using a dense encoding. The needle's
           position is retained across blocks. *)
        fun nextblock (idx, prevx, prevy) =
          let
              val { colorindex = _, stitches } = Vector.sub(blocks, idx)
              fun stitch (prevx, prevy, sidx) =
                  if sidx = Vector.length stitches
                  then (prevx, prevy)
                  else
                    let 
                        val (thisx, thisy) = Vector.sub(stitches, sidx)
                        val (deltax, deltay) = (thisx - prevx, thisy - prevy)
                    in
                        encode deltax;
                        encode deltay;
                        stitch (thisx, thisy, sidx + 1)
                    end

              val (prevx, prevy) = stitch (prevx, prevy, 0)
          in
              if idx = Vector.length blocks - 1
              then
                  let in
                      (* All done. *)
                      #byte f 0w255;
                      #byte f 0w0
                  end
              else
                  let in
                      #byte f 0w254;
                      #byte f 0w176;
                      (* "useless byte" which is mysterious *)
                      #byte f 0w0;
                      nextblock (idx + 1, prevx, prevy)
                  end
          end
    in
        Writer.wstring f "#PEC0001";

        (* 20 char design name *)
        Writer.wstring f "LA:pec.sml          ";
        #byte f 0wx0d;
        (* 12 spaces? *)
        Writer.nchars f #" " 12;
        (* seems to always be the same *)
        #byte f 0wxff;
        #byte f 0wx00;
        #byte f 0wx06;
        #byte f 0wx26;
        (* 12 spaces? *)
        Writer.nchars f #" " 12;
        (* Number of colors (-1), as a byte *)
        #byte f (Word8.fromInt (Vector.length blocks - 1));
        (* Then one byte (color index) per stitchblock. *)
        Vector.app (fn { colorindex, ... } =>
                    #byte f (Word8.fromInt colorindex)) blocks;

        (* Padding with spaces *)
        Writer.nchars f #" " (462 - Vector.length blocks);
        (* Some junk that I don't know what it is *)
        List.app (#byte f)
        [0wx00, 0wx00, 0wx77, 0wx27, 0wx00, 0wx31, 0wxff, 0wxf0,
         0wx00, 0wx03, 0wx04, 0wx03, 0wxe0, 0wx01, 0wxb0, 0wx01,
         0wx91, 0wxc1, 0wx90, 0wx00];

        (* also seen: 00 00 ae 21 00 31 ff f0
                      e0 02 64 03 e0 01 b0 01
                      90 8f 93 58.

           I hope this stuff isn't important? :) *)

        (* do all of the blocks, starting from 0,0 *)
        nextblock (0, 0, 0);

        #close f ()
    end

end