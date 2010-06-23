
signature PEC =
sig

  exception PEC of string

  type color = Word8.word * Word8.word * Word8.word
  (* Standard PEC/PES colors. If the index is out of
     bounds, you should assume white. *)
  val colors : color Vector.vector

  type stitchblock =
      { (* index into the standard colors *)
        colorindex : int,
        (* stitches, in absolute coordinates, measured in mm *)
        stitches : (int * int) vector }

  type pecfile = 
      (* Could parse the name too, which comes in a fixed-length field right
         after the magic. But I can't figure out what the deal is with
         that part of the format: If the name is long, it appears that
         it actually jumps over the color block, and that some
         characters can be lost. Or maybe this is just a bug in the
         writer? *)
      { blocks : stitchblock vector }

  (* Read a full PEC file. Raises PEC if an error is encountered. *)
  val readpec : Reader.reader -> pecfile

  (* The PEC data format is shared between the PEC and PES file formats.
     Call this with the reader positioned at the start of the data
     (for PEC, right after the magic; for PES, at the position indicated
     by the pecstart field) to read that data. *)
  val readpecbody : Reader.reader -> pecfile

  (* Write the PEC file to the named file. Since not all fields
     (e.g. the design name) are present in the pecfile type, dummy
     values are cooked up for these. Raises PEC on any error. *)
  val writefile : string -> pecfile -> unit

end
