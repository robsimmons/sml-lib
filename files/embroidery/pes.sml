(* Implements PES files, a common Brother/Babylock/Bernina embroidery
   format. Only supports stitch data.

   Implemented via the PEC parser, since PEC data is embedded in PES
   files.
*)

structure PES :> PES =
struct

  exception PES of string
  open Reader
  structure GA = GrowArray
      
  open PEC

  (* From the PEC section. Basically everything else is ignored. 

     XXX This is not enough to write a valid PES file, so in order to
     implement writing I'd need to add the other headers in the file
     (assuming they are not optional). But write support for PES is
     not planned. *)
  type pesfile = pecfile

  (* Port note: C# BinaryReader is all little-endian. *)
  fun readpes (r : reader) : pesfile =
    let
      val magic = #vec r 8 
          handle Bounds => raise PES "not a PES file (<8 bytes)"
      val _ = StringUtil.matchhead "#PES" magic
          orelse raise PES "not a PES file (bad magic)"

      val pecstart = rl32 r
      val () = #seek r pecstart
    in
      PEC.readpecbody r
      handle PEC.PEC s => raise PES ("(via PEC) " ^ s)
    end
end