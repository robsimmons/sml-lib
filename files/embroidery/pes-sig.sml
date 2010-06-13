
signature PES =
sig

  exception PES of string

  (* XXX There is more stuff in PES files, which could be returned
     here. But it is pretty boring stuff, to me at least. *)
  type pesfile = PEC.pecfile

  (* Read a full PES file. Raises PES if an error is encountered. *)
  val readpes : Reader.reader -> pesfile

end