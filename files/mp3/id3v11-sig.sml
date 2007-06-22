(* manipulate MP3 ID3v1.1 tags (see id3.org) *)

signature ID3V11 =
sig

    type id3v11 =
        { (* truncated to 30 chars *)
          title : string,
          (* truncated to 30 chars *)
          artist : string,
          (* truncated to 30 chars *)
          album : string,
          (* truncated to 4 chars -- Y10Kbug *)
          year : string,
          (* truncated to 28 chars *)
          comment : string,
          (* 0-255 *)
          track : int,
          (* 0-80 *)
          genre : int }

    (* load the id3v1.1 tag from a file, if it exists *)
    val readid3 : string -> id3v11 option

    (* add an id3v1.1 tag to a file, or overwrite the
       existing one *)
    val writecreateid3 : string -> id3v11 -> unit

end