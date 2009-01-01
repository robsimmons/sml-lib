
signature XML =
sig

  exception XML of string

  (* name and value *)
  type attribute = string * string option
  (* tag name, attributes *)
  type tag = string * attribute list

  datatype tree = 
      Text of string
    | Elem of tag * tree list

  (* Parses a file on disk, returning the (normalized) tree or raising
     the exception XML. *)
  val parsefile : string -> tree

  (* Removes empty text nodes. Concatenates sibling text nodes. *)
  val normalize : tree -> tree

end