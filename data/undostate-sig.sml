(* Keeps an efficient history of states for implementing "undo"
   in editing applications. States can be the entire "drawing"
   or some kind of delta; this is up to the client. Supports
   pushing a new state on the end, removing the oldest states
   to conserve memory, and 'undo' and 'redo' actions. 

   PERF note: These can mostly be implemented in constant time, but
   the current implementation is naive. *)
signature UNDOSTATE =
sig

    type 'a undostate

    (* Creates an empty undo state. *)
    val undostate : unit -> 'a undostate

    (* Make sure the history contains no more than n elements,
       removing the oldest ones. This will never truncate the
       future (nor take it into account), so is best called
       after 'save', which is the only thing that increases
       the size. *)
    val truncate : 'a undostate -> int -> unit

    (* Record the new most-recent state and set the cursor to it.
       This removes any 'future' state so an immediate redo will 
       always fail. *)
    val save : 'a undostate -> 'a -> unit

    (* Backs up to the previous state. Usually this is called after
       'save' (or another 'undo') so that redo is possible. May
       return NONE if there are no previous states (for example
       because of truncation). Preserves the length of the undo
       state, just moving the cursor. *)
    val undo : 'a undostate -> 'a option

    (* Replay to the next state, if any. Always works immediately after 
       an 'undo'; never works immediately after a 'save'. Preserves
       the length of the undo state, just moving the cursor. *)
    val redo : 'a undostate -> 'a option

    (* Total number of states. *)
    val length : 'a undostate -> int
    (* Number of states before the cursor, after *)
    val history_length : 'a undostate -> int
    val future_length : 'a undostate -> int

    (* Convert the whole state a list, ignoring the cursor. The head is
       the oldest state. *)
    val tolist : 'a undostate -> 'a list

end
