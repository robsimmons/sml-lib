structure UndoState : UNDOSTATE =
struct
    (* Not exposed, since these are all bugs *)
    exception UndoState of string

    (* Currenty implemented as two lists,
       where both history and future are stored
       so that the element at the head is
       closest to the cursor in time. 

       PERF: This makes truncation linear time.
       We just need to use a deque for that. *)
    datatype 'a undostate =
        U of { history_size : int ref,
               future_size : int ref,
               history : 'a list ref,
               future : 'a list ref }

    fun undostate () : 'a undostate =
        U { history_size = ref 0,
            future_size = ref 0,
            history = ref nil,
            future = ref nil }

    fun truncate (U { history_size, future_size,
                      history, future }) n =
        if n < !history_size
        then (history_size := n;
              history := List.take (!history, n))
        else ()

    fun save (U { history_size, future_size,
                  history, future }) a =
        let in
            (* Future is invalid now. *)
            future_size := 0;
            future := nil;
            (* Nearest history stored at the head. *)
            history_size := !history_size + 1;
            history := a :: !history
        end

    fun undo (U { history_size = ref 0, 
                  history = ref nil, ... }) = NONE
      | undo (U { history_size, future_size,
                  history = history as (ref (a :: h)), future }) =
        let in
            future_size := !future_size + 1;
            future := a :: !future;

            history_size := !history_size - 1;
            history := h;
            SOME a
        end
      | undo _ = raise UndoState "impossible"

    fun redo (U { future_size = ref 0, 
                  future = ref nil, ... }) = NONE
      | redo (U { history_size, future_size,
                  history, future = future as (ref (a :: f)) }) =
        let in
            history_size := !history_size + 1;
            history := a :: !history;

            future_size := !future_size - 1;
            future := f;
            SOME a
        end
      | redo _ = raise UndoState "impossible"

    fun peek (U { history_size = ref 0, 
                  history = ref nil, ... }) = NONE
      | peek (U { history = ref (a :: _), ... }) = SOME a
      | peek _ = raise UndoState "impossible"

    fun clear (U { history_size, future_size,
                   history, future }) = 
        let in
            history_size := 0;
            future_size := 0;
            history := nil;
            future := nil
        end

    fun length (U { history_size, future_size, ... }) =
        !history_size + !future_size

    fun history_length (U { history_size, ... }) = !history_size
    fun future_length (U { future_size, ... }) = !future_size

    fun tolist (U { history, future, ... }) =
        rev (!history) @ !future

end