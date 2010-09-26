(* Used to generate some of the BDD source code.
   (Functional record update and fetch.)
   You don't need this to use BDD.

   TODO: Line wrapping in generated code. *)

(* nb, no field may be named "r" *)
val world = ("W",
             ["flags",
              "body_list",
              "joint_list",
              "body_count",
              "joint_count",
              "gravity",
              "allow_sleep",
              "ground_body",
              "goodbye_joint_hook",
              "goodbye_fixture_hook",
              "inv_dt0",
              "warm_starting",
              "continuous_physics",
              "broad_phase",
              "contact_list",
              "contact_count",
              "should_collide",
              "begin_contact",
              "end_contact",
              "pre_solve",
              "post_solve"])

val jointedge = ("G",
                 ["other",
                  "joint",
                  "prev",
                  "next"])

fun printl s = (print s; print "\n")
fun make_get (ctor, fields) =
    let 
        fun one field =
            "fun get_" ^ field ^ " (ref (" ^ ctor ^ "{ " ^ 
            field ^ ", ... })) = " ^ field
    in
        app (printl o one) fields
    end

fun make_set (ctor, fields) =
    let
        val assign = "r := " ^ ctor ^ " { " ^
            StringUtil.delimit ", " (map (fn f => f ^ " = " ^ f) fields) ^
            "}"
            
        fun one field =
            "fun set_" ^ field ^ " (r as ref (" ^ ctor ^ "{ " ^
            StringUtil.delimit ", " ((map (fn f => 
                                           f ^ (if f = field
                                                then " = _"
                                                else ""))) fields) ^
            " }), " ^ field ^ ") = " ^ assign
            
    in
        app (printl o one) fields
    end


val () = make_get world
val () = make_set world


val () = make_get jointedge
val () = make_set jointedge