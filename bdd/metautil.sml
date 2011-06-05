(* Used to generate some of the BDD source code.
   (Functional record update and fetch.)
   You don't need this to use BDD.

   This generates a structure implementing an abstract type with the given fields.
   There are multiple strategies for generating the code.

   TODO: Line wrapping in generated code. *)

(* nb, no field may be named "r" *)

val world = ("W", "World", "world", "WORLD",
             [("flags", "Word32.word"),
              ("body_list", "('b, 'f, 'j) bodycell ref option"),
              ("joint_list", "('b, 'f, 'j) jointcell ref option"),
              ("body_count", "int"),
              ("joint_count", "int"),
              ("gravity", "BDDMath.vec2"),
              ("allow_sleep", "bool"),
              ("ground_body", "('b, 'f, 'j) bodycell ref option"),
              ("goodbye_joint_hook", "('b, 'f, 'j) jointcell ref -> unit"),
              ("goodbye_fixture_hook", "('b, 'f, 'j) fixturecell ref -> unit"),
              ("inv_dt0", "real"),
              ("warm_starting", "bool"),
              ("continuous_physics", "bool"),
              ("broad_phase", "('b, 'f, 'j) fixturecell ref BDDBroadPhase.broadphase"),
              ("contact_list", "('b, 'f, 'j) contactcell ref option"),
              ("contact_count", "int"),
              ("should_collide", "('b, 'f, 'j) fixturecell ref * ('b, 'f, 'j) fixturecell ref -> bool"),
              ("begin_contact", "('b, 'f, 'j) contactcell ref -> unit"),
              ("end_contact", "('b, 'f, 'j) contactcell ref -> unit"),
              ("pre_solve", "('b, 'f, 'j) contactcell ref * BDDTypes.manifold -> unit"),
              ("post_solve", "('b, 'f, 'j) contactcell ref * contact_impulse -> unit")])

(* The single output structure is:

   1. Declarations of the 'cells'.
   datatype worldcell = W of ...
        and fixturecell = F of ...
        and ...

   2. Structure declarations defining the get and set functions.
   
   The single output signature is:
   
   1. abstract type definitions for each of the types (in terms of the cell type)
   2. 
   
*)

fun gendata

fun gensig (shortn, structn, typen, sign, fields) =
    let 
        fun sig_getter (name, typ) =
            "  val get_" ^ name ^ " : ('b, 'f, 'j) " ^ typen ^ " -> (" ^
            typ ^ ")\n"

        fun sig_setter (name, typ) =
            "  val set_" ^ name ^ " : ('b, 'f, 'j) " ^ typen ^ " * (" ^
            typ ^ ") -> unit\n"
    in
        "structure " ^ shortn ^ " : " ^ sign ^ (* HERE *)
        "signature " ^ sign ^ " =\n" ^
        "sig\n" ^
        "  type ('b, 'f, 'j) " ^ typen ^ "\n\n" ^
        String.concat (map sig_getter fields) ^ "\n" ^
        String.concat (map sig_setter fields) ^
        "end\n"
    end



val () = print (gensig world)


(*
val jointedge = ("G",
                 ["other",
                  "joint",
                  "prev",
                  "next"])

val joint = ("J",
             ["flags",
              "typ",
              "prev",
              "next",
              "edge_a",
              "edge_b",
              "body_a",
              "body_b",
              "data",
              "local_center_a",
              "local_center_b",
              "inv_mass_a",
              "inv_i_a",
              "inv_mass_b",
              "inv_i_b"])
*)
(*
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
(*
val () = printl ""

val () = make_get jointedge
val () = make_set jointedge

val () = printl ""

val () = make_get joint
val () = make_set joint

*)

*)