(* Used to generate some of the BDD source code.
   (Functional record update and fetch.)
   You don't need this to use BDD.

   This generates a structure implementing an abstract type with the given fields.
   There are multiple strategies for generating the code.

   TODO: Line wrapping in generated code. *)

(* nb, no field may be named "r___" *)
val tmp = "r___"

(* XXX these should be called as 'celltype body'? *)
fun body cc = ("B", "Body", "body", "BODY",
            [("typ", "BDDDynamics.body_type"),
             ("flags", "Word8.word"),
             ("island_index", "int"),
             ("xf", "BDDMath.transform"),
             ("sweep", "BDDMath.sweep"),
             ("linear_velocity", "BDDMath.vec2"),
             ("angular_velocity", "real"),
             ("force", "BDDMath.vec2"),
             ("torque", "real"),
             ("world", cc "world"),
             ("prev", cc "body" ^ " option"),
             ("next", cc "body" ^ " option"),
             ("fixture_list", cc "fixture" ^ " option"),
             ("fixture_count", "int"),
             ("joint_list", cc "jointedge" ^ " option"),
             ("contact_list", cc "contactedge" ^ " option"),
             ("mass", "real"),
             ("inv_mass", "real"),
             ("i", "real"),
             ("inv_i", "real"),
             ("linear_damping", "real"),
             ("angular_damping", "real"),
             ("sleep_time", "real"),
             ("data", "'b")])

fun fixture cc = ("F", "Fixture", "fixture", "FIXTURE",
               [("aabb", "BDDTypes.aabb"),
                ("density", "real"),
                ("next", cc "fixture" ^ " option"),
                ("body", cc "body" ^ " option"),
                ("shape", "BDDShape.shape"),
                ("friction", "real"),
                ("restitution", "real"),
                ("proxy", cc "fixture" ^ " BDDBroadPhase.proxy option"),
                ("filter", "BDDDynamics.filter"),
                ("sensor", "bool"),
                ("data", "'f")])

fun contact cc = ("C", "Contact", "contact", "CONTACT",
               [("flags", "Word32.word"),
                ("prev", cc "contact" ^ " option"),
                ("next", cc "contact" ^ " option"),
                ("node_a", cc "contactedge" ^ " ref"),
                ("node_b", cc "contactedge" ^ " ref"),
                ("fixture_a", cc "fixture" ^ " ref option"),
                ("fixture_b", cc "fixture" ^ " ref option"),
                ("manifold", "BDDTypes.manifold"),
                ("toi_count", "int")])

fun contactedge cc = ("E", "ContactEdge", "contactedge", "CONTACTEDGE",
                   [("other", cc "body" ^ " option"),
                    ("contact", cc "contact" ^ " option"),
                    ("prev", cc "contactedge" ^ " ref option"),
                    ("next", cc "contactedge" ^ " ref option")])

fun joint cc = ("J", "Joint", "joint", "JOINT",
             [("flags", "Word8.word"),
              ("typ", "BDDDynamics.joint_type"),
              ("rev", cc "joint" ^ " option"),
              ("next", cc "joint" ^ " option"),
              ("edge_a", cc "jointedge" ^ ""),
              ("edge_b", cc "jointedge" ^ ""),
              ("body_a", cc "body" ^ ""),
              ("body_b", cc "body" ^ ""),
              ("data", "'j"),
              ("local_center_a", "BDDMath.vec2"),
              ("local_center_b", "BDDMath.vec2"),
              ("inv_mass_a", "real"),
              ("inv_i_a", "real"),
              ("inv_mass_b", "real"),
              ("inv_i_b", "real")])

fun jointedge cc = ("G", "JointEdge", "jointedge", "JOINTEDGE",
                 [("other", cc "body" ^ ""),
                  ("joint", cc "joint" ^ ""),
                  ("prev", cc "jointedge" ^ " option"),
                  ("next", cc "jointedge" ^ " option")])
                    
fun world cc = ("W", "World", "world", "WORLD",
             [("flags", "Word32.word"),
              ("body_list", cc "body" ^ " option"),
              ("joint_list", cc "joint" ^ " option"),
              ("body_count", "int"),
              ("joint_count", "int"),
              ("gravity", "BDDMath.vec2"),
              ("allow_sleep", "bool"),
              ("ground_body", cc "body" ^ " option"),
              ("goodbye_joint_hook", cc "joint" ^ " -> unit"),
              ("goodbye_fixture_hook", cc "fixture" ^ " ref -> unit"),
              ("inv_dt0", "real"),
              ("warm_starting", "bool"),
              ("continuous_physics", "bool"),
              ("broad_phase", cc "fixture" ^ " ref BDDBroadPhase.broadphase"),
              ("contact_list", cc "contact" ^ " option"),
              ("contact_count", "int"),
              ("should_collide", cc "fixture" ^ " ref * " ^ cc "fixture" ^ " -> bool"),
              ("begin_contact", cc "contact" ^ " -> unit"),
              ("end_contact", cc "contact" ^ " -> unit"),
              ("pre_solve", cc "contact" ^ " * BDDTypes.manifold -> unit"),
              ("post_solve", cc "contact" ^ " * BDDDynamics.contact_impulse -> unit")])

val master = ("BDDCells", "BDDCELLS")
val typs = [body, fixture, contact, contactedge, joint, jointedge, world]

datatype mode = FUNCTIONAL | REF
fun celltype typen = typen ^ "cell"
fun external t = "('b, 'f, 'j) " ^ t
fun internal FUNCTIONAL t = "('b, 'f, 'j) " ^ celltype t ^ " ref"

(* The single output structure is:

   1. Declarations of the 'cells'.
   datatype worldcell = W of ...
        and fixturecell = F of ...
        and ...

   2. Structure declarations defining the get and set functions.
*)


(* Generates the single internal datatype declaration. *)
fun gendata FUNCTIONAL l =
    let
        fun onearm (f, t) = "    " ^ f ^ " : " ^ t

        fun onet (shortn, structn, typen, sign, fields) =
            "('b, 'f, 'j) " ^ celltype typen ^ " = " ^
            shortn ^ " of {\n" ^
            StringUtil.delimit ",\n" (map onearm fields) ^ " }"
    in
        "  datatype " ^ StringUtil.delimit "\n\n  and " (map onet l)
    end

fun genstructtype FUNCTIONAL (_, _, typen, _, _) =
    let in
        "  type ('b, 'f, 'j) " ^ typen ^ " = ('b, 'f, 'j) " ^ celltype typen ^ " ref\n"
    end

fun getter FUNCTIONAL (shortn, structn, typen, sign, fields) (field, typ) =
    let in
        "    fun get_" ^ field ^ " (ref (" ^ shortn ^ " { " ^ 
        field ^ ", ... })) = " ^ field ^ "\n"
    end

(* Not that good, but better than 300 character lines *)
fun wraplines indent str = 
    let val i = CharVector.tabulate (indent, fn _ => #" ")
    in String.concat (map (fn s => i ^ s ^ "\n") (StringUtil.wrapto (79 - indent) str))
    end

fun setter FUNCTIONAL (shortn, structn, typen, sign, fields) (field, typ) =
    let
        val assign = 
            wraplines 6
            (tmp ^ " := " ^ shortn ^ " { " ^
            (StringUtil.delimit ", " (map (fn (f, _) => f ^ " = " ^ f) fields) ^
             " }"))
    in
        wraplines 4
        ("fun set_" ^ field ^ " (" ^ tmp ^ " as ref (" ^ 
         shortn ^ " { " ^
         StringUtil.delimit ", " ((map (fn (f, _) =>
                                        f ^ (if f = field
                                             then " = _"
                                             else ""))) fields) ^
         " }), " ^ field ^ ") =") ^
        assign
    end

fun genonestructdecl mode (t as (shortn, structn, typen, sign, fields)) =
    let
    in
        "  structure " ^ shortn ^ " =\n" ^
        "  struct\n" ^
        String.concat (map (getter mode t) fields) ^
        String.concat (map (setter mode t) fields) ^
        "  end\n"
    end

fun genstructdecl mode ((mstruct, msig), typs) =
    let
        val typs = map (fn f => f (internal mode)) typs
    in
        "(* Generated file. Do not edit! *)\n" ^
        "structure " ^ mstruct ^ " :> " ^ msig ^ " =\n" ^
        "struct\n" ^
        gendata mode typs ^ "\n\n" ^
        String.concat (map (genstructtype mode) typs) ^ "\n" ^
        String.concat (map (genonestructdecl mode) typs) ^ "\n" ^
        "end\n"
    end

(*
   The single output signature is:
   
   1. abstract type definitions for each of the types (in terms of the cell type)
   2. signatures for the structures containing the get and set functions
*)

(* Generate the type declaration for the signature. *)
fun gensigtype (_, _, typen, _, _) =
    let in
        "  type ('b, 'f, 'j) " ^ typen ^ "\n"
    end

(* Generate the structure : signature decl for an individual type,
   within the master signature. *)
fun genonestructsig (shortn, structn, typen, sign, fields) =
    let 
        fun sig_getter (name, typ) =
            "    val get_" ^ name ^ " : ('b, 'f, 'j) " ^ typen ^ " -> (" ^
            typ ^ ")\n"

        fun sig_setter (name, typ) =
            "    val set_" ^ name ^ " : ('b, 'f, 'j) " ^ typen ^ " * (" ^
            typ ^ ") -> unit\n"
    in
        "  structure " ^ shortn ^ " :\n" ^
        "  sig\n" ^
        String.concat (map sig_getter fields) ^ "\n" ^
        String.concat (map sig_setter fields) ^
        "  end\n\n"
    end

(* Generate the signature for everything *)
fun gensigdecl ((mstruct, msig), typs) =
    let
        val typs = map (fn f => f external) typs
    in
        "(* Generated file. Do not edit! *)\n" ^
        "signature " ^ msig ^ " =\n" ^
        "sig\n" ^
        String.concat (map gensigtype typs) ^ "\n\n" ^
        String.concat (map genonestructsig typs) ^
        "end\n"
    end
    

(* XXX from commandlines? *)
(*
val () = print (gensigdecl (master, typs))

val () = print "\n-------\n"

val () = print (genstructdecl FUNCTIONAL (master, typs))
*)

val () = StringUtil.writefile "cells-sig.sml" (gensigdecl (master, typs))
val () = StringUtil.writefile "cells.sml" (genstructdecl FUNCTIONAL (master, typs))

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