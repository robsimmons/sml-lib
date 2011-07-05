(* Used to generate some of the BDD source code.
   (Functional record update and fetch.)
   You don't need this to use BDD.

   This generates a structure implementing an abstract type with the
   given fields. There are multiple strategies for generating the
   code.

   TODO: Better line wrapping in generated code. *)

exception MetaUtil of string

(* nb, no field may be named "r___" *)
val tmp = "r___"

(* TODO: I put the comments describing fields here, but this is a
   strange place. Make it possible for these to appear in the code
   that's output. *)

(* TODO: Explain what's going on with jointedge and contactedge. *)
fun body cc = ("B", "Body", "body", "BODY",
            [("typ", "BDDDynamicsTypes.body_type"),
             ("flags", "Word8.word"),
             ("island_index", "int"),
             (* Body origin transform *)
             ("xf", "BDDMath.transform"),
             (* Sept motion for CCD *)
             ("sweep", "BDDMath.sweep"),
             ("linear_velocity", "BDDMath.vec2"),
             ("angular_velocity", "real"),
             ("force", "BDDMath.vec2"),
             ("torque", "real"),
             ("world", cc "world"),
             (* XXX option? *)
             ("prev", cc "body" ^ " option"),
             ("next", cc "body" ^ " option"),
             ("fixture_list", cc "fixture" ^ " option"),
             ("fixture_count", "int"),
             ("joint_list", cc "jointedge" ^ " option"),
             (* Port note: a pointer in body.h,
                but members in contact.h. Using refs everywhere. *)
             ("contact_list", cc "contactedge" ^ " option"),
             ("mass", "real"),
             ("inv_mass", "real"),
             (* Rotational inertia about the center of mass. *)
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
                (* Should always be SOME unless deleted. *)
                ("body", cc "body" ^ " option"),
                ("shape", "BDDShape.shape"),
                ("friction", "real"),
                ("restitution", "real"),
                (* Broad phase proxy, where the user data is
                   this fixture. *)
                ("proxy", cc "fixture" ^ " BDDBroadPhase.proxy option"),
                ("filter", "BDDDynamicsTypes.filter"),
                ("sensor", "bool"),
                ("data", "'f")])

fun contact cc = ("C", "Contact", "contact", "CONTACT",
               [("flags", "Word32.word"),
                (* All contacts in the world. *)
                ("prev", cc "contact" ^ " option"),
                ("next", cc "contact" ^ " option"),
                (* nodes for connecting bodies *)
                ("node_a", cc "contactedge"),
                ("node_b", cc "contactedge"),
                (* Port note: made these non-optional. *)
                ("fixture_a", cc "fixture"),
                ("fixture_b", cc "fixture"),
                ("manifold", "BDDTypes.manifold"),
                ("toi_count", "int")])

(* A contact edge is used to connect bodies and contacts together
   in a contact graph where each body is a node and each contact
   is an edge. A contact edge belongs to a doubly linked list
   maintained in each attached body. Each contact has two contact
   nodes, one for each attached body. *)
fun contactedge cc = ("E", "ContactEdge", "contactedge", "CONTACTEDGE",
                   [
                    (* provides quick access to the other body attached. 
                       PERF: Do these really need to be optional? 
                       See World.ContactManager.add_pair. Could pass them
                       to 'new' or do initialization in that function. *)
                    ("other", cc "body" ^ " option"),
                    ("contact", cc "contact" ^ " option"),
                    (* the previous and next contact edge in the 
                       body's contact list *)
                    ("prev", cc "contactedge" ^ " option"),
                    ("next", cc "contactedge" ^ " option")])

fun joint cc = ("J", "Joint", "joint", "JOINT",
             [
              (* Port note: These were individual boolean fields
                 in Box2D. *)
              ("flags", "Word8.word"),
              ("typ", "BDDDynamicsTypes.joint_type"),
              (* the previous and next joints in the world joint list. 
                 the body joint lists are stored in joint edges. *)
              ("prev", cc "joint" ^ " option"),
              ("next", cc "joint" ^ " option"),
              ("edge_a", cc "jointedge" ^ ""),
              ("edge_b", cc "jointedge" ^ ""),
              ("body_a", cc "body" ^ ""),
              ("body_b", cc "body" ^ ""),
              ("data", "'j"),
              (* Cache here per time step to reduce cache misses.
                 PERF: This is probably not a good idea in the SML port. *)
              ("local_center_a", "BDDMath.vec2"),
              ("local_center_b", "BDDMath.vec2"),
              ("inv_mass_a", "real"),
              ("inv_i_a", "real"),
              ("inv_mass_b", "real"),
              ("inv_i_b", "real")])

(* A joint edge is used to connect bodies and joints together
   in a joint graph where each body is a node and each joint
   is an edge. A joint edge belongs to a doubly linked list
   maintained in each attached body. Each joint has two joint
   nodes, one for each attached body. *)
fun jointedge cc = ("G", "JointEdge", "jointedge", "JOINTEDGE",
                 [(* The other body of the joint. *)
                  ("other", cc "body" ^ ""),
                  (* The joint. *)
                  ("joint", cc "joint" ^ ""),
                  (* The previous and next joint edges in the body's
                     joint list. *)
                  ("prev", cc "jointedge" ^ " option"),
                  ("next", cc "jointedge" ^ " option")])
                    
fun world cc = ("W", "World", "world", "WORLD",
             [("flags", "Word32.word"),
              ("body_list", cc "body" ^ " option"),
              ("joint_list", cc "joint" ^ " option"),
              ("body_count", "int"),
              ("joint_count", "int"),
              ("gravity", "BDDMath.vec2"),
              (* Why not a flag? *)
              ("allow_sleep", "bool"),
              ("ground_body", cc "body" ^ " option"),
              ("goodbye_joint_hook", cc "joint" ^ " -> unit"),
              ("goodbye_fixture_hook", cc "fixture" ^ " -> unit"),
              (* Port note: Skipped debug drawing. *)

              (* This is used to compute the time step ratio to
                 support a variable time step. *)
              ("inv_dt0", "real"),
              (* For debugging the solver. Why not flags? *)
              ("warm_starting", "bool"),
              ("continuous_physics", "bool"),
              (* Port Note: Folded the "contact manager" object into the world
                 object. *)
              (* The broad phase uses the userdata to point back to the 
                 fixture cell. *)
              ("broad_phase", cc "fixture" ^ " BDDBroadPhase.broadphase"),
              ("contact_list", cc "contact" ^ " option"),
              ("contact_count", "int"),
              ("should_collide", cc "fixture" ^ " * " ^ cc "fixture" ^ " -> bool"),
              ("begin_contact", cc "contact" ^ " -> unit"),
              ("end_contact", cc "contact" ^ " -> unit"),
              ("pre_solve", cc "contact" ^ " * BDDTypes.manifold -> unit"),
              ("post_solve", cc "contact" ^ " * BDDDynamicsTypes.contact_impulse -> unit")])

val master = ("BDDCells", "BDDCELLS")
val typs = [body, fixture, contact, contactedge, joint, jointedge, world]

datatype mode = FUNCTIONAL | REF
fun celltype typen = typen ^ "cell"
fun external t = "('b, 'f, 'j) " ^ t
(* Not backwards! In the functional representation, the whole record is
   updated. In the ref representation, each field is a ref so the record
   does not have to be. *)
fun internal FUNCTIONAL t = "('b, 'f, 'j) " ^ celltype t ^ " ref"
  | internal REF t = "('b, 'f, 'j) " ^ celltype t

(* The single output structure is:

   1. Declarations of the 'cells'.
   datatype worldcell = W of ...
        and fixturecell = F of ...
        and ...

   2. Structure declarations defining the get and set and new functions.
*)


(* Generates the single internal datatype declaration. *)
fun gendata mode l =
    let
        fun onearm FUNCTIONAL (f, t) = "    " ^ f ^ " : " ^ t
          | onearm REF (f, t)        = "    " ^ f ^ " : (" ^ t ^ ") ref"

        fun onet (shortn, structn, typen, sign, fields) =
            "('b, 'f, 'j) " ^ celltype typen ^ " = " ^
            shortn ^ " of {\n" ^
            StringUtil.delimit ",\n" (map (onearm mode) fields) ^ " }"
    in
        "  datatype " ^ StringUtil.delimit "\n\n  and " (map onet l)
    end

fun genstructtype mode (_, _, typen, _, _) =
    let in
        "  type ('b, 'f, 'j) " ^ typen ^ " = " ^ internal mode typen
    end

fun getter FUNCTIONAL (shortn, structn, typen, sign, fields) (field, typ) =
    let in
        "    fun get_" ^ field ^ " (ref (" ^ shortn ^ " { " ^ 
        field ^ ", ... })) = " ^ field ^ "\n"
    end
  | getter REF (shortn, structn, typen, sign, fields) (field, typ) =
    let in
        "    fun get_" ^ field ^ " (" ^ shortn ^ " { " ^ 
        field ^ ", ... }) = !" ^ field ^ "\n"
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
  | setter REF (shortn, structn, typen, sign, fields) (field, typ) =
    let in
        "    fun set_" ^ field ^ " (" ^ shortn ^ " { " ^ 
        field ^ ", ... }, v) = " ^ field ^ " := v\n"
    end

fun newer FUNCTIONAL (shortn, structn, typen, sign, fields) =
    let
        val record = 
            wraplines 6
            ("ref (" ^ shortn ^ " { " ^
            (StringUtil.delimit ", " (map (fn (f, _) => f ^ " = " ^ f) fields) ^
             " })"))
    in
        wraplines 4
        ("fun new ({ " ^ StringUtil.delimit ", " (map #1 fields) ^
         " }) = ") ^
        record
    end
  | newer REF (shortn, structn, typen, sign, fields) =
    let
        val record = 
            wraplines 6
            (shortn ^ " { " ^
            (StringUtil.delimit ", " (map (fn (f, _) => f ^ " = ref " ^ f) fields) ^
             " }"))
    in
        wraplines 4
        ("fun new ({ " ^ StringUtil.delimit ", " (map #1 fields) ^
         " }) = ") ^
        record
    end

fun eqer FUNCTIONAL _ = "    val eq = op=\n"
  | eqer REF (shortn, structn, typen, sign, fields) =
    (* Instead of making the whole record a ref, or adding a stamp, we
       can just use any one of the fields as the identity of the value. *)
    (case fields of
         (f, _) :: _ =>
             ("    fun eq (" ^ shortn ^ " { " ^ f ^ ", ... }, " ^ 
                               shortn ^ " { " ^ f ^ " = " ^ tmp ^ ", ... }) =\n" ^
              "        " ^ f ^ " = " ^ tmp ^ "\n")
             (* Could just return true, but this is probably a bug if
                this happens? *)
          | nil => raise MetaUtil "Empty records unsupported.")

fun genonestructdecl mode (t as (shortn, structn, typen, sign, fields)) =
    let
    in
        "  structure " ^ shortn ^ " =\n" ^
        "  struct\n" ^
        String.concat (map (getter mode t) fields) ^
        String.concat (map (setter mode t) fields) ^
        newer mode t ^
        eqer mode t ^
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
        (* Not using eqtype because we sometimes need to compare
           when 'b/'f/'j themselves are not eqtypes. *)
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

        val sig_newer =
            "    val new : {\n" ^
            StringUtil.delimit ",\n" (map (fn (f, t) =>
                                                  "      " ^ f ^ " : " ^ t) fields) ^
            " } -> ('b, 'f, 'j) " ^ typen

        val sig_eq =
            "    val eq : ('b, 'f, 'j) " ^ typen ^ " * ('b, 'f, 'j) " ^ typen ^ " -> bool"
    in
        "  structure " ^ shortn ^ " :\n" ^
        "  sig\n" ^
        String.concat (map sig_getter fields) ^ "\n" ^
        String.concat (map sig_setter fields) ^ "\n" ^
        sig_newer ^ "\n" ^
        sig_eq ^ "\n" ^
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
val () = StringUtil.writefile "cells.sml" (genstructdecl REF (master, typs))

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