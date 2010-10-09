(* Copyright 2010 Tom Murphy VII and Erin Catto. See COPYING for details. *)

(* Corresponding to dynamics/contacts/b2toisolver.cpp. *)
structure BDDTOISolver :> BDDTOI_SOLVER =
struct

  exception BDDTOISolver of string
  open BDDMath BDDTypes BDDSettings BDDOps
  infix 6 :+: :-: %-% %+% +++
  infix 7 *: *% +*: +*+ #*% @*:
  structure D = BDDDynamics

  type ('b, 'f, 'j) constraint =
      { body_a : ('b, 'f, 'j) D.body,
        body_b : ('b, 'f, 'j) D.body,
        local_normal : BDDMath.vec2,
        local_point : BDDMath.vec2,
        local_points : BDDMath.vec2 array,
        point_count : int,
        radius : real,
        typ : BDDTypes.manifold_type }
  type ('b, 'f, 'j) solver =
      { constraints : ('b, 'f, 'j) constraint array,
        toi_body : ('b, 'f, 'j) D.body }

  (* PERF I think there is something faster with slices *)
  fun array_clone a = Array.tabulate (Array.length a,
                                      fn x => Array.sub (a, x))

  (* Port note: contacts arrive (in a list) in the reverse order that
     they would appear in the array in Box2D. *)
  fun solver (contacts : ('b, 'f, 'j) BDDDynamics.contact list,
              body : ('b, 'f, 'j) BDDDynamics.body) : ('b, 'f, 'j) solver =
     let
        (* PERF. Probably not necessary. *)
        val contacts = rev contacts
        fun onecontact contact =
            let
              val fixture_a = D.C.get_fixture_a contact
              val fixture_b = D.C.get_fixture_b contact
              val shape_a = D.F.get_shape fixture_a
              val shape_b = D.F.get_shape fixture_b
              val radius_a : real = BDDShape.get_radius shape_a
              val radius_b : real = BDDShape.get_radius shape_b
              val body_a = D.F.get_body fixture_a
              val body_b = D.F.get_body fixture_b
              val manifold = D.C.get_manifold contact

              (* PERF assertion *)
              val _ = #point_count manifold > 0
                  orelse raise BDDTOISolver "assert failed"
            in
              (* PERF Following Box2D's optimizations, but I doubt
                 there's any point to this copying for the SML version. *)
              { body_a = body_a,
                body_b = body_b,
                local_normal = #local_normal manifold,
                local_point = #local_point manifold,
                typ = #typ manifold,
                point_count = #point_count manifold,
                radius = radius_a + radius_b,
                local_points =
                Array.tabulate (Array.length (#points manifold),
                                fn j =>
                                #local_point 
                                (Array.sub(#points manifold, j))) }
            end
     in
        { constraints = Array.fromList (map onecontact contacts),
          toi_body = body }
     end

  (* Port note: A class in Box2D; it's just a function that
     returns multiple values. *)
  fun toi_solver_manifold (cc : ('b, 'f, 'j) constraint, index : int) :
      { normal : vec2, point : vec2, separation : real } =
      raise BDDTOISolver "unimplemented"

  (* Push out the TOI body to provide clearance for further 
     simulation. *)
  fun solve (solver : ('b, 'f, 'j) solver, baumgarte : real) : bool =
    let
      val min_separation = ref 0.0
      fun oneconstraint (c : ('b, 'f, 'j) constraint) =
        let
            val body_a = #body_a c
            val body_b = #body_b c

            (* Only the TOI body should move. *)
            val (mass_a, mass_b) =
                if body_a = #toi_body solver
                then (D.B.get_mass body_a, 0.0)
                else (0.0, D.B.get_mass body_b)

            val inv_mass_a = mass_a * D.B.get_inv_mass body_a
            val inv_i_a = mass_a * D.B.get_inv_i body_a
            val inv_mass_b = mass_b * D.B.get_inv_mass body_b
            val inv_i_b = mass_b * D.B.get_inv_i body_b

            (* Solve normal constraints. *)
        in
            for 0 (#point_count c - 1)
            (fn j =>
             let
                 val { normal : vec2, point : vec2, separation : real } =
                     toi_solver_manifold (c, j)

                 val r_a : vec2 = point :-: sweepc (D.B.get_sweep body_a)
                 val r_b : vec2 = point :-: sweepc (D.B.get_sweep body_b)

                 (* Track max constraint error. *)
                 val () = if separation < !min_separation
                          then min_separation := separation
                          else ()

                 (* Prevent large corrections and allow slop. *)
                 val capital_c : real = clampr (baumgarte * (separation + linear_slop),
                                                ~max_linear_correction,
                                                0.0)
                 (* Compute the effective mass. *)
                 val rn_a : real = cross2vv (r_a, normal)
                 val rn_b : real = cross2vv (r_b, normal)
                 val k : real = inv_mass_a + inv_mass_b + 
                     inv_i_a * rn_a * rn_a +
                     inv_i_b * rn_b * rn_b

                 (* Compute normal impulse. *)
                 val impulse : real = if k > 0.0 then capital_c / k else 0.0
                 val p : vec2 = impulse *: normal

                 val sweep_a : sweep = D.B.get_sweep body_a
(*
                 val () = D.B.set_sweep
                     (body_a,
                      sweep
                      { local_center = sweeplocalcenter _a,
                        c0 = sweepc0 sweep_a,
                        c = sweepc sweep_a - (inv_mass_a * p),
                        a0 = sweepa0 sweep_a,
                        a = sweepa sweep_a - (inv_i_a * cross2vv (r_a, p)) })
*)
                 (* XXX HERE  8 Oct 2010 *)                     
             (*
                  where? tom
                 val () = D.B.synchronize_transform body_a

                    bodyB->m_sweep.c += invMassB * P;
                    bodyB->m_sweep.a += invIB * b2Cross(rB, P);
                    bodyB->SynchronizeTransform();
              *)
             in
                 raise BDDTOISolver "unimplemented"
             end)
        end
    in
      Array.app oneconstraint (#constraints solver);
      (* We can't expect minSpeparation >= -b2_linearSlop because we don't
         push the separation above -b2_linearSlop. *)
      !min_separation >= ~1.5 * linear_slop
    end
end