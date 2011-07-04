(* Copyright 2010 Tom Murphy VII and Erin Catto. See COPYING for details. *)

(* The implementation of the dynamics engine has several complex,
   mutually-referential data structures. This defines the raw storage
   for these (body, fixture, world, contact, and joint) in a transparent
   way, as well as accessors for them. These are then used to implement
   the algorithms and abstract types in BDDWorld. This way we don't need
   the entire implementation in one file. Clients should not bother with
   this file. You can't even use it to get at the internals of dynamics
   types, because in the client interface those types are abstract.

   Corresponding to parts of dynamics/contacts/b2contact.h, dynamics/b2body.h,
   dynamics/b2fixture.h, dynamics/joints/b2joint.h, etc. *)
structure BDDDynamics =
struct
  open BDDSettings
  open BDDTypes
  open BDDMath
  open BDDOps
  infix 6 :+: :-: %-% %+% +++
  infix 7 *: *% +*: +*+ #*% @*:

  exception BDDDynamics of string

  (* 16 category bits, then 16 mask bits; group index *)
  type filter = Word32.word * int
  val default_filter : BDDDynamicsTypes.filter = (0wx0001FFFF, 1)

  fun !! _ (SOME r) = r
    | !! s NONE = raise BDDDynamics
      ("Expected non-NONE reference; corresponds to an unchecked NULL " ^
       "dereference in Box2D. This is probably because an element " ^
       "(e.g. fixture, joint) was used after being detached, " ^
       "or before being initialized: " ^ s)

  type ('b, 'f, 'j) body = ('b, 'f, 'j) BDDCells.body
  type ('b, 'f, 'j) fixture = ('b, 'f, 'j) BDDCells.fixture
  type ('b, 'f, 'j) contact = ('b, 'f, 'j) BDDCells.contact
  type ('b, 'f, 'j) contactedge = ('b, 'f, 'j) BDDCells.contactedge
  type ('b, 'f, 'j) joint = ('b, 'f, 'j) BDDCells.joint
  type ('b, 'f, 'j) jointedge = ('b, 'f, 'j) BDDCells.jointedge
  type ('b, 'f, 'j) world = ('b, 'f, 'j) BDDCells.world

  (* Internal, fixtures *)
  structure F =
  struct

    open BDDCells.F
    (* XXX in set_density, Box2D has check on range. *)

    val get_body_opt = BDDCells.F.get_body
    fun get_body f = !! "fbody" (get_body_opt f)

    (* Used by body *)
    fun get_mass_data f =
      BDDShape.compute_mass (get_shape f, get_density f)

    (* Port note: Corresponding to b2Fixture::Create and new *)
    fun new (body, { data, friction, restitution, filter, is_sensor,
                     shape, density }) =
        BDDCells.F.new { body = SOME body,
                         data = data,
                         friction = friction,
                         restitution = restitution,
                         filter = filter,
                         sensor = is_sensor,
                         shape = BDDShape.clone shape,
                         density = density,
                         next = NONE,
                         proxy = NONE,
                         (* PERF uninitialized in box2d *)
                         aabb = { lowerbound = vec2 (0.0, 0.0),
                                  upperbound = vec2 (0.0, 0.0) }
                         }

    (* Create a proxy for the fixture in the broad phase, and store it in
       fixture. *)
    fun create_proxy 
        (fixture : ('b, 'f, 'j) fixture, 
         broadphase : ('b, 'f, 'j) fixture BDDBroadPhase.broadphase, 
         xf : BDDMath.transform) : unit =
        case get_proxy fixture of
            NONE =>
                (* Create proxy in the broad-phase. *)
                let 
                    val aabb = BDDShape.compute_aabb (get_shape fixture, xf)

                    fun pxy v = 
                        Real.fmt (StringCvt.FIX (SOME 2)) (vec2x v) ^ " " ^
                        Real.fmt (StringCvt.FIX (SOME 2)) (vec2y v)
                    val () = dprint (fn () => "  fix_aabb: " ^
                                    pxy (#lowerbound aabb) ^ " to " ^
                                    pxy (#upperbound aabb) ^ "\n")
                in
                    set_aabb (fixture, aabb);
                    set_proxy (fixture,
                               SOME (BDDBroadPhase.create_proxy (broadphase, 
                                                                 aabb, 
                                                                 fixture)))
                end
          | SOME _ =>
                raise BDDDynamics "fixture already had a proxy in create_proxy"

    fun destroy_proxy 
        (fixture : ('b, 'f, 'j) fixture, 
         broadphase : ('b, 'f, 'j) fixture BDDBroadPhase.broadphase) =
        (* Proxy can be missing if the body is not active. *)
        case get_proxy fixture of
            NONE => ()
          | SOME p => 
                let in
                    BDDBroadPhase.remove_proxy (broadphase, p);
                    set_proxy (fixture, NONE)
                end

    fun synchronize (fixture : ('b, 'f, 'j) fixture,
                     broadphase : ('b, 'f, 'j) fixture BDDBroadPhase.broadphase,
                     transform1 : transform,
                     transform2 : transform) =
        case get_proxy fixture of
            NONE => ()
          | SOME proxy =>
          (* Compute an AABB that covers the swept shape (may miss some
             rotation effect). *)
          let val shape = get_shape fixture
              val aabb1 = BDDShape.compute_aabb (shape, transform1)
              val aabb2 = BDDShape.compute_aabb (shape, transform2)
              val aabb = BDDCollision.aabb_combine (aabb1, aabb2)

              val displacement : vec2 = 
                  transformposition transform2 :-:
                  transformposition transform1
          in
              set_aabb (fixture, aabb);
              BDDBroadPhase.move_proxy (broadphase, proxy, aabb, displacement)
          end

  end

  (* Internal, joint edges *)
  structure G =
  struct
    open BDDCells.G
  end

  (* Internal, joints *)
  structure J =
  struct
    val FLAG_ISLAND = 0wx1 : Word8.word
    val FLAG_COLLIDE_CONNECTED = 0wx2 : Word8.word

    open BDDCells.J

    fun get_flag (j, f) = Word8.andb (f, get_flags j) <> 0w0
    fun set_flag (j, f) = set_flags (j, Word8.orb(get_flags j, f))
    fun clear_flag (j, f) = set_flags (j, Word8.andb(get_flags j, Word8.notb f))

    fun get_collide_connected j = get_flag(j, FLAG_COLLIDE_CONNECTED)

    (* Used in island solver *)
    fun init_velocity_constraints (j : ('b, 'f, 'j) joint,
                                   step : BDDDynamicsTypes.time_step) : unit =
        raise BDDDynamics "unimplemented"

    (* Used in island solver *)
    fun solve_velocity_constraints (j : ('b, 'f, 'j) joint,
                                    step : BDDDynamicsTypes.time_step) : unit =
        raise BDDDynamics "unimplemented"

    (* Used in island solver *)
    fun solve_position_constraints (j : ('b, 'f, 'j) joint,
                                    baumgarte : real) : bool =
        raise BDDDynamics "unimplemented"
  end

  (* Internal, bodies *)
  structure B =
  struct
    (* Port note: Using Word8, not Word16, since it is more portable
       and probably faster. *)
    val FLAG_ISLAND = 0wx1 : Word8.word
    val FLAG_AWAKE  = 0wx2 : Word8.word
    val FLAG_AUTO_SLEEP = 0wx4 : Word8.word
    val FLAG_BULLET = 0wx8 : Word8.word
    val FLAG_FIXED_ROTATION = 0wx10 : Word8.word
    val FLAG_ACTIVE = 0wx20 : Word8.word
    val FLAG_TOI = 0wx40 : Word8.word

    datatype body_type = datatype BDDDynamicsTypes.body_type
    open BDDCells.B

    fun get_flag (b, f) = Word8.andb (f, get_flags b) <> 0w0
    fun set_flag (b, f) = set_flags (b, Word8.orb(get_flags b, f))
    fun clear_flag (b, f) = set_flags (b, Word8.andb(get_flags b, Word8.notb f))

    fun new ({ typ : body_type,
               position : BDDMath.vec2,
               angle : real,
               linear_velocity : BDDMath.vec2,
               angular_velocity : real,
               linear_damping : real,
               angular_damping : real,
               allow_sleep : bool,
               awake : bool,
               fixed_rotation : bool,
               bullet : bool,
               active : bool,
               data : 'b,
               inertia_scale : real }, 
             world : ('b, 'f, 'j) world,
             next : ('b, 'f, 'j) body option) : ('b, 'f, 'j) body =
        let 
            val xf = transform_pos_angle (position, angle)
            val center = xf @*: vec2 (0.0, 0.0)
            val (mass, inv_mass) =
                case typ of
                    Dynamic => (1.0, 1.0)
                  | _ => (0.0, 0.0)
            val b = 
                BDDCells.B.new
                { typ = typ,
                  flags = 0w0,
                  island_index = 0, (* ? *)
                  xf = xf,
                  sweep = sweep { local_center = vec2 (0.0, 0.0),
                                  a0 = angle, a = angle,
                                  c0 = center, c = center },
                  linear_velocity = linear_velocity,
                  angular_velocity = angular_velocity,
                  force = vec2 (0.0, 0.0),
                  torque = 0.0,
                  world = world,
                  prev = NONE,
                  next = next,
                  fixture_list = NONE,
                  fixture_count = 0,
                  joint_list = NONE,
                  contact_list = NONE,
                  mass = mass,
                  inv_mass = inv_mass,
                  i = 0.0,
                  inv_i = 0.0,
                  linear_damping = linear_damping,
                  angular_damping = angular_damping,
                  sleep_time = 0.0,
                  data = data }

        in
            (* PERF asserts *)
            if vec2is_valid position then () 
            else raise BDDDynamics "invalid position";
            if vec2is_valid linear_velocity then () 
            else raise BDDDynamics "invalid linear_velocity";
            if is_valid angle then () 
            else raise BDDDynamics "invalid angle";
            if is_valid angular_velocity then () 
            else raise BDDDynamics "invalid angular_velocity";
            if is_valid inertia_scale andalso inertia_scale >= 0.0 then ()
            else raise BDDDynamics "invalid inertia_scale";
            if is_valid angular_damping andalso angular_damping >= 0.0 then ()
            else raise BDDDynamics "invalid angular_damping";
            if is_valid linear_damping andalso linear_damping >= 0.0 then ()
            else raise BDDDynamics "invalid linear_damping";

            if bullet then set_flag (b, FLAG_BULLET) else ();
            if fixed_rotation then set_flag (b, FLAG_FIXED_ROTATION) else ();
            if allow_sleep then set_flag (b, FLAG_AUTO_SLEEP) else ();
            if awake then set_flag (b, FLAG_AWAKE) else ();
            if active then set_flag (b, FLAG_ACTIVE) else ();

            b
        end

    (* This is used to prevent connected bodies from colliding.
       It may lie, depending on the collideConnected flag.
       Port note: Used in ContactManager. *)
    fun should_collide (body : ('b, 'f, 'j) body, 
                        other : ('b, 'f, 'j) body) : bool =
        (* At least one body should be dynamic. *)
        if get_typ body <> Dynamic andalso get_typ other <> Dynamic
        then false
        else
            (* Does a joint prevent collision? *)
            let exception ReturnFalse
            in (oapp G.get_next
                  (fn jn =>
                   if eq(G.get_other jn, other)
                   then if J.get_collide_connected (G.get_joint jn) = false
                        then raise ReturnFalse
                        else ()
                   else ())
                  (get_joint_list body);
                true)
                handle ReturnFalse => false
            end

   (* Port note: used in TOISolver. *)
   fun synchronize_transform b : unit =
       let 
           val sweep : sweep = get_sweep b
           val () = dprint (fn () => "s_t angle: " ^ rtos (sweepa sweep) ^ "\n")
(* not error
           val () = if sweepa sweep > (2.0 * BDDSettings.pi + BDDSettings.epsilon)
                    then raise BDDDynamics "ANGLE OVERFLOW."
                    else ()
*)
           val r : mat22 = mat22angle (sweepa sweep)
           val pos : vec2 = sweepc sweep :-: (r +*: sweeplocalcenter sweep)
       in
           set_xf (b, transform (pos, r))
       end

    (* Port note: Used in world. *)
    fun advance (body : ('b, 'f, 'j) body, t : real) : unit =
        (* Advance to the new safe time. *)
        let
            val sweep = get_sweep body
        in
            sweep_advance (sweep, t);
            sweep_set_c (sweep, sweepc0 sweep);
            sweep_set_a (sweep, sweepa0 sweep);
            synchronize_transform body
        end

    (* Used in world. *)
    (* Port note: Passing broadphase instead of whole world value,
       to reduce dependencies. *)
    fun synchronize_fixtures 
        (b : ('b, 'f, 'j) body,
         broadphase : ('b, 'f, 'j) fixture BDDBroadPhase.broadphase) : unit =
      let
          val sweep : sweep = get_sweep b
          val r = mat22angle (sweepa0 sweep)
          val xf1 = transform (sweepc0 sweep :-: 
                               (r +*: sweeplocalcenter sweep),
                               r)
      in
          oapp F.get_next
          (fn f => F.synchronize (f, broadphase, xf1, get_xf b))
          (get_fixture_list b)
      end

    fun get_world_point (b, p) = 
        let in
            dprint (fn () => "[getworldpoint lp " ^ vtos p ^
                   " xf " ^ xftos (get_xf b) ^ 
                   " -> " ^ vtos (get_xf b @*: p) ^ "]\n");

            get_xf b @*: p
        end
    fun get_world_vector (b, v) = transformr (get_xf b) +*: v
    fun get_local_point (b, p) = mul_ttransformv (get_xf b, p)
    fun get_local_vector (b, v) = mul_t22mv (transformr (get_xf b), v)

    fun get_linear_velocity_from_world_point (b, world_point : vec2) =
        get_linear_velocity b :+: 
        cross2sv (get_angular_velocity b,
                  world_point :-: sweepc (get_sweep b))

    fun get_linear_velocity_from_local_point (b, local_point : vec2) =
        get_linear_velocity_from_world_point(b, 
                                             get_world_point(b, local_point))

    fun set_awake (b, f) = 
        let in
            set_sleep_time (b, 0.0);
            if f 
            then set_flag (b, FLAG_AWAKE)
            else 
                let in
                    clear_flag (b, FLAG_AWAKE);
                    set_linear_velocity (b, vec2 (0.0, 0.0));
                    set_force (b, vec2 (0.0, 0.0));
                    set_torque (b, 0.0)
                end
        end

  end

  (* Internal, contact edges *)
  structure E =
  struct
      open BDDCells.E

      fun new () = BDDCells.E.new { contact = NONE, other = NONE,
                                    prev = NONE, next = NONE }
  end

  (* Internal, contacts *)
  structure C =
  struct

    (* Used when crawling contact graph when forming islands. *)
    val FLAG_ISLAND = 0wx1 : Word32.word
    (* Set when the shapes are touching. *)
    val FLAG_TOUCHING  = 0wx2 : Word32.word
    (* This contact can be disabled (by user). *)
    val FLAG_ENABLED = 0wx4 : Word32.word
    (* This contact needs filtering because a fixture filter was changed. *)
    val FLAG_FILTER = 0wx8 : Word32.word
    (* This bullet contact had a TOI event. *)
    val FLAG_BULLET_HIT = 0wx10 : Word32.word

    open BDDCells.C

(*
    fun get_fixture_a (ref (C { fixture_a, ... })) = !! "cfa" fixture_a
    fun get_fixture_b (ref (C { fixture_b, ... })) = !! "cfb" fixture_b
*)

    fun get_flag (b, f) = Word32.andb (f, get_flags b) <> 0w0
    fun set_flag (b, f) = set_flags (b, Word32.orb(get_flags b, f))
    fun clear_flag (b, f) = set_flags (b, Word32.andb(get_flags b, Word32.notb f))

    fun get_fixtures c = (get_fixture_a c, get_fixture_b c)

    fun flag_for_filtering c = set_flag (c, FLAG_FILTER)
    fun get_enabled c = get_flag (c, FLAG_ENABLED)
    fun set_enabled (c, b) = if b
                             then set_flag (c, FLAG_ENABLED)
                             else clear_flag (c, FLAG_ENABLED)

    fun is_touching c = get_flag (c, FLAG_TOUCHING)
    (* Port note: Box2D implements its own binary vtable here; pattern
       matching is much simpler. *)
    fun shapes (fix_a, fix_b) = (F.get_shape fix_a, F.get_shape fix_b)

    fun evaluate (c : ('b, 'f, 'j) contact,
                  xfa : BDDMath.transform, xfb : BDDMath.transform) :
        BDDTypes.manifold =
        case (F.get_shape (get_fixture_a c), F.get_shape (get_fixture_b c)) of
            (BDDShape.Polygon pa, BDDShape.Polygon pb) =>
                BDDCollision.collide_polygons(pa, xfa, pb, xfb)
          | (BDDShape.Circle ca, BDDShape.Circle cb) =>
                BDDCollision.collide_circles(ca, xfa, cb, xfb)
          | (BDDShape.Polygon pa, BDDShape.Circle cb) =>
                BDDCollision.collide_polygon_and_circle (pa, xfa, cb, xfb)
          (* XXX: Not sure this is right. A consequence of the create
             dispatch above is that a Contact in Box2D is always in
             normalized order (polygon, circle), so the "A" fixture actually
             changes meaning. 

             Some other code actually mentions this fact (but does not
             appear to rely on it). We should probably reproduce the
             behavior here though. *)
          | (BDDShape.Circle _, BDDShape.Polygon _) =>
                raise BDDDynamics "impossible! abnormal contact order"
                (* BDDCollision.collide_polygon_and_circle (pb, xfb, ca, xfa) *)

    fun new (fixture_a : ('b, 'f, 'j) fixture, fixture_b) =
        let 
            (* Port note: Normalize so that circle always comes before polygon.
               Not clear that the code requires this (except see above), but
               this makes it easier to compare to Box2D's behavior directly,
               since it swaps as a result of its handmade vtable. *)
            val (fixture_a, fixture_b) = 
                case (F.get_shape fixture_a, F.get_shape fixture_b) of
                    (BDDShape.Circle _, BDDShape.Polygon _) => (fixture_b, fixture_a)
                  | _ => (fixture_a, fixture_b)
        in
          BDDCells.C.new { flags = FLAG_ENABLED,
                           fixture_a = fixture_a,
                           fixture_b = fixture_b,
                           manifold = { point_count = 0,
                                        (* PERF uninitialized in Box2D. *)
                                        typ = E_Circles,
                                        points = Array.fromList nil,
                                        local_normal = vec2 (0.0, 0.0),
                                        local_point = vec2 (0.0, 0.0) },
                           prev = NONE,
                           next = NONE,
                           node_a = E.new (),
                           node_b = E.new (),
                           toi_count = 0 }
        end
  end

  (* Internal, worlds *)
  structure W =
  struct

    val FLAG_NEW_FIXTURE = 0wx1 : Word32.word
    val FLAG_LOCKED = 0wx2 : Word32.word
    val FLAG_CLEAR_FORCES = 0wx4 : Word32.word

    open BDDCells.W

    fun oeq e (NONE, NONE) = true
      | oeq e (SOME a, SOME b) = e (a, b)
      | oeq _ _ = false

    fun get_flag (w, f) = Word32.andb (f, get_flags w) <> 0w0
    fun set_flag (w, f) = set_flags (w, Word32.orb(get_flags w, f))
    fun clear_flag (w, f) = set_flags (w, Word32.andb(get_flags w, Word32.notb f))

    fun is_locked w = get_flag (w, FLAG_LOCKED)
    fun set_auto_clear_forces (w, b) = if b then set_flag (w, FLAG_CLEAR_FORCES)
                                       else clear_flag (w, FLAG_CLEAR_FORCES)
    fun get_auto_clear_forces w = get_flag (w, FLAG_CLEAR_FORCES)

    structure CM =
    struct
      (* Regrettably, used by body.sml when setting a body inactive. 
         XXX figure out the right place for this. *)
         
      fun destroy (world : ('b, 'f, 'j) world, 
                   c : ('b, 'f, 'j) contact) : unit =
        let
          val fixture_a = C.get_fixture_a c
          val fixture_b = C.get_fixture_b c
          val body_a = F.get_body fixture_a
          val body_b = F.get_body fixture_b

          val () = if C.is_touching c
                   then get_end_contact world c
                   else ()

          (* remove from world DLL. *)
          val prev = C.get_prev c
          val next = C.get_next c
          val () = case prev of
              NONE => ()
            | SOME prev => C.set_next (prev, next)
          val () = case next of
              NONE => ()
            | SOME next => C.set_prev (next, prev)

          val () = if oeq C.eq (SOME c, get_contact_list world)
                   then set_contact_list (world, next)
                   else ()

          (* Remove from body A *)
          val nodea = C.get_node_a c
          val prev = E.get_prev nodea
          val next = E.get_next nodea
          val () = case prev of
              NONE => ()
            | SOME prev => E.set_next (prev, next)
          val () = case next of
              NONE => ()
            | SOME next => E.set_prev (next, prev)
          (* Port note: The original code uses pointers to the interior of the
             contact (&c->m_nodeA == ?); I've made these their own cells. I 
             think this is right, but if something is going wrong here, this 
             is a good thing to take a look at. *)
          val () = if oeq E.eq (SOME nodea, B.get_contact_list body_a)
                   then B.set_contact_list (body_a, next)
                   else ()

          (* Remove from body B *)
          val nodeb = C.get_node_b c
          val prev = E.get_prev nodeb
          val next = E.get_next nodeb
          val () = case prev of
              NONE => ()
            | SOME prev => E.set_next (prev, next)
          val () = case next of
              NONE => ()
            | SOME next => E.set_prev (next, prev)
          val () = if oeq E.eq (SOME nodeb, B.get_contact_list body_b)
                   then B.set_contact_list (body_b, next)
                   else ()
        in
            set_contact_count (world, get_contact_count world - 1)
        end

      (* Callback used in find_new_contacts. *)
      exception Return
      fun add_pair (world : ('b, 'f, 'j) world) 
          (fixture_a : ('b, 'f, 'j) fixture, 
           fixture_b : ('b, 'f, 'j) fixture) : unit =
        let
          val body_a = F.get_body fixture_a
          val body_b = F.get_body fixture_b
          (* No self-contact *)
          val () = if B.eq (body_a, body_b) then raise Return
                   else ()

          (* Just raises Return if a contact already exists. *)
          fun one_edge e =
              if oeq B.eq (SOME body_a, E.get_other e)
              then
                  let val fa = C.get_fixture_a (!! "fa" (E.get_contact e))
                      val fb = C.get_fixture_b (!! "fb" (E.get_contact e))
                  in
                      if (F.eq(fa, fixture_a) andalso F.eq(fb, fixture_b)) orelse
                         (F.eq(fa, fixture_b) andalso F.eq(fb, fixture_a))
                      (* Contact already exists. *)
                      then raise Return
                      else ()
                  end
              else ()
          val () = oapp E.get_next one_edge (B.get_contact_list body_b)

          val () = dprint (fn () => "Should collide?\n")

          (* Does a joint override collision? Is at least one body dynamic? *)
          val () = if B.should_collide (body_b, body_a)
                   then ()
                   else raise Return

          val () = dprint (fn () => " ok 1\n")

          (* Check user filtering. *)
          val () = if get_should_collide world (fixture_a, fixture_b)
                   then ()
                   else raise Return

          val () = dprint (fn () => " ok 2\n")

          (* Call the factory. *)
          val c = C.new (fixture_a, fixture_b)

          (* Contact creation may swap fixtures.
             XXX (it doesn't, currently.)
             *)
          val fixture_a = C.get_fixture_a c
          val fixture_b = C.get_fixture_b c
          val body_a = F.get_body fixture_a
          val body_b = F.get_body fixture_b

          (* Add to world DLL. *)
          val () = C.set_next (c, get_contact_list world)
          val () = case get_contact_list world of
              NONE => ()
            | SOME prev => C.set_prev (prev, SOME c)
          val () = set_contact_list (world, SOME c)

          (* Connect to island graph. *)
          val node_a = C.get_node_a c
          val () = E.set_contact (node_a, SOME c)
          val () = E.set_other (node_a, SOME body_b)

          val () = E.set_next (node_a, B.get_contact_list body_a)
          val () = case B.get_contact_list body_a of
              NONE => ()
            | SOME prev => E.set_prev (prev, SOME node_a)
          val () = B.set_contact_list (body_a, SOME node_a)

          val node_b = C.get_node_b c
          val () = E.set_contact (node_b, SOME c)
          val () = E.set_other (node_b, SOME body_a)

          val () = E.set_next (node_b, B.get_contact_list body_b)
          val () = case B.get_contact_list body_b of
              NONE => ()
            | SOME prev => E.set_prev (prev, SOME node_b)
          val () = B.set_contact_list (body_b, SOME node_b)
        in
          set_contact_count (world, get_contact_count world + 1)
        end handle Return => ()

      fun find_new_contacts world =
        BDDBroadPhase.update_pairs (get_broad_phase world, add_pair world)

    end

  end

end

(* XXX New name for this functor or for the struct in dynamics-types.sml.
   Confusing!! *)
functor BDDDynamicsTypes(Arg :
                         sig
                             type body_data
                             type fixture_data
                             type joint_data
                         end) =
struct

  type body = (Arg.body_data, Arg.fixture_data, Arg.joint_data)
      BDDDynamics.body
  type fixture = (Arg.body_data, Arg.fixture_data, Arg.joint_data)
      BDDDynamics.fixture
  type contact = (Arg.body_data, Arg.fixture_data, Arg.joint_data)
      BDDDynamics.contact
  type contactedge = (Arg.body_data, Arg.fixture_data, Arg.joint_data)
      BDDDynamics.contactedge
  type joint = (Arg.body_data, Arg.fixture_data, Arg.joint_data)
      BDDDynamics.joint
  type jointedge = (Arg.body_data, Arg.fixture_data, Arg.joint_data)
      BDDDynamics.jointedge
  type world = (Arg.body_data, Arg.fixture_data, Arg.joint_data)
      BDDDynamics.world
end
