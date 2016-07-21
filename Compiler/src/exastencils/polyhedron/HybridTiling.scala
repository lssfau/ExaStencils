package exastencils.polyhedron

import exastencils.core._
import isl._

import scala.collection._

/**
 * The hybrid tiling implemented in this file is based on Grosser et al., "Hybrid Hexagonal/Classical Tiling for GPUs".
 */
object HybridTiling {

  /**
   * Name of the ts-space.
   */
  val ts_space_name = "ts"

  /**
   * Name of the local ts-space.
   */
  val local_ts_space_name = "local_ts"

  /**
   * Name used in mark nodes that contain a pointer to a ppcg_ht_phase.
   */
  val ppcg_phase_name = "phase"

  var mark_map = mutable.HashMap[String, ppcg_ht_phase]()
  var mark_count = 0

  /**
   * Create a ppcg_ht_bounds object for a band living in "space". The bounds are initialized to NaN.
   */
  def ppcg_ht_bounds_alloc(space : isl.Space) = {
    val ctx = space.getCtx
    val upper = isl.Val.nan(ctx)
    var lower = isl.MultiVal.zero(space)
    val n = space.dim(isl.DimType.Set)

    (0 until n).foreach(x => {
      val v = upper
      lower = lower.setVal(x, v)
    })

    ppcg_ht_bounds(upper, lower)
  }

  /**
   *  Replace the upper bound on the relative dependence distances in the first space dimension by "upper".
   */
  def ppcg_ht_bounds_set_upper(bounds : ppcg_ht_bounds, upper : isl.Val) = {
    ppcg_ht_bounds(upper, bounds.lower)
  }

  /**
   *  Return the lower bound on the relative dependence distances in space dimension "pos".
   */
  def ppcg_ht_bounds_get_lower(bounds : ppcg_ht_bounds, pos : Int) = {
    bounds.lower.getVal(pos)
  }

  /**
   *  Replace the lower bound on the relative dependence distances in space dimension "pos" by "lower".
   */
  def ppcg_ht_bounds_set_lower(bounds : ppcg_ht_bounds, pos : Int, lower : isl.Val) = {
    bounds.lower.setVal(pos, lower)
    bounds
  }

  /**
   *  Can the bounds on relative dependence distances recorded in "bounds" be used to perform hybrid tiling?
   * In particular, have appropriate lower and upper bounds been found?
   * Any NaN indicates that no corresponding bound was found.
   */
  def ppcg_ht_bounds_is_valid(bounds : ppcg_ht_bounds) : Boolean = {
    var is_nan = bounds.upper.isNan
    if (is_nan)
      return false

    val n = bounds.lower.dim(isl.DimType.Set)
    (0 until n).foreach(x => {
      val v = bounds.lower.getVal(x)
      is_nan = v.isNan
      if (is_nan)
        return false
    })

    true
  }

  /**
   *  Return the space of the pair of band nodes that form the input to the hybrid tiling.
   * In particular, return the space [P -> C], where P is the space of the parent node and C is the space of the child node.
   */
  def ppcg_ht_tiling_get_input_space(tile : ppcg_ht_tiling) = {
    tile.input_schedule.getSpace
  }

  /**
   *  Return a new reference to "tiling".
   */
  def ppcg_ht_tiling_copy(tiling : ppcg_ht_tiling) = {
    ppcg_ht_tiling(tiling.ref + 1, tiling.bounds, tiling.input_node, tiling.input_schedule, tiling.space_sizes, tiling.time_tile, tiling.local_time, tiling.shift_space, tiling.shift_phase, tiling.hex, tiling.project_ts)
  }

  /**
   * Return the space of the pair of band nodes that form the input to the hybrid tiling of which "phase" is a phase.
   * In particular, return the space [P -> C], where P is the space of the parent node and C is the space of the child node.
   */
  def ppcg_ht_phase_get_input_space(phase : ppcg_ht_phase) = {
    ppcg_ht_tiling_get_input_space(phase.tiling)
  }

  /**
   * Construct the lower left constraint of the hexagonal tile, i.e.,
   *
   * du a - b <= (2h+1) du - duh
   * -du a + b + (2h+1) du - duh >= 0
   *
   * where duh = floor(du * h).
   *
   * This constraint corresponds to (6) in
   * "Hybrid Hexagonal/Classical Tiling for GPUs".
   */
  def hex_lower_left(ls : isl.LocalSpace, h : isl.Val, du : isl.Val, duh : isl.Val) : isl.BasicMap = {
    var v = Duplicate(h).mulUi(2).addUi(1)
    v = v.mul(Duplicate(du))
    v = v.sub(Duplicate(duh))
    var aff : isl.Aff = isl.Aff.valOnDomain(ls, v)
    v = Duplicate(du).neg()
    aff = aff.setCoefficientVal(isl.DimType.In, 0, v)
    aff = aff.setCoefficientSi(isl.DimType.In, 1, 1)

    //TODO: return isl_inequality_from_aff(aff)
    isl.BasicMap.fromAff(aff)
  }

  /* Construct the lower constraint of the hexagonal tile, i.e.,
    *
    *	a <= 2h+1
      *	-a + 2h+1 >= 0
      *
      * This constraint corresponds to (7) in
      * "Hybrid Hexagonal/Classical Tiling for GPUs".
      */
  def hex_lower(ls : isl.LocalSpace, h : isl.Val) : isl.BasicMap = {
    val v = Duplicate(h).mulUi(2).add(isl.Val.fromInt(h.getCtx, 1))
    var aff = isl.Aff.valOnDomain(ls, v)
    aff = aff.setCoefficientSi(isl.DimType.In, 0, -1)

    //TODO: return isl_inequality_from_aff(aff);
    isl.BasicMap.fromAff(aff)
  }

  /* Construct the lower right constraint of the hexagonal tile, i.e.,
      *
      *	dl a + b <= (2h+1) dl + duh + (s0-1)
        *	-dl a - b + (2h+1) dl + duh + (s0-1) >= 0
        *
        * where duh = floor(du * h).
        *
        * This constraint corresponds to (8) in
        * "Hybrid Hexagonal/Classical Tiling for GPUs".
        */
  def hex_lower_right(ls : isl.LocalSpace, h : isl.Val, s0 : isl.Val, dl : isl.Val, duh : isl.Val) : isl.BasicMap = {
    var v = Duplicate(h).mulUi(2).add(isl.Val.fromInt(h.getCtx, 1))
    v = v.mul(Duplicate(dl))
    v = v.add(Duplicate(duh))
    v = v.add(Duplicate(s0))
    v = v.sub(isl.Val.fromInt(v.getCtx, 1))
    var aff = isl.Aff.valOnDomain(ls, v)
    v = Duplicate(dl).neg()
    aff = aff.setCoefficientVal(isl.DimType.In, 0, v)
    aff = aff.setCoefficientSi(isl.DimType.In, 1, -1)

    //TODO:return isl_inequality_from_aff(aff);
    isl.BasicMap.fromAff(aff)
  }

  /* Construct the upper left constraint of the hexagonal tile, i.e.,
        *
        *	dl a + b >= h dl - (d - 1)/d				with d = den(dl)
        *	dl a + b - h dl + (d - 1)/d >= 0
        *
        * This constraint corresponds to (10) in
        * "Hybrid Hexagonal/Classical Tiling for GPUs".
        */
  def hex_upper_left(ls : isl.LocalSpace, h : isl.Val, dl : isl.Val) : isl.BasicMap = {
    val d = isl.Val.fromBigInteger(dl.getCtx, dl.getDen)
    var v = Duplicate(d).sub(isl.Val.fromInt(d.getCtx, 1))
    v = v.div(d)
    v = v.sub(Duplicate(h).mul(Duplicate(dl)))
    var aff = isl.Aff.valOnDomain(ls, v)
    aff = aff.setCoefficientVal(isl.DimType.In, 0, Duplicate(dl))
    aff = aff.setCoefficientSi(isl.DimType.In, 1, 1)

    //TODO:return isl_inequality_from_aff(aff);
    isl.BasicMap.fromAff(aff)
  }

  /* Construct the upper right constraint of the hexagonal tile, i.e.,
  *
  *	du a - b >= du h - duh - (s0-1) - dlh - (d - 1)/d	with d = den(du)
  *	du a - b - du h + duh + (s0-1) + dlh + (d - 1)/d >= 0
  *
  * where dlh = floor(dl * h) and duh = floor(du * h).
  *
  * This constraint corresponds to (12) in
  * "Hybrid Hexagonal/Classical Tiling for GPUs".
  */
  def hex_upper_right(ls : isl.LocalSpace, h : isl.Val, s0 : isl.Val, du : isl.Val, dlh : isl.Val, duh : isl.Val) : isl.BasicMap = {
    val d = isl.Val.fromBigInteger(du.getCtx, du.getDen)
    var v = Duplicate(d).sub(isl.Val.fromInt(d.getCtx, 1))
    v = v.div(d)
    v = v.sub(Duplicate(h).mul(Duplicate(du)))
    v = v.add(Duplicate(duh))
    v = v.add(Duplicate(dlh))
    v = v.add(Duplicate(s0))
    v = v.sub(isl.Val.fromInt(v.getCtx, 1))
    var aff = isl.Aff.valOnDomain(ls, v)
    aff = aff.setCoefficientVal(isl.DimType.In, 0, Duplicate(du))
    aff = aff.setCoefficientSi(isl.DimType.In, 1, -1)

    //TODO: return isl_inequality_from_aff(aff);
    isl.BasicMap.fromAff(aff)
  }

  /* Construct the uppper constraint of the hexagonal tile, i.e.,
  *
  * a >= 0
  *
  * This constraint corresponds to (13) in
  * "Hybrid Hexagonal/Classical Tiling for GPUs".
  */
  def hex_upper(ls : isl.LocalSpace) : isl.BasicMap = {
    val aff = isl.Aff.varOnDomain(ls, isl.DimType.Set, 0)

    //TODO:return isl_inequality_from_aff(aff);
    isl.BasicMap.fromAff(aff)
  }

  /**
   * Construct the basic hexagonal tile shape.
   * "space" is the 2D space in which the hexagon should be constructed.
   * h is st-1, with st the tile size in the time dimension
   * s0 is the tile size in the space dimension
   * dl is a bound on the negative relative dependence distances, i.e.,
   *
   * d_s >= -dl d_t
   *
   * du is a bound on the positive relative dependence distances, i.e.,
   *
   * d_s <= du d_t
   *
   * with (d_t,d_s) any dependence distance vector.
   * dlh = floor(dl * h)
   * duh = floor(du * h)
   *
   * The shape of the hexagon is as follows:
   *
   * 0 dlh   dlh+s0-1
   *      ______                __
   * 0     /      \_             /
   *    /         \_          /
   * h   /            \ ______ /
   * h+1   \_           //      \\_
   *     \_        //         \\_
   * 2h+1        \______//            \\
   *   0   duh   duh+s0-1
   *                duh+s0-1+dlh
   *                     duh+s0-1+dlh+1+s0+1
   *
   * The next hexagon is shifted by duh + dlh + 2 * s0.
   *
   * The slope of the "/" constraints is dl.
   * The slope of the "\_" constraints is du.
   */
  def compute_hexagon(space : isl.Space, h : isl.Val, s0 : isl.Val, dl : isl.Val, du : isl.Val, dlh : isl.Val, duh : isl.Val) = {
    val ls : isl.LocalSpace = isl.LocalSpace.fromSpace(space)

    //    var c : isl.Constraint = hex_lower_left(Duplicate(ls), h, du, duh)
    //    var bset = isl.BasicSet.fromConstraint(c)
    //    c = hex_lower(Duplicate(ls), h)
    //    bset = bset.addConstraint(c)
    //    c = hex_lower_right(Duplicate(ls), h, s0, dl, duh)
    //    bset = bset.addConstraint(c)
    //    c = hex_upper_left(Duplicate(ls), h, dl)
    //    bset = bset.addConstraint(c)
    //    c = hex_upper_right(Duplicate(ls), h, s0, du, dlh, duh)
    //    bset = bset.addConstraint(c)
    //    c = hex_upper(ls)
    //    bset = bset.addConstraint(c)

    var c = hex_lower_left(Duplicate(ls), h, du, duh)
    var bset = c.wrap()
    bset = bset.apply(c)
    c = hex_lower(Duplicate(ls), h)
    bset = bset.intersect(c.wrap())
    c = hex_lower_right(Duplicate(ls), h, s0, dl, duh)
    bset = bset.intersect(c.wrap())
    c = hex_upper_left(Duplicate(ls), h, dl)
    bset = bset.intersect(c.wrap())
    c = hex_upper_right(Duplicate(ls), h, s0, du, dlh, duh)
    bset = bset.intersect(c.wrap())
    c = hex_upper(ls)
    bset = bset.intersect(c.wrap())

    isl.Set.fromBasicSet(bset)
  }

  /**
   * Construct and return the space ts[t, s].
   */
  def construct_ts_space(ctx : isl.Ctx) = {
    val s = isl.Space.setAlloc(ctx, 0, 2)
    s.setTupleName(isl.DimType.Set, ts_space_name)
  }

  /**
   * Construct and return the space local_ts[t, s].
   */
  def construct_local_ts_space(ctx : isl.Ctx) = {
    val s = isl.Space.setAlloc(ctx, 0, 2)
    s.setTupleName(isl.DimType.Set, local_ts_space_name)
  }

  /**
   * Compute the total size of a tile for the space dimensions,
   * i.e., those corresponding to the child node
   * of the input pattern.
   * If S_0 is the original tile size in the first space dimension,
   * then the first entry of "space_sizes" is equal to
   * W = 2*S_0 + floor(d_l h) + floor(d_u h).
   * The remaining entries are the same as in the original tile sizes.
   * "tile_sizes" contains the original tile sizes, including
   * the tile size corresponding to the parent node.
   * "dlh" is equal to floor(d_l h).
   * "duh" is equal to floor(d_u h).
   */
  def compute_space_sizes(tile_sizes : isl.MultiVal, dlh : isl.Val, duh : isl.Val) = {
    var space_sizes : isl.MultiVal = Duplicate(tile_sizes)
    space_sizes = space_sizes.factorRange()
    var size : isl.Val = space_sizes.getVal(0)
    size = size.mulUi(2)
    size = size.add(Duplicate(duh))
    size = size.add(Duplicate(dlh))
    space_sizes = space_sizes.setVal(0, size)
    space_sizes
  }

  /**
   * Compute the offset of phase 1 with respect to phase 0
   * in the ts-space ("space").
   * In particular, return
   *
   * ts[st, s0 + duh]
   */
  def compute_phase_shift(space : isl.Space, st : isl.Val, s0 : isl.Val, duh : isl.Val) = {
    var phase_shift : isl.MultiVal = isl.MultiVal.zero(Duplicate(space))
    phase_shift = phase_shift.setVal(0, Duplicate(st))
    val v = Duplicate(duh).add(Duplicate(s0))
    phase_shift.setVal(1, v)
  }

  /**
   *  Return the function
   *
   * ts[t, s] -> floor(t/(2 * st))
   *
   * representing the time tile.
   * "space" is the space ts[t, s].
   */
  def compute_time_tile(space : isl.Space, st : isl.Val) = {
    val ls : isl.LocalSpace = isl.LocalSpace.fromSpace(Duplicate(space))
    val t : isl.Aff = isl.Aff.varOnDomain(ls, isl.DimType.Set, 0)
    val v : isl.Val = Duplicate(st).mulUi(2)
    t.scaleDownVal(v).floor()
  }

  /**
   * Compute a shift in the space dimension for tiles
   * at time tile T = floor(t/(2 * S_t))
   * such that they align to a multiple of the total space tile dimension W.
   * In particular, compute
   *
   * ts[t, s] -> s + (-(2 * shift_s)*T) % W
   *
   * where shift_s is the shift of phase 1 with respect to phase 0
   * in the space dimension (the first element of "phase_shift").
   * W is stored in the first element of "space_sizes".
   * "time_tile" is the function
   *
   * ts[t, s] -> floor(t/(2 * S_T))
   *
   * Since phase 1 is shifted by shift_s with respect to phase 0,
   * the next line of phase 0 (at T+1) is shifted by 2*shift_s
   * with respect to the previous line (at T).
   * A shift of -(2 * shift_s)*T therefore allows the basic pattern
   * (which starts at 0) to be applied.
   * However, this shift will be used to obtain the tile coordinate
   * in the first space dimension and if the original values
   * in the space dimension are non-negative, then the shift should
   * not make them negative.  Moreover, the shift should be as minimal
   * as possible.
   * Since the pattern repeats itself with a period of W in the space
   * dimension, the shift can be replaced by (-(2 * shift_s)*T) % W.
   */
  def compute_shift_space(time_tile : isl.Aff, space_sizes : isl.MultiVal, phase_shift : isl.MultiVal) = {
    val ls : isl.LocalSpace = isl.LocalSpace.fromSpace(time_tile.getDomainSpace)
    var t : isl.Aff = Duplicate(time_tile)
    var v : isl.Val = phase_shift.getVal(1).mulUi(2)
    v = v.neg()
    t = t.scaleVal(v)
    v = space_sizes.getVal(0)
    t = t.modVal(v)
    val s : isl.Aff = isl.Aff.varOnDomain(ls, isl.DimType.Set, 1)
    s.add(t)
  }

  /**
   * Give the phase_shift ts[S_t, S_0 + floor(d_u h)],
   * compute a function that applies the shift, i.e.,
   *
   * ts[t, s] -> ts[t + S_t, s + S_0 + floor(d_u h)],
   */
  def compute_shift_phase(phase_shift : isl.MultiVal) = {
    var space : isl.Space = phase_shift.getSpace
    val shift : isl.MultiAff = isl.MultiAff.multiValOnSpace(space, Duplicate(phase_shift))
    space = shift.getSpace
    shift.add(isl.MultiAff.identity(space))
  }

  /**
   * Compute a mapping from the ts-space to the local coordinates within each tile.  In particular, compute
   *
   * ts[t, s] -> local_ts[t % (2 S_t), (s + (-(2 * shift_s)*T) % W) % W]
   *
   * "ts" is the space ts[t, s]
   * "local_ts" is the space local_ts[t, s]
   * "shift_space" is equal to ts[t, s] -> s + (-(2 * shift_s)*T) % W
   * "st" is the tile size in the time dimension S_t.
   * The first element of "space_sizes" is equal to W.
   */
  def compute_localize(local_ts : isl.Space, shift_space : isl.Aff, st : isl.Val, space_sizes : isl.MultiVal) = {
    var space : isl.Space = shift_space.getDomainSpace
    space = isl.Space.mapFromDomainAndRange(space, local_ts)
    var localize : isl.MultiAff = isl.MultiAff.identity(space)
    var t : isl.Aff = localize.getAff(0)
    var v : isl.Val = Duplicate(st).mulUi(2)
    t = t.modVal(v)
    localize = localize.setAff(0, t)
    var s : isl.Aff = Duplicate(shift_space)
    v = space_sizes.getVal(0)
    s = s.modVal(v)
    localize.setAff(1, s)
  }

  /**
   * Set the project_ts field of "tiling".
   *
   * This field projects the space of the input schedule to the ts-space.
   * It is equal to [P[t] -> C[s_0, ...]] -> ts[t, s_0].
   */
  def ppcg_ht_tiling_set_project_ts(tiling : ppcg_ht_tiling) = {
    val space : isl.Space = ppcg_ht_tiling_get_input_space(tiling)
    val n = space.dim(isl.DimType.Set)
    var project = isl.MultiAff.projectOutMap(space, isl.DimType.Set, 2, n - 2)
    project = project.setTupleName(isl.DimType.Out, ts_space_name)
    ppcg_ht_tiling(tiling.ref, tiling.bounds, tiling.input_node, tiling.input_schedule, tiling.space_sizes, tiling.time_tile, tiling.local_time, tiling.shift_space, tiling.shift_phase, tiling.hex, project)
  }

  /**
   * Construct a hybrid tiling description from bounds on the dependence distances "bounds".
   * "input_node" points to the original parent node.
   * "input_schedule" is the combined schedule of the parent and child node in the input.
   * "tile_sizes" are the original, user specified tile sizes.
   */
  def ppcg_ht_bounds_construct_tiling(bounds : ppcg_ht_bounds, input_node : isl.ScheduleNode, input_schedule : isl.MultiUnionPwAff, tile_sizes : isl.MultiVal) = {
    val ctx : isl.Ctx = input_schedule.getCtx
    val st : isl.Val = tile_sizes.getVal(0)
    val h = Duplicate(st).subUi(1)
    val s0 = tile_sizes.getVal(1)
    val du = Duplicate(bounds.upper)
    val dl = ppcg_ht_bounds_get_lower(bounds, 0)
    val duh = Duplicate(du).mul(Duplicate(h)).floor()
    val dlh = Duplicate(dl).mul(Duplicate(h)).floor()
    val ts = construct_ts_space(ctx)
    val local_ts = construct_local_ts_space(ctx)

    val space_sizes = compute_space_sizes(tile_sizes, dlh, duh)
    val phase_shift = compute_phase_shift(ts, st, s0, duh)
    val time_tile = compute_time_tile(ts, st)
    val shift_space = compute_shift_space(time_tile, space_sizes, phase_shift)
    val localize = compute_localize(local_ts, shift_space, st, space_sizes)

    var hex = compute_hexagon(local_ts, h, s0, dl, du, dlh, duh)
    hex = hex.preimageMultiAff(localize)
    val tiling : ppcg_ht_tiling = ppcg_ht_tiling(1, bounds, Duplicate(input_node), Duplicate(input_schedule), space_sizes, time_tile, localize.getAff(0), shift_space, compute_shift_phase(phase_shift), hex, null)
    ppcg_ht_tiling_set_project_ts(tiling)
  }

  /**
   * Are all members of the band node "node" coincident?
   */
  def all_coincident(node : isl.ScheduleNode) : Boolean = {
    val n = node.bandNMember()
    (0 until n).foreach(x => {
      if (!node.bandMemberGetCoincident(x))
        return false
    })

    true
  }

  /**
   * Does "node" satisfy the properties of the inner node in the input pattern for hybrid tiling?
   * That is, is it a band node with only coincident members, of which there is at least one?
   */
  def has_child_properties(node : isl.ScheduleNode) : Boolean = {
    if (!isl.ScheduleNodeType.NodeBand.equals(node.getType) || node.bandNMember() < 1) {
      false
    } else {
      all_coincident(node)
    }
  }

  /**
   * Does "node" satisfy the properties of the outer node in the input pattern for hybrid tiling?
   * That is, is it a band node with a single member?
   */
  def has_parent_properties(node : isl.ScheduleNode) : Boolean = {
    !(!isl.ScheduleNodeType.NodeBand.equals(node.getType) || node.bandNMember() != 1)
  }

  /**
   *  Does the parent of "node" satisfy the input patttern for hybrid tiling?
   * That is, does "node" satisfy the properties of the inner node and does the parent of "node" satisfy the properties
   * of the outer node?
   */
  def ppcg_ht_parent_has_input_pattern(node : isl.ScheduleNode) : Boolean = {
    var has_pattern = has_child_properties(node)

    if (!has_pattern)
      return has_pattern

    var localNode = Duplicate(node)
    localNode = localNode.parent()
    has_pattern = has_parent_properties(node)
    has_pattern
  }

  /**
   * Does "node" satisfy the input patttern for hybrid tiling?
   * That is, does "node" satisfy the properties of the outer node and does the child of "node" satisfy the properties
   * of the inner node?
   */
  def ppcg_ht_has_input_pattern(node : isl.ScheduleNode) : Boolean = {
    val has_pattern = has_parent_properties(node)

    if (!has_pattern)
      return has_pattern

    val localNode = node.getChild(0)
    has_child_properties(localNode)
  }

  /**
   * Check that "node" satisfies the input pattern for hybrid tiling. Error out if it does not.
   */
  def check_input_pattern(node : isl.ScheduleNode) = {
    val has_pattern = ppcg_ht_has_input_pattern(node)

    if (!has_pattern)
      throw new Exception("invalid input pattern for hybrid tiling")

    has_pattern
  }

  /**
   * Extract the input schedule from "node", i.e., the product of the partial schedules of the parent and child nodes
   * in the input pattern.
   */
  def extract_input_schedule(node : isl.ScheduleNode) = {
    val partial = node.bandGetPartialSchedule()
    val localNode = node.getChild(0)
    val partial2 = localNode.bandGetPartialSchedule()
    partial.rangeProduct(partial2)
  }

  /**
   * Collect all dependences from "scop" that are relevant for performing hybrid tiling on "node" and its child and map
   * them to the schedule space of this pair of nodes.
   *
   * In case live range reordering is not used, the flow and the false dependences are collected.
   * In case live range reordering is used, the flow and the forced dependences are collected, as well as the order
   * dependences that are adjacent to non-local flow dependences.
   *
   * In all cases, only dependences that map to the same instance of the outer part of the schedule are considered.
   */
  def collect_deps(scop : Scop, node : isl.ScheduleNode) : isl.Map = {
    isl.Map.fromUnionMap(scop.deps.validity())
  }

  /**
   * Given a constraint of the form
   *
   * a i_0 + b i_1 >= 0
   * or
   * a i_0 + b i_1 = 0
   *
   * use it to update one or both of the non-negative bounds in "list" = (min, max) such that
   *
   * i_1 >= -min i_0
   * and
   * i_1 <= max i_0
   *
   * If b = 0, then the constraint cannot be used. Otherwise, the constraint is equivalent to
   *
   * sgn(b) i_1 >= - a/abs(b) i_0
   * i.e.,
   * i_1 >= - a/abs(b) i_0
   * or
   * i_1 <= a/abs(b) i_0
   *
   * Set the first or second element of "list" to max(0, a/abs(b)), according to the sign of "b".  Or set both in
   * case the constraint is an equality, taking into account the sign change.
   */
  def list_set_min_max(list : isl.ValList, c : isl.Constraint) : isl.ValList = {
    val eq = c.isEquality
    var b = c.getCoefficientVal(isl.DimType.Set, 1)
    val is_zero = b.isZero

    if (is_zero) {
      return list
    }

    var a = c.getCoefficientVal(isl.DimType.Set, 0)
    val sign = b.sgn()
    b = b.abs()
    a = a.div(b)

    if (eq)
      b = Duplicate(a)

    var pos = if (sign > 0) 0 else 1
    var is_neg = a.isNeg

    if (is_neg)
      a = a.setSi(0)

    val localList = list.setVal(pos, a)

    if (!eq)
      return localList

    pos = 1 - pos
    a = b.neg()
    is_neg = a.isNeg

    if (is_neg)
      a = a.setSi(0)

    localList.setVal(pos, a)
  }

  /**
   * Given a set of dependence distance vectors "dist", compute pair of non-negative bounds min and max such that
   *
   * d_pos >= -min d_0
   * and
   * d_pos <= max d_0
   *
   * and return the pair (min, max).
   * If no bound can be found in either direction, then the bound is replaced by NaN.
   *
   * The dependence distances are first projected onto the (d_0, d_pos).
   * Then the zero dependence distance is added and the convex hull is computed.
   * Finally, the bounds are extracted from the constraints of the convex hull that pass through the origin.
   */
  def min_max_dist(dist : isl.Set, pos : Int) = {
    val ctx = dist.getCtx
    val nan = isl.Val.nan(ctx)
    var list = isl.ValList.alloc(ctx, 2)
    list = list.add(Duplicate(nan))
    list = list.add(nan)
    var localDist = Duplicate(dist)
    val dim = dist.dim(isl.DimType.Set)

    if (pos >= dim)
      throw new Exception("position out of bounds")

    localDist = localDist.projectOut(isl.DimType.Set, pos + 1, dim - (pos + 1))
    localDist = localDist.projectOut(isl.DimType.Set, 1, pos - 1)

    val space = localDist.getSpace
    localDist = localDist.union(isl.Set.fromPoint(isl.Point.zero(space)))
    localDist = localDist.removeDivs()

    //TODO: val hull = dist.convexHull()
    //TODO: if (isl_basic_set_foreach_constraint(hull, &set_min_max, &list) < 0)
    //        list = isl_val_list_free(list);
    //      isl_basic_set_free(hull);

    list
  }

  /**
   * Given a schedule node "node" that, together with its child, satisfies the input pattern for hybrid tiling, compute
   * bounds on the relative dependence distances of the child node with respect to the parent node.  These bounds are
   * needed to construct a hybrid tiling.
   *
   * First all relevant dependences are collected and mapped to the schedule space of the pair of nodes.  Then, the
   * dependence distances are computed in this space.
   *
   * These dependence distances are then projected onto a two-dimensional space consisting of the single schedule
   * dimension of the outer node and one of the schedule dimensions of the inner node. The maximal and minimal relative
   * dependence distances are extracted from these projections. This process is repeated for each of the schedule
   * dimensions of the inner node.  For the first dimension, both minimal and maximal relative dependence distances
   * are stored in the result. For the other dimensions, only the minimal relative dependence distance is stored.
   */
  def ppcg_ht_compute_bounds(scop : Scop, node : isl.ScheduleNode) = {
    val child = node.getChild(0)
    val space = child.bandGetSpace()
    val dim = child.bandNMember()
    var bnd = ppcg_ht_bounds_alloc(space)
    val map = collect_deps(scop, node)
    var dist = map.deltas()
    val n = dist.dim(isl.DimType.Param)
    dist = dist.projectOut(isl.DimType.Param, 0, n)
    var pair = min_max_dist(dist, 1)
    bnd = ppcg_ht_bounds_set_lower(bnd, 0, pair.getVal(0))
    bnd = ppcg_ht_bounds_set_upper(bnd, pair.getVal(1))

    (1 until dim).foreach(x => {
      pair = min_max_dist(dist, 1 + x)
      bnd = ppcg_ht_bounds_set_lower(bnd, x, pair.getVal(0))
    })

    bnd
  }

  /**
   * Construct a ppcg_ht_phase object, that simply copies information from "tiling".
   * That is, the result is defined over the "ts" space and corresponds to phase 1.
   */
  def construct_phase(tiling : ppcg_ht_tiling) = {
    ppcg_ht_phase(ppcg_ht_tiling_copy(tiling), Duplicate(tiling.time_tile), Duplicate(tiling.local_time), Duplicate(tiling.shift_space), Duplicate(tiling.hex), null, null)
  }

  /**
   * Align the parameters of the elements of "phase" to those of "space".
   */
  def phase_align_params(phase : ppcg_ht_phase, space : isl.Space) = {
    val time_tile = phase.time_tile.alignParams(Duplicate(space))
    val local_time = phase.local_time.alignParams(Duplicate(space))
    val shift_space = phase.shift_space.alignParams(Duplicate(space))
    val domain = phase.domain.alignParams(Duplicate(space))

    ppcg_ht_phase(phase.tiling, time_tile, local_time, shift_space, domain, phase.space_shift, phase.space_tile)
  }

  /**
   * Pull back "phase" over "ma".
   * That is, take a phase defined over the range of "ma" and turn it into a phase defined over the domain of "ma".
   */
  def pullback_phase(phase : ppcg_ht_phase, ma : isl.MultiAff) = {
    val localPhase = phase_align_params(phase, ma.getSpace)
    val time_tile = localPhase.time_tile.pullbackMultiAff(Duplicate(ma))
    val local_time = localPhase.local_time.pullbackMultiAff(Duplicate(ma))
    val shift_space = localPhase.shift_space.pullbackMultiAff(Duplicate(ma))
    val domain = localPhase.domain.preimageMultiAff(ma)

    ppcg_ht_phase(phase.tiling, time_tile, local_time, shift_space, domain, phase.space_shift, phase.space_tile)
  }

  /**
   *  Pullback "phase" over phase->tiling->shift_phase, which shifts phase 0 to phase 1.
   *  The pullback therefore takes a phase 1 description and turns it into a phase 0 description.
   */
  def shift_phase(phase : ppcg_ht_phase) = {
    val tiling = phase.tiling
    pullback_phase(phase, Duplicate(tiling.shift_phase))
  }

  /**
   * Take a "phase" defined over the ts-space and plug in the projection from the input schedule space to the ts-space.
   * The result is then defined over this input schedule space.
   */
  def lift_phase(phase : ppcg_ht_phase) = {
    val tiling = phase.tiling
    pullback_phase(phase, Duplicate(tiling.project_ts))
  }

  /**
   * Compute the shift that should be added to the space band in order to be able to apply rectangular tiling to the
   * space.
   * Store the shift in phase->space_shift.
   *
   * In the first dimension, it is equal to shift_space - s.
   * For phase 1, this results in
   *
   * (-(2 * shift_s)*T) % W
   *
   * In phase 0, the "s" in shift_space has been replaced by "s + shift_s", so the result is
   *
   * shift_s + (-(2 * shift_s)*T) % W
   *
   * In the other dimensions, the shift is equal to
   *
   * dl_i * local_time.
   */
  def compute_space_shift(phase : ppcg_ht_phase) = {
    var space = ppcg_ht_phase_get_input_space(phase)
    space = space.unwrap()
    space = space.rangeMap()
    var space_shift = isl.MultiAff.zero(space)
    var aff = Duplicate(phase.shift_space)
    val ls = isl.LocalSpace.fromSpace(aff.getDomainSpace)
    val s = isl.Aff.varOnDomain(ls, isl.DimType.Set, 1)
    aff = aff.sub(s)
    space_shift = space_shift.setAff(0, aff)
    val n = space_shift.dim(isl.DimType.Out)
    (1 until n).foreach(x => {
      val v = ppcg_ht_bounds_get_lower(phase.tiling.bounds, x)
      var time = Duplicate(phase.local_time)
      time = time.scaleVal(v)
      space_shift = space_shift.setAff(x, time)
    })

    ppcg_ht_phase(phase.tiling, phase.time_tile, phase.local_time, phase.shift_space, phase.domain, space_shift, phase.space_tile)
  }

  /**
   * Compute the space tiling and store the result in phase->space_tile.
   * The space tiling is of the form
   *
   * [P[t] -> C[s]] -> C[floor((s + space_shift)/space_size]
   */
  def compute_space_tile(phase : ppcg_ht_phase) = {
    var space = ppcg_ht_phase_get_input_space(phase)
    space = space.unwrap()
    var tile = isl.MultiAff.rangeMap(space)
    val space_shift = Duplicate(phase.space_shift)
    tile = space_shift.add(tile)
    val space_sizes = Duplicate(phase.tiling.space_sizes)
    tile = tile.scaleDownMultiVal(space_sizes)
    tile = tile.floor()

    ppcg_ht_phase(phase.tiling, phase.time_tile, phase.local_time, phase.shift_space, phase.domain, phase.space_shift, tile)
  }

  /**
   * Construct a representation for one of the two phase for hybrid tiling "tiling".
   * If "shift" is not set, then the phase is constructed directly from the hexagonal tile shape in "tiling", which
   * represents the phase-1 tiles.  If "shift" is set, then this tile shape is shifted back over tiling->shift_phase to
   * obtain the phase-0 tiles.
   *
   * First copy data from "tiling", then optionally shift the phase and finally move the tiling from the "ts" space of
   * "tiling" to the space of the input pattern.
   *
   * After the basic phase has been computed, also compute the corresponding space shift.
   */
  def ppcg_ht_tiling_compute_phase(tiling : ppcg_ht_tiling, shift : Int) = {
    var phase = construct_phase(tiling)

    if (shift > 0)
      phase = shift_phase(phase)

    phase = lift_phase(phase)

    phase = compute_space_shift(phase)
    phase = compute_space_tile(phase)

    phase
  }

  /**
   * Consruct a function that is equal to the time tile of "phase0" on the domain of "phase0" and equal to the time
   * tile of "phase1" on the domain of "phase1".
   * The two domains are assumed to form a partition of the input schedule space.
   */
  def combine_time_tile(phase0 : ppcg_ht_phase, phase1 : ppcg_ht_phase) = {
    var T = Duplicate(phase0.time_tile)
    var time = isl.PwAff.alloc(Duplicate(phase0.domain), T)

    T = Duplicate(phase1.time_tile)
    val time1 = isl.PwAff.alloc(Duplicate(phase1.domain), T)

    time = time.unionAdd(time1)
    isl.PwMultiAff.fromPwAff(time)
  }

  /**
   * Does "id" contain a pointer to a ppcg_ht_phase? That is, is it called "phase"?
   */
  def is_phase_id(id : isl.Id) : Boolean = {
    val name = id.getName
    name.startsWith(ppcg_phase_name)
  }

  /**
   * Given a mark node with an identifier that points to a ppcg_ht_phase, extract this ppcg_ht_phase pointer.
   */
  def ppcg_ht_phase_extract_from_mark(node : isl.ScheduleNode) : ppcg_ht_phase = {
    if (!isl.ScheduleNodeType.NodeMark.equals(node.getType))
      throw new Exception("not a phase mark")

    val id = node.markGetId()
    val is_phase = is_phase_id(id)

    if (!is_phase || !mark_map.contains(id.getName))
      throw new Exception("not a phase mark")

    mark_map(id.getName)
  }

  /**
   * Insert a mark node at "node" holding a pointer to "phase".
   */
  def insert_phase(node : isl.ScheduleNode, phase : ppcg_ht_phase) = {
    val ctx = node.getCtx
    val idName = ppcg_phase_name + mark_count
    val id = isl.Id.alloc(ctx, idName)
    mark_count = mark_count + 1

    mark_map(idName) = phase
    node.insertMark(id)
  }

  /**
   * Construct a mapping from the elements of the original pair of bands to which tiling was applied that belong to a
   * tile of "phase" to that tile, preserving the values for the outer bands.
   *
   * The mapping is of the form [ [outer] -> [P -> C -> [ [outer] -> [tile] ]
   *
   * where tile is defined by a concatenation of the time_tile and the space_tile.
   */
  def construct_tile_map(phase : ppcg_ht_phase) = {
    val depth = phase.tiling.input_node.getScheduleDepth
    var space = phase.time_tile.getSpace
    space = space.params()
    space = space.setFromParams()
    space = space.addDims(isl.DimType.Set, depth)
    space = isl.Space.mapFromSet(space)
    val ma = isl.MultiAff.identity(space)
    val tiling = isl.MultiAff.fromAff(Duplicate(phase.time_tile)).flatRangeProduct(Duplicate(phase.space_tile))
    var el2tile = isl.Map.fromMultiAff(tiling)
    el2tile = el2tile.intersectDomain(Duplicate(phase.domain))
    el2tile = el2tile.product(isl.Map.fromMultiAff(ma))
    el2tile
  }

  /**
   *  Return a description of the full tiles of "phase" at the point in the original schedule tree where the tiling was
   *  applied.
   *
   * First construct a mapping from the input schedule dimensions up to an including the original pair of bands to which
   * hybrid tiling was applied to schedule dimensions in which this original pair has been replaced by the tiles.
   * This mapping is of the form
   *
   *  [ [outer] -> [P -> C] ] -> [ [outer] -> [tile] ]
   *
   * Apply this mapping to the set of all values for the input schedule dimensions and then apply its inverse.
   * The result is the set of values for the input schedule dimensions that would map to any of the tiles.  Subtracting
   * from this set the set of values that are actually executed produces the set of values that belong to a tile but
   * that are not executed. Mapping these back to the tiles produces a description of the partial tiles.
   * Subtracting these from the set of all tiles produces a description of the full tiles in the form
   *
   *  [ [outer] -> [tile] ]
   */
  def compute_full_tile(phase : ppcg_ht_phase) = {
    val el2tile = construct_tile_map(phase)
    val tile2el = Duplicate(el2tile).reverse()
    val node = phase.tiling.input_node
    val prefix = node.getPrefixScheduleUnionMap
    val domain = node.domainGetDomain()
    val mupa = Duplicate(phase.tiling.input_schedule)
    var schedule = isl.UnionMap.fromMultiUnionPwAff(mupa)
    schedule = prefix.rangeProduct(schedule)
    var all_el = isl.Set.fromUnionSet(domain.apply(schedule))
    all_el = all_el.coalesce()
    val all = Duplicate(all_el).apply(Duplicate(el2tile))

    var partial = Duplicate(all)
    partial = partial.apply(tile2el)
    partial = partial.subtract(all_el)
    partial = partial.apply(el2tile)
    all.subtract(partial)
  }

  /**
   * Copy the AST loop types of the non-isolated part to those of the isolated part.
   */
  def set_isolate_loop_type(node : isl.ScheduleNode) = {
    val n = node.bandNMember()
    var localNode = node
    (0 until n).foreach(x => {
      val nodeType = localNode.bandMemberGetAstLoopType(x)
      localNode = localNode.bandMemberSetIsolateAstLoopType(x, nodeType)
    })

    node
  }

  /**
   * If options->isolate_full_tiles is set, then mark the full tiles in "node" for isolation.
   * The full tiles are derived from "phase". "node" may point to a part of the tiling, e.g., the space tiling.
   *
   * The full tiles are originally computed in the form
   *
   * [ [outer] -> [tile] ]
   *
   * However, the band that "node" points to may only contain subset of the tile dimensions.
   * The description above is therefore treated as
   *
   * [ [outer] -> [before; this; after] ]
   *
   * before is of size "pos"; this is of size "dim"; and after is of size "out - pos - dim".
   * The after part is first project out.  Then the range is split into a before and this part and finally the before
   * part is moved to the domain, resulting in
   *
   * [ [outer; before] -> [this] ]
   *
   * This description is then used as the isolate option.
   *
   * The AST loop type for the isolated part is set to be the same as that of the non-isolated part.
   */
  def ppcg_ht_phase_isolate_full_tile_node(phase : ppcg_ht_phase, node : isl.ScheduleNode) : ScheduleNode = {
    val depth = node.getScheduleDepth
    val dim = node.bandNMember()
    val tile = compute_full_tile(phase)
    var map = tile.unwrap()
    val in = map.dim(isl.DimType.In)
    val out = map.dim(isl.DimType.Out)
    val pos = depth - in
    map = map.projectOut(isl.DimType.Out, pos + dim, out - (pos + dim))
    val space = map.getSpace.range()
    var ma1 = isl.MultiAff.projectOutMap(Duplicate(space), isl.DimType.Set, pos, dim)
    val ma2 = isl.MultiAff.projectOutMap(Duplicate(space), isl.DimType.Set, 0, pos)
    ma1 = ma1.rangeProduct(ma2)
    map = map.applyRange(isl.Map.fromMultiAff(ma1))
    map = map.uncurry()
    map = map.flattenDomain()
    var set = map.wrap()
    set = set.setTupleName("isolate")

    var opt1 = node.bandGetAstBuildOptions()
    opt1 = opt1.addSet(set)
    var localNode = node.bandSetAstBuildOptions(opt1)
    localNode = set_isolate_loop_type(localNode)

    localNode
  }

  /**
   * Insert a band node for performing the space tiling for "phase" at "node".
   * In particular, insert a band node with partial schedule
   *
   * [P[t] -> C[s] ] -> C[floor((s + space_shift)/space_size)]
   *
   * pulled back over the input schedule.
   *
   * The first tile dimension iterates over the hexagons in the same phase, which are independent by construction.
   * The first dimension is therefore marked coincident.
   * All dimensions are also marked for being generated as atomic loops because separation is usually not desirable on tile loops.
   */
  def insert_space_tiling(phase : ppcg_ht_phase, node : isl.ScheduleNode) = {
    val space_tile = Duplicate(phase.space_tile)
    var mupa = Duplicate(phase.tiling.input_schedule)
    mupa = mupa.applyMultiAff(space_tile)
    var localNode = node.insertPartialSchedule(mupa)

    val n = localNode.bandNMember()
    (0 until n).foreach(x => {
      localNode = localNode.bandMemberSetAstLoopType(x, isl.AstLoopType.LoopAtomic)
    })

    localNode = ppcg_ht_phase_isolate_full_tile_node(phase, localNode)
    localNode = localNode.bandMemberSetCoincident(0, 1)
    localNode
  }

  /**
   * Given a pointer "node" to (a copy of) the original child node in the input pattern, adjust its partial schedule
   * such that it starts at zero within each tile.
   *
   * That is, replace "s" by (s + space_shift) % space_sizes.
   */
  def ppcg_ht_phase_shift_space_point(phase : ppcg_ht_phase, node : isl.ScheduleNode) = {
    val space_shift = Duplicate(phase.space_shift)
    var mupa = Duplicate(phase.tiling.input_schedule)
    mupa = mupa.applyMultiAff(space_shift)
    var localNode = node.bandShift(mupa)
    val space_sizes = Duplicate(phase.tiling.space_sizes)
    localNode = localNode.bandMod(space_sizes)
    localNode
  }

  /**
   * Does
   *
   * s0 > delta + 2 * {delta * h} - 1
   *
   * hold?
   */
  def wide_enough(s0 : isl.Val, delta : isl.Val, h : isl.Val) = {
    var v = Duplicate(delta).mul(Duplicate(h))
    val v2 = Duplicate(v).floor()
    v = v.sub(v2)
    v = v.mulUi(2)
    v = v.add(Duplicate(delta))
    v = v.sub(isl.Val.fromInt(v.getCtx, 1))
    s0.gt(v)
  }

  /**
   * Is the tile size specified by "sizes" wide enough in the first space dimension, i.e., the base of the hexagon?
   * This ensures that, after hybrid tiling using "bounds" and these sizes, neighboring hexagons in the same phase
   * are far enough apart that they do not depend on each other. The test is only meaningful if the bounds are valid.
   *
   * Let st be (half) the size in the time dimension and s0 the base size in the first space dimension.
   * Let delta be the dependence distance in either positive or negative direction.
   * In principle, it should be enough to have s0 + 1 > delta, i.e., s0 >= delta.
   * However, in case of fractional delta, the tile is not extended with delta * (st - 1), but instead with floor(delta * (st - 1)).
   * The condition therefore needs to be adjusted to
   *
   * s0 + 1 > delta + 2 {delta * (st - 1)}
   *
   * (with {} the fractional part) to account for the two slanted sides.
   * The condition in the paper "Hybrid Hexagonal/Classical Tiling for GPUs" translates to
   *
   * s0 >= delta + {delta * (st - 1)}
   *
   * Since 1 > frac(delta * (st - 1)), this condition implies the condition above.
   *
   * The condition is checked for both directions.
   */
  def ppcg_ht_bounds_supports_sizes(bounds : ppcg_ht_bounds, sizes : isl.MultiVal) : Boolean = {
    var ok = ppcg_ht_bounds_is_valid(bounds)
    if (!ok)
      return ok

    val h = sizes.getVal(0).sub(isl.Val.fromInt(sizes.getCtx, 1))
    val s0 = sizes.getVal(1)
    var delta = ppcg_ht_bounds_get_lower(bounds, 0)

    ok = wide_enough(s0, delta, h)

    delta = Duplicate(bounds.upper)
    if (ok)
      ok = wide_enough(s0, delta, h)

    ok
  }

  /**
   * Check that the tile will be wide enough in the first space dimension, i.e., the base of the hexagon.
   * This ensures that neighboring hexagons in the same phase are far enough apart that they do not depend on each other.
   *
   * Error out if the condition fails to hold.
   */
  def check_width(bounds : ppcg_ht_bounds, sizes : isl.MultiVal) : Boolean = {
    val ok = ppcg_ht_bounds_supports_sizes(bounds, sizes)
    if (!ok)
      throw new Exception("base of hybrid tiling hexagon not sufficiently wide")

    true
  }

  /**
   * Given valid bounds on the relative dependence distances for the pair of nested nodes that "node" point to, as
   * well as sufficiently wide tile sizes "sizes", insert the corresponding time and space tiling at "node", along
   * with a pair of phase nodes that can be used to make further changes. The space of "sizes" should be the product of
   * the spaces of the schedules of the pair of parent and child nodes.
   *
   * In particular, given an input of the form
   *
   * P - C - ...
   *
   * the output has the form
   *
   *         /- F0 - M0 - CT0 - P - C - ...
   * PT - seq
   *         \- F1 - M1 - CT1 - P - C - ...
   *
   * PT is the global time tiling. Within each of these tiles, two phases are executed in order. Within each phase, the schedule
   * space is further subdivided into tiles through CT0 and CT1. The first dimension of each of these iterates over the hexagons
   * within a phase and these are independent by construction. The F0 and F1 filters filter the statement instances that belong
   * to the corresponding phase.  The M0 and M1 marks contain a pointer to a ppcg_ht_phase object that can be used to
   * perform further changes.
   *
   * After checking that input satisfies the requirements, a data structure is constructed that represents the tiling and
   * two additional data structures are constructed for the two phases of the tiling. These are then used to define the
   * filters F0 and F1 and combined to construct the time tiling PT. Then the time tiling node PT is inserted,
   * followed by the sequence with the two filters, the CT space tiling nodes and the phase markers M.
   */
  def ppcg_ht_bounds_insert_tiling(bounds : ppcg_ht_bounds, sizes : isl.MultiVal, node : isl.ScheduleNode) : ScheduleNode = {
    val ctx = node.getCtx
    val input = extract_input_schedule(node)

    val tiling = ppcg_ht_bounds_construct_tiling(bounds, node, input, sizes)
    val phase_0 = ppcg_ht_tiling_compute_phase(tiling, 1)
    val phase_1 = ppcg_ht_tiling_compute_phase(tiling, 0)
    val time = combine_time_tile(phase_0, phase_1)

    val upma = isl.UnionPwMultiAff.fromMultiUnionPwAff(Duplicate(input))
    var phase0 = isl.UnionSet.fromSet(Duplicate(phase_0.domain))
    phase0 = phase0.preimageUnionPwMultiAff(Duplicate(upma))
    var phase1 = isl.UnionSet.fromSet(Duplicate(phase_1.domain))
    phase1 = phase1.preimageUnionPwMultiAff(upma)

    var phases = isl.UnionSetList.alloc(ctx, 2)
    phases = phases.add(phase0)
    phases = phases.add(phase1)

    val dom_time = input.applyPwMultiAff(time)
    var localNode = node.insertPartialSchedule(dom_time)
    localNode = localNode.child(0)
    localNode = localNode.insertSequence(phases)
    localNode = localNode.child(0)
    localNode = localNode.child(0)
    localNode = insert_space_tiling(phase_0, node)
    localNode = insert_phase(node, phase_0)
    localNode = localNode.parent()
    localNode = localNode.nextSibling()
    localNode = localNode.child(0)
    localNode = insert_space_tiling(phase_1, node)
    localNode = insert_phase(node, phase_1)
    localNode = localNode.parent()
    localNode = localNode.parent()
    localNode = localNode.parent()
    localNode
  }

  /**
   * Given a branch "node" that contains a sequence node with two phases of hybrid tiling as input, call "fn" on each
   * of the two phase marker nodes.
   *
   * That is, the input is as follows
   *
   *          /- F0 - M0 - ...
   * ... - seq
   *          \- F1 - M1 - ...
   *
   * and "fn" is called on M0 and on M1.
   */
  def hybrid_tile_foreach_phase(node : isl.ScheduleNode, fn : isl.ScheduleNode => isl.ScheduleNode) : ScheduleNode = {
    val depth0 = node.getTreeDepth
    var localNode = node

    while (!isl.ScheduleNodeType.NodeSequence.equals(localNode.getType)) {
      localNode = localNode.child(0)
    }

    localNode = localNode.child(0)
    localNode = localNode.child(0)
    localNode = fn(localNode)
    localNode = localNode.parent()
    localNode = localNode.nextSibling()
    localNode = localNode.child(0)
    localNode = fn(localNode)
    localNode = localNode.parent()
    localNode = localNode.parent()

    val depth = localNode.getTreeDepth
    localNode = localNode.ancestor(depth - depth0)
    localNode
  }

  /**
   * This function is called on each of the two phase marks in a hybrid tiling tree. Drop the phase mark at "node".
   */
  def drop_phase_mark(node : isl.ScheduleNode) : ScheduleNode = {
    if (!isl.ScheduleNodeType.NodeMark.equals(node.getType))
      return node

    var localNode = node
    val id = node.markGetId()
    val is_phase = is_phase_id(id)

    if (is_phase)
      localNode = node.delete()

    localNode
  }

  /**
   * Given a branch "node" that contains a sequence node with two phases of hybrid tiling as input, remove the two
   * phase marker nodes.
   *
   * That is, the input is as follows
   *
   *          /- F0 - M0 - ...
   * ... - seq
   *          \- F1 - M1 - ...
   *
   * and the output is
   *
   *          /- F0 - ...
   * ... - seq
   *          \- F1 - ...
   */
  def hybrid_tile_drop_phase_marks(node : isl.ScheduleNode) = {
    hybrid_tile_foreach_phase(node, drop_phase_mark)
  }
}

/**
 * Bounds on relative dependence distances in input to hybrid tiling.
 * upper is an upper bound on the relative dependence distances in the first space dimension
 * -lower is a lower bound on the relative dependence distances in all space dimensions.
 *
 * In particular,
 *
 * d_i >= -lower_i d_0
 * and
 * d_1 <= upper d_0
 *
 * for each dependence distance vector d, where d_1 is the component corresponding to the first space dimension.
 *
 * upper and lower are always non-negative. Some of the values may be NaN if no bound could be found.
 */
case class ppcg_ht_bounds(upper : isl.Val, lower : isl.MultiVal) {
}

/**
 * Structure that represents the basic hexagonal tiling, along with information that is needed to perform the hybrid tiling.
 *
 * "bounds" are the bounds on the dependence distances that define the hexagonal shape and the required skewing in the remaining
 * space dimensions.
 * "input_node" points to the input pair of band nodes.
 * "input_schedule" is the partial schedule of this input pair of band nodes.
 * The space of this schedule is [P -> C], where P is the space of the parent node and C is the space of the child node.
 *
 * "space_sizes" represent the total size of a tile for the space dimensions, i.e., those corresponding to the child node.
 * The space of "space_sizes" is C. If S_0 is the original tile size in the first space dimension, then the first entry of
 * "space_sizes" is equal to W = 2*S_0 + floor(d_l h) + floor(d_u h). The remaining entries are the same as in the
 * original tile sizes.
 *
 * The basic hexagonal tiling "hex" is defined in a "ts" (time-space) space and corresponds to the phase-1 tiles.
 * "time_tile" maps the "ts" space to outer time tile.
 * Is is equal to ts[t, s] -> floor(t/(2 * S_t)), with S_t the original tile size corresponding to the parent node.
 * "local_time" maps the "ts" space to the time dimension inside each tile.
 * It is equal to ts[t, s] -> t mod (2 S_t), with S_t the original tile size corresponding to the parent node.
 * "shift_space" shifts the tiles at time tile T = floor(t/(2 S_t)) in the space dimension such that they align to a multiple of W.
 * It is equal to ts[t, s] -> s + (-(2 * shift_s)*T) % W, with shift_s = S_0 + floor(d_u h).
 * "shift_phase" is the shift taken to go from phase 0 to phase 1
 * It is equal to ts[t, s] -> ts[t + S_t, s + shift_s], with shift_s = S_0 + floor(d_u h).
 *
 * "project_ts" projects the space of the input schedule to the ts-space.
 * It is equal to [P[t] -> C[s_0, ...]] -> ts[t, s_0].
 */
case class ppcg_ht_tiling(ref : Int, bounds : ppcg_ht_bounds, input_node : isl.ScheduleNode,
    input_schedule : isl.MultiUnionPwAff,
    space_sizes : isl.MultiVal,
    time_tile : isl.Aff,
    local_time : isl.Aff,
    shift_space : isl.Aff,
    shift_phase : isl.MultiAff,
    hex : isl.Set,
    project_ts : isl.MultiAff) {
}

/**
 * Representation of one of the two phases of hybrid tiling.
 *
 * "tiling" points to the shared tiling data.
 *
 * "time_tile", "local_time" and "shift_space" are equal to the corresponding fields of "tiling", pulled back to the input space.
 * In case of phase 0, these expressions have also been moved from phase 1 to phase 0.
 *
 * "domain" contains the hexagonal tiling of this phase.
 *
 * "space_shift" is the shift that should be added to the space band in order to be able to apply rectangular tiling to the space.
 * For phase 1, it is equal to
 *
 * [P[t] -> C[s_0, s_i]] -> C[(-(2 * shift_s)*T) % W, dl_i * u]
 *
 * with shift_s = S_0 + floor(d_u h), T equal to "time_tile" and u equal to "local_time".
 * For phase 0, it is equal to
 *
 * [P[t] -> C[s_0, s_i]] -> C[shift_s + (-(2 * shift_s)*T) % W, dl_i * u]
 *
 * "space_tile" is the space tiling.  It is equal to
 *
 * [P[t] -> C[s]] -> C[floor((s + space_shift)/space_size]
 */
case class ppcg_ht_phase(tiling : ppcg_ht_tiling, time_tile : isl.Aff, local_time : isl.Aff, shift_space : isl.Aff,
    domain : isl.Set, space_shift : isl.MultiAff, space_tile : isl.MultiAff) {
}
