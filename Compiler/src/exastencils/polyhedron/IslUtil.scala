package exastencils.polyhedron

import isl.Conversions._

object Isl {

  def simplify(uset : isl.UnionSet) : isl.UnionSet = {
    if (uset == null)
      return null
    var nju : isl.UnionSet = isl.UnionSet.empty(uset.getSpace())
    uset.coalesce().foreachSet({ set : isl.Set => nju = nju.addSet(set.removeRedundancies()) })
    return nju
  }

  def simplify(umap : isl.UnionMap) : isl.UnionMap = {
    if (umap == null)
      return null
    var nju : isl.UnionMap = isl.UnionMap.empty(umap.getSpace())
    umap.coalesce().foreachMap({ map : isl.Map => nju = nju.addMap(map.removeRedundancies()) })
    return nju
  }

  def simplify(set : isl.Set) : isl.Set = {
    if (set == null)
      return null
    return set.coalesce().removeRedundancies()
  }

  def simplify(map : isl.Map) : isl.Map = {
    if (map == null)
      return null
    return map.coalesce().removeRedundancies()
  }
}
