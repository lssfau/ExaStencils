package exastencils.polyhedron

import isl.Conversions.convertLambdaToVoidCallback1

object Isl {

  def simplify(uset : isl.UnionSet) : isl.UnionSet = {
    var nju : isl.UnionSet = isl.UnionSet.empty(uset.getSpace())
    uset.coalesce().foreachSet({ set : isl.Set => nju = nju.addSet(set.removeRedundancies()) })
    return nju
  }

  def simplify(umap : isl.UnionMap) : isl.UnionMap = {
    var nju : isl.UnionMap = isl.UnionMap.empty(umap.getSpace())
    umap.coalesce().foreachMap({ map : isl.Map => nju = nju.addMap(map.removeRedundancies()) })
    return nju
  }

  def simplify(set : isl.Set) : isl.Set = {
    return set.coalesce().removeRedundancies()
  }

  def simplify(map : isl.Map) : isl.Map = {
    return map.coalesce().removeRedundancies()
  }
}
