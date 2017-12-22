package test.isl

import exastencils.polyhedron.Isl

object Bindings {

  def main(args : Array[String]) = {

    println("start...")

    val ctx = Isl.ctx
    val s = isl.Set.readFromStr(ctx, "[n] -> { [i] : 0<=i<=100 and i<=n }")
    println(s)
    val lm = s.lexmax
    println(lm)

    lm.foreachBasicSet((bs : isl.BasicSet) => println(bs.samplePoint))

    val writes = isl.UnionMap.readFromStr(ctx, "{ S[i,j] -> A[i,j] }")
    val reads = isl.UnionMap.readFromStr(ctx, "{ T[i,j] -> A[i,j] }")
    val empty = isl.UnionMap.empty(writes.getSpace())
    val schedule = isl.UnionMap.readFromStr(ctx, "{ S[i,j] -> [i,j,0]; T[i,j] -> [i,j,1] }")
    val depArr = new Array[isl.UnionMap](1)
    reads.computeFlow(writes, empty, schedule, depArr, null, null, null)

    println(depArr(0))

    println("isl works!")
  }
}
