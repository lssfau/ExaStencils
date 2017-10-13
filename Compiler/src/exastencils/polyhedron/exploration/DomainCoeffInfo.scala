package exastencils.polyhedron.exploration

import exastencils.polyhedron.Isl.TypeAliases._

object DomainCoeffInfo {
  def apply(domain : isl.UnionSet) : DomainCoeffInfo = {

    val ctx : isl.Ctx = domain.getCtx()
    var count : Int = 0
    var i : Int = 0
    val nrStmts : Int = domain.nSet()
    val domainSets = new Array[isl.Set](nrStmts)
    domain.foreachSet { set : isl.Set =>
      domainSets(i) = set
      count += set.dim(T_SET)
      i += 1
    }
    java.util.Arrays.sort(domainSets, new java.util.Comparator[isl.Set]() {
      override def compare(x : isl.Set, y : isl.Set) : Int = x.getTupleName().compareTo(y.getTupleName())
    })

    var stmtInfo : Map[String, StmtCoeffInfo] = Map.empty

    val nrIt : Int = count
    count = 0
    val domainParDim : Int = domain.params().dim(T_PAR)
    for ((set, i) <- domainSets.view.zipWithIndex) {
      val stmtNrIt : Int = set.dim(T_SET)
      stmtInfo += (set.getTupleName() ->
        new StmtCoeffInfo(count, stmtNrIt,
          nrIt + domainParDim * i,
          nrIt + domainParDim * nrStmts + i))
      count += stmtNrIt
    }

    val dim : Int = nrIt + nrStmts * (domainParDim + 1)
    val universe = isl.Set.universe(isl.Space.setAlloc(ctx, 0, dim))

    return new DomainCoeffInfo(nrIt, domainParDim, stmtInfo, universe, domain.getSpace().params())
  }
}

class DomainCoeffInfo private (val nrIt : Int, val nrParPS : Int, val stmtInfo : Map[String, StmtCoeffInfo],
    val universe : isl.Set, val scheduleParamSpace : isl.Space) {

  val ctx : isl.Ctx = universe.getCtx()
  val nrStmts : Int = stmtInfo.size
  val dim : Int = nrIt + nrParPS * nrStmts + nrStmts
}

case class StmtCoeffInfo(val itStart : Int, val nrIt : Int, val parStart : Int, val cstIdx : Int)
