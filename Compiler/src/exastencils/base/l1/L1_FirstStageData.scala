package exastencils.base.l1

/** First stage of data-structure received from the main token parser (parsing.L1Parser.scala)
  * Includes the Domain, left and right hand side of the pde
  */
class L1_FirstStageData(val domain : L1_Domain, _lhs : L1_MathTree, _rhs : L1_MathTree) {
  def this(domain : L1_Domain, eq : L1_Equation) = this(domain, eq.left, eq.right)

  val dimCount = domain.dimCount

  val lhs = L1_MathTree.simplify(_lhs)
  val rhs = L1_MathTree.simplify(_rhs)

  override def toString : String = s"FirstStageData:\n\tDomain: $domain\n \tlhs: $lhs\n\trhs: $rhs\n"
}

class L1_Options(val dirs : Seq[Int]) {
  def this(dims : Int, dir : Int) = this(Seq.fill(dims)(dir))

  val dims = dirs.length
  assert(dims > 0)
  assert(dirs forall { x => x == -1 || x == 0 || x == 1 })

  def setSingleDirection(pos : Int, dir : Int) = {
    assert(dir == -1 || dir == 0 || dir == 1)
    new L1_Options(dirs.updated(pos, dir))
  }
}