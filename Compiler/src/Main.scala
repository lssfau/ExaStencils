import exastencils.datastructures._
import exastencils.core._
import exastencils.datastructures._
import exastencils.datastructures.l4._

object Main {
  object CountingStrategy extends Strategy {
    override def apply : Boolean = {
      var constcount = 0
      var forcount = 0

      TreeManager.apply(new Transformation({ case x : AbstractConstantExpression => constcount += 1; WARN(x); Some(x) }))
      TreeManager.apply(new Transformation({ case a : AbstractForLoop => forcount += 1; Some(a) }))

      WARN(f"Counted $constcount consts and $forcount fors")

      true
    }
  }

  def main(args : Array[String]) : Unit = {
    
//    val newt = Duplicate(TreeManager.root)
//    // annotations copied?
//    println("original annotations")
//    TreeManager.root.getAnnotations.foreach(a => println(a.name + " <-> " + a.value))
//    println("cloned annotations")
//    newt.getAnnotations.foreach(a => println(a.name + " <-> " + a.value))

    var absvarStrategy = new Strategy
    absvarStrategy += new Transformation({ case x : AbstractVariable => Some(AbstractVariable("j", x.Type)) })
    absvarStrategy.apply

    DBG(TreeManager.root)
//    println(newt)

    TreeManager.apply(CountingStrategy)
  }
}