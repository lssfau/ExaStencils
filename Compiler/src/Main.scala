import exastencils.datastructures._
import exastencils.core._
import exastencils.datastructures._
import exastencils.datastructures.l4._

object Main {
  object CountingStrategy extends Strategy {
    override def apply : Boolean = {
      var constcount = 0
      var forcount = 0

      StateManager.apply(new Transformation({ case x : AbstractConstantExpression => constcount += 1; WARN(x); Some(x) }))
      StateManager.apply(new Transformation({ case a : AbstractForLoop => forcount += 1; Some(a) }))

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

    DBG("previous:")
    DBG(StateManager.root)
    
    var replacingStrategy = new Strategy
    replacingStrategy += new Transformation({case x : AbstractConstantExpression if(x.Value == 1) => Some(AbstractConstantExpression(3)) })
    replacingStrategy += new Transformation({case x : AbstractConstantExpression if(x.Value.isInstanceOf[Int]) => Some(AbstractConstantExpression(x.Value.asInstanceOf[Int] * 10)) })
    replacingStrategy.apply
    var absvarStrategy = new Strategy
    absvarStrategy += new Transformation({ case x : AbstractVariable => Some(AbstractVariable("j", x.Type)) })
    absvarStrategy.apply

    DBG("===========================")
    DBG("after:")
    DBG(StateManager.root)

    StateManager.apply(CountingStrategy)
  }
}