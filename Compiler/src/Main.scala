import exastencils.datastructures._
import exastencils.core._
import exastencils.datastructures._
import exastencils.datastructures.l4._

object Main {
  def main(args : Array[String]) : Unit = {

    //    var aa1 = new Annotation("a1", "hallo welt")
    //    var aa2 = aa1.deepClone.asInstanceOf[Annotation]
    //    aa1.setValue("geändert")
    //    
    //    println(aa1.value)
    //    println(aa2.value)
    //    exit(0)

    //	  val a1 : PartialFunction[Node, Unit] = { case absvar : AbstractVariable => System.out.println("found abs var " + absvar.Name) case _ => }
    //	  TreeManager.apply( a1 )

    // rename "i" inside a for-loop to "j"
    //	  val a2 : PartialFunction[Node, Unit] = { 
    //	    case forloop : AbstractForLoop => forloop.Begin.Variable = forloop.Begin.Variable match {
    //	        case absvar : AbstractVariable if(absvar.Name == "i") => AbstractVariable("j", absvar.Type)
    //	    }
    //	    case _ =>
    //	  }
    //	  TreeManager.apply( a2 )

    //	  TreeManager.register(new StackCollector)
    //	  TreeManager.apply(a1)
    System.out.println("For-Loops: " + countForLoops)
    
    var constcount = 0
    TreeManager.apply(new Transformation({ case x : AbstractConstantExpression => constcount = constcount+1; println(x); Some(x) }))
    System.out.println("Constants: " + constcount)
    

    val newt = TreeManager.root.deepClone

    val t = new Transformation({ case x : AbstractVariable => Some(AbstractVariable("j", x.Type)) })
    //println(t)
    

    //TreeManager.apply(t)
    
    println(TreeManager.root)
    println(newt)

    //    val newRoot = TreeManager.root.copy(_)

    //println(TreeManager.root_)
    //    println(Cpp(TreeManager.root))

    //val t2 = new Transformation({ case x : AbstractForLoop => x.add(new Annotation("hallo", "ichbineineannotation")) )

    //	  val t : Transformation = new Transformation(match { case forloop : AbstractForLoop
    //	  => return new AbstractForLoop(forloop.Begin, forloop.Inc, forloop.End) } )
  }

  def countForLoops() : Int = {
    var c = 0
    val forCounter : PartialFunction[Node, Unit] = {
      case a : AbstractForLoop => c += 1
      case _                   =>
    }
    TreeManager.apply(forCounter)
    return c
  }
}