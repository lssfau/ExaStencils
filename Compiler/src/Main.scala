import exastencils.datastructures._
import exastencils.core._

object Main {
  def main(args : Array[String]) : Unit = {

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
    //	  System.out.println("For-Loops: " + countForLoops)
    println(Cpp(TreeManager.root))

    val t = new Transformation({ case x : AbstractVariable => Some(AbstractVariable("j", x.Type)) })
    //	  println(t)
    //	  println(t.func)

    //t.apply(TreeManager.root_)
    TreeManager.applyT(t)

    //println(TreeManager.root_)
    println(Cpp(TreeManager.root))

    //val t2 = new Transformation({ case x : AbstractForLoop => x.add(new Annotation("hallo", "ichbineineannotation")) )

    //	  val t : Transformation = new Transformation(match { case forloop : AbstractForLoop
    //	  => return new AbstractForLoop(forloop.Begin, forloop.Inc, forloop.End) } )
  }

  //	def topdown (name : String, s : Strategy) : Strategy = {
  //        lazy val result : Strategy = s <* (all (result))
  //        result
  //    }
  //	
  //	val replaceFromsWithTos =
  //            topdown {
  //                rule {
  //                    case From (x) => To (x)
  //                }
  //            }

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