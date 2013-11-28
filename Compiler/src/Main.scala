import exastencils.datastructures._
import exastencils.core._
import exastencils.datastructures._
import exastencils.datastructures.l4._

object Main {
  def main(args : Array[String]) : Unit = {

    // FIXME build unit tests
    TreeManager.root.add(new Annotation("ich bin ein test", "hallo"))
    TreeManager.root.add(new Annotation("und noch einer", "blabla"))
    
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
    
    val newt = Duplicate(TreeManager.root)
    // annotations copied?
    println("original annotations")
    TreeManager.root.getAnnotations.foreach(a => println(a.name + " <-> " + a.value))
    println("cloned annotations")
    newt.getAnnotations.foreach(a => println(a.name + " <-> " + a.value))
    
    println("For-Loops: " + countForLoops)
    printConsts

    // Replacing all Variable names with "j"
    
    val t = new Transformation({ case x : AbstractVariable => Some(AbstractVariable("j", x.Type)) })
    TreeManager.apply(t)
    
    println(TreeManager.root)
    println(newt)

    
    
    
    //    val newRoot = TreeManager.root.copy(_)

    //println(TreeManager.root_)
    //    println(Cpp(TreeManager.root))

    //val t2 = new Transformation({ case x : AbstractForLoop => x.add(new Annotation("hallo", "ichbineineannotation")) )

    //	  val t : Transformation = new Transformation(match { case forloop : AbstractForLoop
    //	  => return new AbstractForLoop(forloop.Begin, forloop.Inc, forloop.End) } )
  }

  def printConsts = {
    var constcount = 0
    TreeManager.apply(new Transformation({ case x : AbstractConstantExpression => constcount = constcount + 1; println(x); Some(x) }))
    System.out.println("Constants: " + constcount)
  }
  
  def countForLoops() : Int = {
    var c = 0
    val forCounter = new Transformation({
      case a : AbstractForLoop => c += 1; Some(a)
    })
    TreeManager.apply(forCounter)
    return c
  }
}