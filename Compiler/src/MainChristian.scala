import exastencils.datastructures._
import exastencils.core._
import exastencils.datastructures._
import exastencils.datastructures.l4._
import exastencils.parsers.l4
import exastencils.parsers.l4.ParserL4
import exastencils.primitives._
import exastencils.datastructures.ir.BooleanLiteral

object MainChristian {
  //  object CountingStrategy extends Strategy("Counting") {
  //    override def apply : Option[StrategyResult] = {
  //      var constcount = 0
  //      var forcount = 0
  //
  ////      StateManager.apply(new Transformation({ case x : Constant => constcount += 1; WARN(x); Some(x) }))
  ////      StateManager.apply(new Transformation({ case a : ForStatement => forcount += 1; Some(a) }))
  //
  //      WARN(f"Counted $constcount consts and $forcount fors")
  //
  //      null
  //    }
  //  }

  def main(args : Array[String]) : Unit = {
    //    val newt = Duplicate(TreeManager.root)
    //    // annotations copied?
    //    println("original annotations")
    //    TreeManager.root.getAnnotations.foreach(a => println(a.name + " <-> " + a.value))
    //    println("cloned annotations")
    //    newt.getAnnotations.foreach(a => println(a.name + " <-> " + a.value))

    //    DBG("previous:")
    //    DBG(StateManager.root)
    //    
    //    var replacingStrategy = new Strategy
    //    replacingStrategy += new Transformation({case x : ConstantExpression if(x.Value == 1) => Some(ConstantExpression(3)) })
    //    replacingStrategy += new Transformation({case x : ConstantExpression if(x.Value.isInstanceOf[Int]) => Some(ConstantExpression(x.Value.asInstanceOf[Int] * 10)) })
    //    replacingStrategy.apply
    //    var absvarStrategy = new Strategy
    //    absvarStrategy += new Transformation({ case x : Variable => Some(AbstractVariable("j", x.Type)) })
    //    absvarStrategy.apply
    //
    //    DBG("===========================")
    //    DBG("after:")
    //    DBG(StateManager.root)
    //
    //    StateManager.apply(CountingStrategy)

    //    val p4 = new ParserL4
    //    val result = p4.parseFile("/scratch-local/schmittch/ExaStencils/ScalaExaStencil/Compiler/examples/level4_simple.exa")
    //    
    //    StateManager.root_ = result
    //
    //    
    //    println(StateManager.root_)
    //    var replacingStrategy = new Strategy("replacing")
    //    replacingStrategy += new Transformation("replace", { case x : BinaryExpression => println(x); Some(BinaryExpression("XXX", Constant(0), Constant(0))) }, false)
    //    replacingStrategy.apply
    //    println(StateManager.root_)
    //    
    //    var positionStrategy = new Strategy("PositionStrategy")
    //    positionStrategy += new Transformation({case x : Annotatable => x.getAnnotations.foreach(a => println(x + " " + a.name + " - " + a.value)); Some(x) })
    //    //positionStrategy.apply
    
    
    case class MapNode(var x: scala.collection.mutable.Map[String, Node]) extends Node {
      override def duplicate = this.copy().asInstanceOf[this.type]
    }
    
    StateManager.root_ = MapNode(scala.collection.mutable.Map(("bla", BooleanLiteral(true)), ("bla2", StringLiteral("bla2-stringlit")), ("bla3", BooleanLiteral(true))))
    println(StateManager.root_)
    println("eq: " + (StateManager.root_ eq StateManager.root_))
    //val duplicated = Duplication.duplicate(StateManager.root_)
    import com.rits.cloning.Cloner
    val cloner = new Cloner
    val duplicated = cloner.deepClone(StateManager.root_)
    println("eq0: " + (StateManager.root_ eq duplicated))
    println("eq1: " + (StateManager.root_.asInstanceOf[MapNode].x("bla") eq duplicated.asInstanceOf[MapNode].x("bla")))
//    
//    val mapnodestrat = Strategy("mapnodestrat")
//    mapnodestrat += Transformation("s1", { case x: BooleanLiteral => println(x); BooleanLiteral(false) })
//    mapnodestrat.apply
//    println(StateManager.root_)
//        
//    sys.exit
//
//    import exastencils.datastructures.Transformation._
//    val x = new Output(new exastencils.datastructures.ir.BooleanLiteral(true))
//    val y = new Output(List(exastencils.datastructures.ir.BooleanLiteral(true)))
//    println(y)
//    
//    y.inner match {
//      case x : Node => println("node")
//      case x : List[_] => println("list")
//      case _ => println("anderes")
//    }
//    
//    sys.exit
//    
//    val s = new exastencils.parsers.settings.ParserSettings
//    s.parseFile(args(0))
//    
//    val k = new exastencils.parsers.settings.ParserKnowledge
//    k.parseFile(args(1))
//
//
//    import exastencils.prettyprinting.PrettyprintingManager
//    val p = PrettyprintingManager.getPrinter("hallo")
//    p << "blabla"
//    PrettyprintingManager.finish
//    
//    
//    import exastencils.datastructures.ir._
//    val a = FunctionCallExpression(null, null)
//    val b = BinaryExpression("x", null, null)
//    val c = Constant("hallo")
//    val d = a ~ b ~ c
//    
//    //val x = new Node with Debuggable
//
//    println(d)
  }
}
