import exastencils.datastructures._
import exastencils.core._
import exastencils.logger._
import exastencils.smoother._
import exastencils.math._

object DummyRoot {
  def apply(children : Node*) = new DummyRoot(children.toList)
}
case class DummyRoot(var children : List[Node]) extends Node

object MainWuppertal extends App {

  // for profiling
  Console.println("Press enter to continue...")
  Console.in.readLine();

  val v = Vector[Int](1, 2, 3)

  // build up jacobi tree

  {
    import exastencils.matlang._

    val st_L = GeneralStencil[Double](
      (-1, Vector[Int](0, -1)),
      (-1, Vector[Int](-1, 0)),
      (4, Vector[Int](0, 0)),
      (-1, Vector[Int](1, 0)),
      (-1, Vector[Int](0, 1)))

    val f = new Variable(FieldT())
    val u = new Variable(FieldT())
    val r = new Variable(FieldT())
    val L = new Variable(MatrixT())
    val diag = new Function(MatrixT(), "diag")
    val inv = new Function(MatrixT(), "inv")
    val sum = new Function(MatrixT(), "sum")
    val diff = new Function(MatrixT(), "diff")
    val mv = new Function(MatrixT(), "mv")
    val D_inv = new FunctionCall(inv,
      List(new FunctionCall(diag, List(L))))

    StateManager.root_ = DummyRoot(
      new Assignment(L,
        new ConstantStencilExpr(st_L)),
      new Assignment(r,
        new FunctionCall(diff, List(
          f,
          new FunctionCall(mv, List(L, u))))),
      new FunctionCall(sum, List(
        new FunctionCall(mv, List(D_inv, r)))))

  }

  println("\nJacobi Tree:\n" + StateManager.root_ + "\n")

  StateManager.root_ = DummyRoot(new SmootherNode("Gs"))

  val strategy = DefaultStrategy("Smoother")

  strategy += Transformation("Implement", {
    case s : SmootherNode => {
      if (s.name == "Gs") {
        Logger.debug("Smoother name = " + s.name)
        new GsSmootherNode()
      } else {
        throw new Exception("Invalid smoother name: " + s.name)
        s
      }
    }
  })

  strategy.apply()

  println("\nResulting tree:\n" + StateManager.root_)

  // for profiling
  Console.println("Press enter to finish...")
  Console.in.readLine();
}