package exastencils.base.l1

/**
  * Abstract base-class for a binary tree structure of mathematical expressions,
  * where leaves are either actual values or derivations and inner nodes represent
  * the operations.
  */
sealed abstract class L1_MathTree

case class L1_Addition(left : L1_MathTree, right : L1_MathTree) extends L1_MathTree

case class L1_Subtraction(left : L1_MathTree, right : L1_MathTree) extends L1_MathTree

case class L1_Multiplication(left : L1_MathTree, right : L1_MathTree) extends L1_MathTree

case class L1_Division(left : L1_MathTree, right : L1_MathTree) extends L1_MathTree

case class L1_Exponential(base : L1_MathTree, power : L1_MathTree) extends L1_MathTree

case class L1_Value(t : Double) extends L1_MathTree // leaf

case class L1_FunctionCall(name : String, args : List[L1_MathTree]) extends L1_MathTree

case class L1_Access(name : String) extends L1_MathTree

sealed abstract class L1_Differentiable extends L1_MathTree // leaf
case class L1_Function() extends L1_Differentiable // Represents the 0th derivative
case class L1_Derivation(direction : Int, number : Int) extends L1_Differentiable // Single Derivation
case class L1_NestedDerivation(devs : Seq[L1_Derivation]) extends L1_Differentiable // Nested Derivations
case class L1_Laplace() extends L1_Differentiable // Laplace operator

case class L1_Equation(left : L1_MathTree, right : L1_MathTree); // Root for the PDE

object L1_MathTree {

  /**
    * Tries to evaluate the given MathTree if possible,
    * throws an exception otherwise
    *
    * @param  expr MathTree to be evaluated
    * @throws  RuntimeException         for unknown MathTree subclasses
    * @throws  IllegalArgumentException for subclasses that cannot be evaluated
    * @return The evaluated value as Double
    */
  //  def evaluate(expr : L1_MathTree) : Double = {
  //    expr match {
  //      case L1_Addition(left, right)       => evaluate(left) + evaluate(right)
  //      case L1_Subtraction(left, right)    => evaluate(left) - evaluate(right)
  //      case L1_Multiplication(left, right) => evaluate(left) * evaluate(right)
  //      case L1_Division(left, right)       => evaluate(left) / evaluate(right)
  //      case L1_Exponential(base, power)    => math.pow(evaluate(base), evaluate(power))
  //      case L1_Value(t)                    => t
  //      case _ : L1_Differentiable          => throw new IllegalArgumentException("cant evaluate expression including derivatives")
  //      case unknown                        => throw new RuntimeException("MathExpr.evaluate: unknown expression \"" + unknown + "\"")
  //    }
  //  }

  /**
    * Tries to create a stencil-discretization of the given MathTree if possible,
    * throws an exception otherwise
    *
    * @param  expr MathTree to be discretized
    * @throws  IllegalArgumentException for MathTrees that cannot be discretized
    * @return The resulting Stencil
    */
  def createStencil(expr : L1_MathTree, dims : Int) : L1_MathTree = {
    def createStencil(expr : L1_MathTree) : L1_MathTree = expr match {
      case L1_Addition(left : L1_Value, right)                                                          => throw new IllegalArgumentException("Addition of Stencil and Value not supported")
      case L1_Addition(left, right : L1_Value)                                                          => throw new IllegalArgumentException("Addition of Stencil and Value not supported")
      case L1_Subtraction(left : L1_Value, right)                                                       => throw new IllegalArgumentException("Subtraction of Stencil and Value not supported")
      case L1_Subtraction(left, right : L1_Value)                                                       => throw new IllegalArgumentException("Subtraction of Stencil and Value not supported")
      case L1_Multiplication(left : L1_Value, right : L1_Value)                                         => throw new IllegalArgumentException("Multiplication of Stencils not supported")
      case L1_Division(left : L1_Value, right)                                                          => throw new IllegalArgumentException("Cannot divide Value by Stencil")
      case L1_Division(left, right) if !(right.isInstanceOf[L1_Value])                                  => throw new IllegalArgumentException("Stencil can only be divided by a Value")
      case L1_Division(left, right) if (left.isInstanceOf[L1_Value] || (!right.isInstanceOf[L1_Value])) => throw new IllegalArgumentException("Cannot divide Value by Stencil")
      case L1_Exponential(left, right)                                                                  => throw new IllegalArgumentException("Exponentiation with Stencils not supported")
      case L1_Value(_)                                                                                  => throw new IllegalArgumentException("FD-Discretization of single Values not supported")

      case L1_Addition(left, right)    => L1_Addition(createStencil(left), createStencil(right))
      case L1_Subtraction(left, right) => L1_Subtraction(createStencil(left), createStencil(right))

      case L1_Multiplication(left : L1_Value, right) => L1_Multiplication(createStencil(right), left)
      case L1_Multiplication(left, right : L1_Value) => L1_Multiplication(createStencil(left), right)

      case L1_Multiplication(left, right) => L1_Multiplication(createStencil(left), createStencil(right))

      //case L1_Division(left, right) => createStencil(left) / right.asInstanceOf[L1_Value].t

      //case dev : L1_Differentiable => discretizeDifferentiable(dev, dims)
    }
    assert(dims >= 1)
    createStencil(simplify(expr))
  }

  /**
    * Discretizes a single Differentiable object
    *
    * @param  diffAble Differentiable to be discretized
    * @param  dims     Number of dimensions of the domain
    * @return The Stencil with corresponding number of dimensions
    */
  def discretizeDifferentiable(diffAble : L1_Differentiable, dims : Int) : L1_Stencil = {
    val h = 1 // stepsize
    val e = 1 // order of the truncation error

    def discretizeLaplace(laplace : L1_Laplace) : L1_Stencil = {
      val stencils = for (i <- 1 to dims) yield discretizeDevs(Seq(L1_Derivation(i, 2)))
      stencils.reduce { _ + _ }
    }

    def discretizeDevs(devs : Seq[L1_Derivation]) : L1_Stencil = {
      val stencils = for (dir <- 1 to dims) yield {
        devs.find { _.direction == dir } match {
          case Some(dev) => discretizeSingleDev(dev)
          case None      => new L1_Stencil(Seq(1))
        }
      }
      stencils.reduceLeft { _ tensorProduct _ }
    }

    def discretizeSingleDev(dev : L1_Derivation) : L1_Stencil = {
      val count = dev.number
      val n = if ((count + e) % 2 == 1) count + e
      else count + e - 1
      val taylor = new L1_TaylorApproach(n, count, h, e, 0)
      val weights = taylor.getWeights
      new L1_Stencil(weights map { L1_StencilEntry(_) })
    }

    diffAble match {
      case _ : L1_Function              => discretizeDevs(Seq())
      case dev : L1_Derivation          => discretizeDevs(Seq(dev))
      case nested : L1_NestedDerivation => discretizeDevs(nested.devs)
      case laplace : L1_Laplace         => discretizeLaplace(laplace)
      case unknown                      => throw new RuntimeException("Missing implementation for class '" + unknown.getClass + "'")
    }
  }

  /**
    * simplifies an expression as far as possible
    *
    * @param  expr MathTree to be simplified
    * @throws  RuntimeException for unknown MathTree subclasses
    * @return The simplified MathTree
    */
  def simplify(expr : L1_MathTree) : L1_MathTree = {
    def simplifyOnce(expr : L1_MathTree) : L1_MathTree = {
      expr match {
        case expr @ L1_Addition(left : L1_Value, right : L1_Value)       => L1_Value(left.t + right.t)
        case expr @ L1_Subtraction(left : L1_Value, right : L1_Value)    => L1_Value(left.t - right.t)
        case expr @ L1_Multiplication(left : L1_Value, right : L1_Value) => L1_Value(left.t * right.t)
        case expr @ L1_Division(left : L1_Value, right : L1_Value)       => L1_Value(left.t / right.t)
        case expr @ L1_Exponential(base : L1_Value, power : L1_Value)    => L1_Value(math.pow(base.t, power.t))
        case default                                                     => default
      }
    }
    expr match {
      case t : L1_Value                   => t
      case t : L1_Function                => t
      case dev : L1_Derivation            => dev
      case lap : L1_Laplace               => lap
      case nDev : L1_NestedDerivation     => simplifyNestedDerivative(nDev)
      case L1_Addition(left, right)       => simplifyOnce(L1_Addition(simplify(left), simplify(right)))
      case L1_Subtraction(left, right)    => simplifyOnce(L1_Subtraction(simplify(left), simplify(right)))
      case L1_Multiplication(left, right) => simplifyOnce(L1_Multiplication(simplify(left), simplify(right)))
      case L1_Division(left, right)       => simplifyOnce(L1_Division(simplify(left), simplify(right)))
      case L1_Exponential(base, power)    => simplifyOnce(L1_Exponential(simplify(base), simplify(power)))
      case x : L1_FunctionCall            => x
      case x : L1_Access                  => x
      case unknown                        => throw new RuntimeException(s"MathTree.simplifyOnce: unknown MathTree class ${ unknown.getClass }")
    }
  }

  /**
    * Summarizes and orders nested Derivations with same direction
    *
    * @param  nDev NestedDerivation to be summarized
    * @return Either a 'NestedDerivation' or a single 'Derivation'
    */
  def simplifyNestedDerivative(nDev : L1_NestedDerivation) : L1_Differentiable = {
    assert(nDev.devs.size > 0)
    // group based on directions
    val directionMap = nDev.devs groupBy { _.direction }
    // aggregate number of derivatives in same direction
    val aggregatedMap = directionMap mapValues {
      devSet => devSet.foldLeft(0) { (number, dev) => number + dev.number }
    }
    // convert to Set of Derivations
    val devsUnsorted = aggregatedMap.toSeq.map { case (direction, number) => L1_Derivation(direction, number) }
    // sorts based on direction
    val devs = devsUnsorted.sortWith(_.direction < _.direction)

    if (devs.length == 0)
      throw new Exception("Unforeseen error occured")
    else if (devs.length == 1)
      devs.head
    else
      L1_NestedDerivation(devs)
  }
}

