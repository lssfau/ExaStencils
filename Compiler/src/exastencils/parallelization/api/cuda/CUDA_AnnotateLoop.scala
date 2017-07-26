package exastencils.parallelization.api.cuda

import scala.collection._
import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.optimization.ir._
import exastencils.util.ir.IR_FctNameCollector

/// CUDA_AnnotateLoop

/**
  * This transformation is used to calculate the annotations for CUDA loops.
  */
object CUDA_AnnotateLoop extends DefaultStrategy("Calculate the annotations for CUDA loops") {
  val collector = new IR_FctNameCollector
  this.register(collector)

  /**
    * Annotate the inner CUDA loops.
    *
    * @param loop the loop to traverse
    */
  def annotateInnerCudaLoops(loop : IR_ForLoop) : Unit = {
    loop.body.foreach {
      case x : IR_ForLoop =>
        x.annotate(CUDA_Util.CUDA_LOOP_ANNOTATION, CUDA_Util.CUDA_INNER)
        annotateInnerCudaLoops(x)
      case _              =>
    }
  }

  /**
    * Calculate the CUDA loops that are part of the band.
    *
    * @param extremaMap the map with the extrema for the loop iterators
    * @param loop       the current loop to traverse
    */
  def calculateLoopsInBand(extremaMap : mutable.HashMap[String, (Long, Long)], loop : IR_ForLoop) : Unit = {
    loop.body.head match {
      case innerLoop : IR_ForLoop if loop.body.size == 1 &&
        CUDA_Util.verifyCudaLoopSuitability(innerLoop) && CUDA_Util.verifyCudaLoopParallel(innerLoop) =>
        try {
          val (loopVariables, lowerBounds, upperBounds, _) = CUDA_Util.extractRelevantLoopInformation(ListBuffer(innerLoop))
          var loopDependsOnSurroundingIterators = false
          CUDA_GatherLoopIteratorUsage.loopIterators = extremaMap.keySet
          CUDA_GatherLoopIteratorUsage.usedLoopIterators.clear()
          CUDA_GatherLoopIteratorUsage.applyStandalone(IR_Scope(IR_ExpressionStatement(lowerBounds.head)))
          loopDependsOnSurroundingIterators |= CUDA_GatherLoopIteratorUsage.usedLoopIterators.nonEmpty
          CUDA_GatherLoopIteratorUsage.usedLoopIterators.clear()
          CUDA_GatherLoopIteratorUsage.applyStandalone(IR_Scope(IR_ExpressionStatement(upperBounds.head)))
          loopDependsOnSurroundingIterators |= CUDA_GatherLoopIteratorUsage.usedLoopIterators.nonEmpty

          extremaMap.put(loopVariables.head, (IR_SimplifyExpression.evalIntegralExtrema(lowerBounds.head, extremaMap)._1, IR_SimplifyExpression.evalIntegralExtrema(upperBounds.head, extremaMap)._2))
          innerLoop.annotate(IR_SimplifyExpression.EXTREMA_MAP, extremaMap)

          if (loopDependsOnSurroundingIterators) {
            innerLoop.annotate(CUDA_Util.CUDA_LOOP_ANNOTATION, CUDA_Util.CUDA_INNER)
            annotateInnerCudaLoops(innerLoop)
          } else {
            innerLoop.annotate(CUDA_Util.CUDA_LOOP_ANNOTATION, CUDA_Util.CUDA_BAND_PART)
            calculateLoopsInBand(extremaMap, innerLoop)
          }
        } catch {
          case e : EvaluationException =>
            Logger.warning(s"""Error annotating the inner loops! Failed to calculate bounds extrema: '${ e.msg }'""")
            innerLoop.annotate(CUDA_Util.CUDA_LOOP_ANNOTATION, CUDA_Util.CUDA_INNER)
            annotateInnerCudaLoops(innerLoop)
        }
      case _                                                                                          =>
        loop.body.foreach {
          case x : IR_ForLoop =>
            annotateInnerCudaLoops(x)
          case _              =>
        }
    }
  }

  /**
    * Calculate extrema values for the loop iterators and annotate loop with adequate CUDA loop annotation.
    *
    * @param extremaMap the map containing the extrema values for the loop iterators
    * @param loop       the loop to traverse
    */
  def updateLoopAnnotations(extremaMap : mutable.HashMap[String, (Long, Long)], loop : IR_ForLoop, bodyDecl : ListBuffer[IR_Statement] = ListBuffer[IR_Statement]()) : Unit = {
    if (CUDA_Util.verifyCudaLoopSuitability(loop)) {
      try {
        val (loopVariables, lowerBounds, upperBounds, _) = CUDA_Util.extractRelevantLoopInformation(ListBuffer(loop))
        extremaMap.put(loopVariables.head, (IR_SimplifyExpression.evalIntegralExtrema(lowerBounds.head, extremaMap)._1, IR_SimplifyExpression.evalIntegralExtrema(upperBounds.head, extremaMap)._2))
        loop.annotate(IR_SimplifyExpression.EXTREMA_MAP, extremaMap)

        if (CUDA_Util.verifyCudaLoopParallel(loop)) {
          loop.annotate(CUDA_Util.CUDA_LOOP_ANNOTATION, CUDA_Util.CUDA_BAND_START)
          loop.annotate(CUDA_Util.CUDA_BODY_DECL, bodyDecl)

          calculateLoopsInBand(extremaMap, loop)
        } else {
          val innerLoops : ListBuffer[IR_ForLoop] = loop.body.filter(x => x.isInstanceOf[IR_ForLoop]).asInstanceOf[ListBuffer[IR_ForLoop]]

          // if there is no more inner loop and a band start is not found, add the body declarations to this loop
          if (innerLoops.nonEmpty) {
            innerLoops.foreach(updateLoopAnnotations(extremaMap, _))
          } else {
            loop.annotate(CUDA_Util.CUDA_BODY_DECL, bodyDecl)
          }

        }
      } catch {
        case e : EvaluationException =>
          Logger.warning(s"""Error while searching for band start! Failed to calculate bounds extrema: '${ e.msg }'""")
      }
    }
  }

  this += new Transformation("Processing ForLoopStatement nodes", {
    case loop : IR_ForLoop if loop.hasAnnotation(CUDA_Util.CUDA_LOOP_ANNOTATION) =>
      loop.removeAnnotation(CUDA_Util.CUDA_LOOP_ANNOTATION)
      updateLoopAnnotations(mutable.HashMap[String, (Long, Long)](), loop)
      loop
    case scope : IR_Scope if scope.hasAnnotation(CUDA_Util.CUDA_LOOP_ANNOTATION) =>
      scope.removeAnnotation(CUDA_Util.CUDA_LOOP_ANNOTATION)

      val varDeclarations = scope.body.takeWhile(x => x.isInstanceOf[IR_VariableDeclaration])
      val remainingBody = scope.body.dropWhile(x => x.isInstanceOf[IR_VariableDeclaration])

      remainingBody match {
        case ListBuffer(c : IR_Comment, loop : IR_ForLoop) =>
          updateLoopAnnotations(mutable.HashMap[String, (Long, Long)](), loop, varDeclarations)
          IR_Scope(c, loop)
        case _                                             =>
          scope
      }
  }, false)

  /// CUDA_GatherLoopIteratorUsage
  object CUDA_GatherLoopIteratorUsage extends QuietDefaultStrategy("Gather surrounding loop iterator accesses") {
    var loopIterators : Set[String] = Set[String]()
    var usedLoopIterators = ListBuffer[String]()

    this += new Transformation("Searching", {
      case access @ IR_VariableAccess(name : String, _) if loopIterators.contains(name) =>
        usedLoopIterators += name
        access
    }, false)
  }

}
