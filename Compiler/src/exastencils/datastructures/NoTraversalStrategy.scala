package exastencils.datastructures

import exastencils.core.TransformationException
import exastencils.logger.Logger

abstract class NoTraversalStrategy(name : String) extends Strategy(name) {
  def apply() = {
    this.onBefore()
    this.transaction()
    Logger.info(s"""Applying strategy "${ name }"""")
    try {
      doWork()
      this.commit()
    } catch {
      case x : TransformationException =>
        Logger.warn(s"""Strategy "${ name }" did not apply successfully""")
        Logger.warn(s"""Error in Transformation ${ x.transformation.name }""")
        Logger.warn(s"Message: ${ x.msg }")
        Logger.warn(s"Rollback will be performed")
        this.abort()
    }
    this.onAfter()
  }

  def doWork()
}
