package exastencils.parallelization.api.cuda

import scala.collection.Iterable

import exastencils.config.Knowledge

// check if buffers need to be synchronized before host/device execution
trait CUDA_PrepareBufferSync {
  def syncBeforeHost(access : String, others : Iterable[String]) = {
    var sync = true
    if (access.startsWith("write") && !Knowledge.cuda_syncHostForWrites)
      sync = false // skip write accesses if demanded
    if (access.startsWith("write") && others.exists(_ == "read" + access.substring("write".length)))
      sync = false // skip write access for read/write accesses
    sync
  }

  def syncAfterHost(access : String, others : Iterable[String]) = {
    access.startsWith("write")
  }

  def syncBeforeDevice(access : String, others : Iterable[String]) = {
    var sync = true
    if (access.startsWith("write") && !Knowledge.cuda_syncDeviceForWrites)
      sync = false // skip write accesses if demanded
    if (access.startsWith("write") && others.exists(_ == "read" + access.substring("write".length)))
      sync = false // skip write access for read/write accesses
    sync
  }

  def syncAfterDevice(access : String, others : Iterable[String]) = {
    access.startsWith("write")
  }
}
