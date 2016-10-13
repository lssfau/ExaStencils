package exastencils.deprecated.harald.dsl

object Hardware {

  def initHWFeatures() = {
    if (DomainKnowledge.cores_HW.get > 1)
      DomainKnowledge.use_Openmp = true
    else
      DomainKnowledge.use_Openmp = false

    if (DomainKnowledge.nodes_HW.get > 1)
      DomainKnowledge.use_MPI = true
    else
      DomainKnowledge.use_MPI = false

    if (DomainKnowledge.hardware_HW.get.equals("gpu"))
      DomainKnowledge.use_gpu = true
    else
      DomainKnowledge.use_gpu = false
  }
}