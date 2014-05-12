package harald.dsl

object HardwareKnowledge extends ExaKnowledge {

  def initHWFeatures() = {
    if (DomainKnowledge.cores_HW.get > 1)
      DomainKnowledge.use_Openmp = true
    if (DomainKnowledge.nodes_HW.get > 1)
      DomainKnowledge.use_MPI = true
    if (DomainKnowledge.hardware_HW.get.equals("gpu"))
      DomainKnowledge.use_gpu = true
  }
}