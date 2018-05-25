package wires

import scala.collection.mutable.Map
import scala.collection.mutable.Set

class WireDependencies {
  private val dependencies = Map[String, Set[String]]()

  def getImmediateDependencies(wire: String) = {
    dependencies.get(wire)
  }

  def getAllDependencies(wire: String) : Set[String] = {
    val visited = Set[String]()
    def helper(wire: String) : Set[String]= {
      val deps = getImmediateDependencies(wire)
      if (deps.isEmpty)
        Set()
      else {
        deps.get ++ deps.get.flatMap(visitWire)
      }
    }

    def visitWire(wireToVisit: String) = {
      if (!visited.contains(wireToVisit)) {
        visited += wireToVisit
        helper(wireToVisit)
      } else {
        Set[String]()
      }
    }
    helper(wire)
  }

  def addDownstreamDepedency(wire: String, upstreamWires: List[String]) = {
    upstreamWires
      .filter(! Wires.wireIsANumber(_))
      .foreach(
        downstreamWire ⇒ {
          val deps = dependencies.get(downstreamWire)
          if (deps.isEmpty)
            dependencies(downstreamWire) = Set(wire)
          else
            dependencies(downstreamWire) = dependencies(downstreamWire) + wire
        })
  }
}
