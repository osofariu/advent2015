package wires

import scala.collection.mutable.Map
import scala.collection.mutable.Set

class SignalValues {
  private val evaluatedSignals = Map[String, Int]()

  def signalsEvaluated = evaluatedSignals.size

  def getSignal(wire: String) = wire match {
    case Wires.numRE(numStr) ⇒ numStr.toInt
    case _ ⇒ getValue(wire)
  }

  def addSignal(wire: String, signalValue: Int) =
    evaluatedSignals(wire) = signalValue

  def removeSignal(wire: String) =
    evaluatedSignals.remove(wire)

  def getValueOpt(wire: String) =
    evaluatedSignals.get(wire)

  def getValue(wire: String) =
    evaluatedSignals(wire)

  def resetValues(wire: String, signal: Int, wireDependencies: Set[String]) = {
    addSignal(wire, signal)
    wireDependencies.map(wire ⇒ removeSignal(wire))
    wireDependencies
  }

  def signalIsEvaluated(wire: String) = {
    if (Wires.wireIsANumber(wire))
      true
    else
      getValueOpt(wire).nonEmpty
  }

  def toList = evaluatedSignals.toList
}
