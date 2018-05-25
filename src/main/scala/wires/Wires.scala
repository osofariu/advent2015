package wires

import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.io.Source

trait Operation
case class Assign(val wire: String) extends Operation
case class Not(val wire: String) extends Operation
case class And(wire1: String, wire2: String) extends Operation
case class Or(wire1: String, wire2: String) extends Operation
case class Xor(wire1: String, wire2: String) extends Operation
case class Rshift(wire: String, bits: String) extends Operation
case class Lshift(wire: String, bits: String) extends Operation

class Circuit {
  private val wireOperations = Map[String, Operation]()
  val signalValues = new SignalValues()
  val wireDependencies = new WireDependencies()

  def fromFile(fileName: String) : List[(String, Int)]= {
    val signalInstructions = Source.fromFile(fileName).getLines.toList
    signalInstructions
      .map(instr ⇒ InstructionParser.parse(instr))
      .foreach(register)
    evaluateCircuit
  }

  def evaluateCircuit = {
    var maxLoopCount = wireOperations.size
    while(signalValues.signalsEvaluated < wireOperations.size && maxLoopCount >= 0 ) {
      wireOperations.keys.foreach(key ⇒ evaluateWire(key))
      maxLoopCount -= 1
    }
    if (maxLoopCount < 0)
      throw new UnexpectedCycleException()
    else {
      returnEvaluatedSignals
    }
  }

  def resetWire(wire: String, value: Int) : Set[String]= {
    signalValues.resetValues(wire, value, wireDependencies.getAllDependencies(wire))
  }

  def evaluateWire(wire: String) = {
    val op = wireOperations.get(wire)
    ensureSignalsExist(wire)
    if (!signalValues.signalIsEvaluated(wire)) {
      op.get match {
        case Assign(signal)  ⇒ applyUnaryOperation(signal, signal ⇒ signal)
        case Not(other)      ⇒ applyUnaryOperation(other, BitwiseOps.not)
        case And(s1, s2)     ⇒ applyBinaryOperation(s1, s2, BitwiseOps.and)
        case Or(s1, s2)      ⇒ applyBinaryOperation(s1, s2,BitwiseOps.or)
        case Xor(s1, s2)     ⇒ applyBinaryOperation(s1, s2, BitwiseOps.xor)
        case Rshift(s1, num) ⇒ applyBinaryOperation(s1, num, BitwiseOps.rshift)
        case Lshift(s1, num) ⇒ applyBinaryOperation(s1, num, BitwiseOps.lshift)
      }
    }

    def applyBinaryOperation(wire1: String, wire2: String, op: (Int, Int) ⇒ Int) = {
      ensureSignalsExist(wire1, wire2)
      if (signalValues.signalIsEvaluated(wire1) && signalValues.signalIsEvaluated(wire2)) {
        val res = op(signalValues.getSignal(wire1), signalValues.getSignal(wire2))
        signalValues.addSignal(wire, res)
        wireDependencies.addDownstreamDepedency(wire, List(wire1, wire2))
      }
    }

    def applyUnaryOperation(other: String, op: Int ⇒ Int) = {
      ensureSignalsExist(other)
      if (signalValues.signalIsEvaluated(other)) {
        val res = op(signalValues.getSignal(other))
        signalValues.addSignal(wire, res)
        wireDependencies.addDownstreamDepedency(wire, List(other))
      }
    }
  }

  def ensureSignalsExist(wires: String*) = {
    wires
      .filter(!Wires.wireIsANumber(_))
      .foreach(
      wire ⇒ {
        if (wireOperations.get(wire).isEmpty)
          throw new UnknownWireException(wire)
      })
  }

  def register(wireOperation: (String, Operation)) = {
    val (wire, operation) = wireOperation
    wireOperations += (wire → operation)
  }

  def returnEvaluatedSignals =
    signalValues.toList
      .toList
      .sorted((p1: (String, Int), p2: (String, Int)) ⇒ {
                if (p1._1 < p2._1) -1
                else if (p1._1 == p2._1) 0
                else 1
              })

  def getSignal(wire: String) =
    signalValues.getSignal(wire)
}

object Wires {
  val numRE = "([0-9]+)".r

  def wireIsANumber(wire: String) = wire match {
    case numRE(value) ⇒ true
    case _ ⇒ false
  }
}
