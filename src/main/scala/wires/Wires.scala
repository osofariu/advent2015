package wires

import scala.collection.mutable.Map
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
  private val wireSignals = Map[String, Int]()

  def fromFile(fileName: String) : List[(String, Int)]= {
    val signalInstructions = Source.fromFile(fileName).getLines.toList
    signalInstructions
      .map(instr ⇒ InstructionParser.parse(instr))
      .foreach(wireOperation ⇒ register(wireOperation._1, wireOperation._2))
    evaluateCircuit
  }

  def evaluateCircuit = {
    var loopCount = 0
    while(wireSignals.size < wireOperations.size && loopCount <= wireOperations.size ) {
      wireOperations.keys.toList.foreach(key ⇒ evaluateWire(key))
      loopCount += 1
    }
    sortByWireName(wireSignals.toList)
  }

  def evaluateWire(wire: String) = {
    val op = wireOperations.get(wire)
    if (!signalIsEvaluated(wire)) {
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
      if (signalIsEvaluated(wire1) && signalIsEvaluated(wire2)) {
        val res = op(getSignal(wire1), getSignal(wire2))
        saveSignal(wire, res)
      }
    }
    def applyUnaryOperation(other: String, op: Int ⇒ Int) = {
      if (signalIsEvaluated(other)) {
        val res = op(getSignal(other))
        saveSignal(wire, res)
      }
    }
  }

  def wireIsKnown(wire: String) = wireOperations.get(wire).nonEmpty

  val numRE = "([0-9]+)".r

  def wireIsANumber(wire: String) = wire match {
    case numRE(value) ⇒ true
    case _ ⇒ false
  }

  def getSignal(wire: String) = wire match {
    case numRE(numStr) ⇒ numStr.toInt
    case _ ⇒ wireSignals(wire)
  }

  def signalIsEvaluated(wire: String) = {
    if (wireIsANumber(wire))
      true
    else if(wireIsKnown(wire))
      wireSignals.get(wire).nonEmpty
    else
      throw new UnknownWireException(wire)
  }

  def saveSignal(wire: String, wireSignal: Int) =
    wireSignals(wire) = wireSignal

  def register(wire: String, operation: Operation) =
    wireOperations += (wire → operation)

  def sortByWireName(l: List[(String, Int)]) = {
    l.sorted((p1: (String, Int), p2: (String, Int)) ⇒ {
              if (p1._1 < p2._1) -1
              else if (p1._1 == p2._1) 0
              else 1})
  }
}
