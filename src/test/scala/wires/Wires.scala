package wires

import  scala.collection.mutable.Map

trait Operation {
  def evaluate(circuit: Circuit): Int
}

case class Straight(signal: String) extends Operation {
  override def evaluate(circuit: Circuit) = ???
}

case class Value(signal: Int) extends Operation {
  override def evaluate(circuit: Circuit) = signal
}

trait Gate extends Operation

case class Not(s: String) extends Gate {
  override def evaluate(circuit: Circuit) = {
    val v = circuit.evaluate(s)
    BitwiseOps.not(v)
  }
}

case class And(s1: String, s2: String) extends Gate {
  override def evaluate(circuit: Circuit) = {
    val v1 = circuit.evaluate(s1)
    val v2 = circuit.evaluate(s2)
    BitwiseOps.and(v1, v2)
  }
}

case class Or(s1: String, s2: String) extends Gate {
  override def evaluate(circuit: Circuit) = {
    val v1 = circuit.evaluate(s1)
    val v2 = circuit.evaluate(s2)
    BitwiseOps.or(v1, v2)
  }
}

case class Xor(s1: String, s2: String) extends Gate {
  override def evaluate(circuit: Circuit) = {
    val v1 = circuit.evaluate(s1)
    val v2 = circuit.evaluate(s2)
    BitwiseOps.xor(v1, v2)
  }
}

case class Rshift(s: String, bits: Int) extends Gate {
  override def evaluate(circuit: Circuit) = {
    val v = circuit.evaluate(s)
    BitwiseOps.rshift(v, bits)
  }
}

case class Lshift(s: String, bits: Int) extends Gate {
  override def evaluate(circuit: Circuit) = {
    val v = circuit.evaluate(s)
    BitwiseOps.lshift(v, bits)
  }
}

class Circuit {
  private val wires = Map[String, Operation]()

  def register(name: String, op: Operation) = {
    wires += (name → op)
  }

  def evaluate(wire: String) = {
    val op = wires(wire)
    op.evaluate(this)
  }

  def evaluate : List[(String, Int)] = {
    wires.keys
      .toList
      .sorted
      .map(key ⇒ {
             (key, wires(key).evaluate(this))
           })
  }

  def fromFile(fileName: String) = ???
}
