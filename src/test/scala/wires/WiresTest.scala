package wires

import org.scalatest.{path, Matchers}

class WiresTest extends path.FunSpec with Matchers {

  describe("a basic circuit with no gates") {

    val c = new Circuit
    it("evaluates as expected") {
      c.register("x", Assign("123"))
      c.evaluateCircuit shouldEqual(List(("x", 123)))
    }
  }

  describe("a simple circuit with one gate") {
    val c = new Circuit
    c.register("x", Assign("123"))
    c.register("y", Assign("456"))

    it("evaluates a circuit with a single wire") {
      c.evaluateCircuit shouldEqual(List(("x", 123), ("y", 456)))
    }

    it("NOTs one wire") {
      c.register("n", Not("x"))
      c.evaluateCircuit shouldEqual(List(("n", 65412), ("x", 123), ("y", 456)))
    }

    it("ANDs the two wires") {
      c.register("dd", And("x", "y"))
      c.evaluateCircuit shouldEqual(List(("dd", 72), ("x", 123), ("y", 456)))
    }

    it("ANDs the two wires where the first is a number") {
      c.register("d", And("123", "y"))
      c.evaluateCircuit shouldEqual(List(("d", 72), ("x", 123), ("y", 456)))
    }

    it("ORs the two wires") {
      c.register("e", Or("x", "y"))
      c.evaluateCircuit shouldEqual(List(("e", 507), ("x", 123), ("y", 456)))
    }

    it("xORs the two wires") {
      c.register("f", Xor("x", "y"))
      c.evaluateCircuit shouldEqual(List(("f", 435), ("x", 123), ("y", 456)))
    }

    it("lShifts the two wires") {
      c.register("f", Lshift("x", "2"))
      c.evaluateCircuit shouldEqual(List(("f", 492), ("x", 123), ("y", 456)))
    }

    it("rShifts the two wires") {
      c.register("f", Rshift("x", "2"))
      c.evaluateCircuit shouldEqual(List(("f", 30), ("x", 123), ("y", 456)))
    }
  }

  describe("a more complicated circuit") {
    val c = new Circuit()
    c.register("a", And("b", "c"))
    c.register("d",Lshift("a", "2"))
    c.register("e", Rshift("b", "2"))
    c.register("b0", Assign("123"))
    c.register("b", Assign("b0"))
    c.register("c", Assign("456"))
    it("evaluates as expected") {
      c.evaluateCircuit shouldEqual List(("a",72), ("b",123), ("b0",123), ("c",456), ("d",288), ("e",30))
    }
  }

  describe("lets have a special exception when evaluating a wire that hasn't been defined") {
    it("throws an UnknownWireException exception when trying to evaluate an undefined wire") {
      val c = new Circuit()
      c.register("a", Assign("b"))
      val caught = intercept[UnknownWireException] {
        c.evaluateCircuit
      }
      caught.getMessage shouldEqual("b")
    }
  }
  describe("process instructions from file") {
    val circuit = new Circuit()
    it("processes every wire signal in file") {
      println("Reading from file")
      val signals = circuit.fromFile("7_input.txt")
      signals
        .find(signalPair ⇒ (signalPair._1 == "a"))
        .head._2 shouldEqual 16076
    }
  }
}
