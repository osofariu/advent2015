package wires

import org.scalatest.{path, Matchers}

class WiresTest extends path.FunSpec with Matchers {

  describe("a basic circuit with no gates") {

    val c = new Circuit
    it("evaluates the one wire provided") {
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

  describe("wire dependencies are maintained correctly") {
    val c = new Circuit()
    c.register("a", And("b", "e"))
    c.register("d",Lshift("e", "2"))
    c.register("e", Rshift("b", "2"))
    c.register("f", Or("a", "b"))
    c.register("b0", Assign("123"))
    c.register("b", Assign("b0"))
    c.register("c", Assign("456"))
    it("evaluates then resets wire b0") {
      c.evaluateCircuit
      c.resetWire("b", 124) shouldEqual Set("f", "a", "e", "d")
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
      val signals = circuit.fromFile("input/7_input.txt")
      circuit.getSignal("a") shouldEqual 16076
    }

    it("re-evaluates a after re-setting b to a") {
      val signals = circuit.fromFile("input/7_input.txt")
      val wireASignal = circuit.getSignal("a")
      circuit.resetWire("b", wireASignal)
      circuit.evaluateCircuit
      circuit.getSignal("a") shouldEqual(2797)
    }
  }

  describe("detect cycles") {
    it("won't evaluate a circuit with mutually-dependent wires") {
      val circuit = new Circuit()
      circuit.register("a", Not("b"))
      circuit.register("b", Not("a"))
      intercept[UnexpectedCycleException] {
        circuit.evaluateCircuit
      }
    }
  }
}
