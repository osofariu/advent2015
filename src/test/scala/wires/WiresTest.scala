package wires

import org.scalatest.{path, Matchers}

class WiresTest extends path.FunSpec with Matchers {

  describe("a basic circuit with no gates") {

    val circuit = new Circuit
    it("evaluates as expected") {
      circuit.register("x", Value(123))
      circuit.evaluate shouldEqual(List(("x", 123)))
    }
  }

  describe("a simple circuit with one gate") {
    val circuit = new Circuit
    circuit.register("x", Value(123))
    circuit.register("y", Value(456))

    it("evaluates a circuit with a single wire") {
      circuit.evaluate shouldEqual(List(("x", 123), ("y", 456)))
    }

    it("NOTs one wire") {
      circuit.register("n", Not("x"))
      circuit.evaluate shouldEqual(List(("n", 65412), ("x", 123), ("y", 456)))
    }

    it("ANDs the two wires") {
      circuit.register("d", And("x", "y"))
      circuit.evaluate shouldEqual(List(("d", 72), ("x", 123), ("y", 456)))
    }

    it("ORs the two wires") {
      circuit.register("e", Or("x", "y"))
      circuit.evaluate shouldEqual(List(("e", 507), ("x", 123), ("y", 456)))
    }

    it("xORs the two wires") {
      circuit.register("f", Xor("x", "y"))
      circuit.evaluate shouldEqual(List(("f", 435), ("x", 123), ("y", 456)))
    }

    it("lShifts the two wires") {
      circuit.register("f", Lshift("x", 2))
      circuit.evaluate shouldEqual(List(("f", 492), ("x", 123), ("y", 456)))
    }

    it("rShifts the two wires") {
      circuit.register("f", Rshift("x", 2))
      circuit.evaluate shouldEqual(List(("f", 30), ("x", 123), ("y", 456)))
    }
  }
}
