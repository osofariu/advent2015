package wires

import org.scalatest.{path, Matchers}

class InstructionParserTest extends path.FunSpec with Matchers {

  describe("process basic instructions") {
    it("parses straight wire") {
      val instr = "bb -> aa"
      InstructionParser.parse(instr) shouldEqual ("aa", Assign("bb"))
    }
    it("parses value") {
      val instr = "432 -> ab"
      InstructionParser.parse(instr) shouldEqual ("ab", Assign("432"))
    }
    it("parses NOT") {
      val instr = "NOT rr -> ss"
      InstructionParser.parse(instr) shouldEqual ("ss", Not("rr"))
    }
    it("parses AND") {
      val instr = "lf AND lq -> ls"
      InstructionParser.parse(instr) shouldEqual ("ls", And("lf", "lq"))
    }
    it("parses OR") {
      val instr = "ld OR rq -> ws"
      InstructionParser.parse(instr) shouldEqual ("ws", Or("ld", "rq"))
    }
    it("parses XOR") {
      val instr = "vg XOR pr -> qq"
      InstructionParser.parse(instr) shouldEqual ("qq", Xor("vg", "pr"))
    }
    it("parses lShift") {
      val instr = "m LSHIFT 2 -> v"
      InstructionParser.parse(instr) shouldEqual ("v", Lshift("m", "2"))
    }
    it("parses rShift") {
      val instr = "n RSHIFT 12 -> a"
      InstructionParser.parse(instr) shouldEqual ("a", Rshift("n", "12"))
    }
  }
  it("allowes for values in gate instructions") {
    val instr = "1 AND io -> ip"
    InstructionParser.parse(instr) shouldEqual ("ip", And("1", "io"))
  }
}
