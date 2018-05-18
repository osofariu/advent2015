package wires

object InstructionParser {
  val AssignRE = "^([0-9a-z]+) -> ([a-z]+)$".r
  val NotRE = "^NOT ([0-9a-z]+) -> ([a-z]+)".r
  val BinaryRE = "^([0-9a-z]+) ([A-Z]+) ([0-9a-z]+) -> ([a-z]+)$".r
  val ShiftRE = "^([a-z]+) (LSHIFT|RSHIFT) ([0-9]+) -> ([a-z]+)$".r

  def parse(instr: String) : (String, Operation) = instr match {
    case AssignRE(sIn, sOut) ⇒ (sOut, Assign(sIn))
    case NotRE(sIn, sOut) ⇒ (sOut, Not(sIn))
    case ShiftRE(s1, shift, bits, sOut) ⇒ shift match {
      case "LSHIFT" ⇒ (sOut, Lshift(s1, bits))
      case "RSHIFT" ⇒ (sOut, Rshift(s1, bits))
    }
    case BinaryRE(s1, op, s2, sOut) ⇒ op match {
      case "AND" ⇒ (sOut, And(s1, s2))
      case "OR" ⇒ (sOut, Or(s1, s2))
      case "XOR" ⇒ (sOut, Xor(s1, s2))
    }
  }
}
