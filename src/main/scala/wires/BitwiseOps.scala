package wires

object BitwiseOps {

  def not(in: Int) : Int = {
    val in_short = in.toChar
    (~in_short).toChar
  }

  def and(a: Int, b: Int) : Int = {
    val a_short = a.toChar
    val b_short = b.toChar
    (a_short & b_short).toChar
  }

  def or(a: Int, b: Int) : Int = {
    val a_short = a.toChar
    val b_short = b.toChar
    (a_short | b_short).toChar
  }

  def xor(a: Int, b: Int) : Int = {
    val a_short = a.toChar
    val b_short = b.toChar
    (a_short ^ b_short).toChar
  }

  def lshift(a: Int, b: Int) : Int = {
    val a_short = a.toChar
    val b_short = b.toChar
    (a_short<< b_short).toChar
  }

  def rshift(a: Int, b: Int) : Int = {
    val a_short = a.toChar
    val b_short = b.toChar
    (a_short >> b_short).toChar
  }
}
