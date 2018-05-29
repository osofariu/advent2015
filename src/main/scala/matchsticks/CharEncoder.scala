package matchsticks

trait EncodedChar {
  def space: SpaceStats
}

case object DoubleQuoteChar extends EncodedChar {
  def space = SpaceStats(code = 1, memory = 0)
}

case class PlainChar(x: String) extends EncodedChar {
  def space = SpaceStats(code = 1, memory = 1)
}

case class EscapedChar(x: String) extends EncodedChar {
  def space = SpaceStats(code = 2, memory = 1)
}

case class HexChar(x: String) extends EncodedChar {
  def space = SpaceStats(code = 4, memory = 1)
}

object CharEncoder {
  val doubleQuoteCharRe = "\"(.*)".r
  val plainCharRe = "([a-z0-9])(.*)".r
  val escapedCharRe = """[\\](["\\])(.*)""".r
  val hexCharRe = """\\x([0-9a-f][0-9a-f])(.*)""".r

  def encode(str: String) : List[EncodedChar] = {
    if (str.length == 0)
      List[EncodedChar]()
    else {
      str match {
        case hexCharRe(char, rest)     ⇒ HexChar(char)     +: encode(rest)
        case escapedCharRe(char, rest) ⇒ EscapedChar(char) +: encode(rest)
        case plainCharRe(char, rest)   ⇒ PlainChar(char)   +: encode(rest)
        case doubleQuoteCharRe(rest)   ⇒ DoubleQuoteChar   +: encode(rest)
      }
    }
  }

  def reencode(str: String): String =
    "\"" +
      str.foldLeft("")((acc, char) ⇒ char match {
                         case '"' ⇒ acc + "\\\""
                         case '\\'⇒ acc + "\\\\"
                         case c   ⇒ acc + c
                       }) +
      "\""
}
