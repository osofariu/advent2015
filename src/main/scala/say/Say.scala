package say

object Say {

  def repeatSay(charsToRead: String, count: Int) : Vector[Char]  =
    repeatSayIt(charsToRead.toVector, count: Int)

  def repeatSayIt(charsToRead: Vector[Char], count: Int) : Vector[Char]  = {
    if (count == 0)
      charsToRead
    else {
      val sayResult = sayIt(charsToRead, Vector[Char]())
      repeatSayIt(sayResult, count - 1)
    }
  }

  def say(charsToRead: String) = {
   sayIt(charsToRead.toVector, Vector[Char]())
  }

  def sayIt(charsToRead: Vector[Char], charsProcessed: Vector[Char]): Vector[Char] = {
    if (charsToRead.isEmpty)
      charsProcessed
    else {
      val (sameChars, rest) = firstSameChars(charsToRead)
      val repeats = sameChars.length.toString.toVector ++ Vector(sameChars(0))
      sayIt(rest, charsProcessed ++ repeats)
    }
  }

  def firstSameChars(charsToRead: Vector[Char]): (Vector[Char], Vector[Char]) = {
    val sameChars = charsToRead.takeWhile(c ⇒ c == charsToRead(0))
    val restChars = charsToRead.drop(sameChars.length)
    (sameChars, restChars)
  }
}
