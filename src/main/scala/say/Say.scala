package say

object Say {

  def say(stringToRead: String): String = {
    if (stringToRead.isEmpty)
      ""
    else {
      val (sameChars, rest) = firstSameChars(stringToRead)
      sameChars.length + sameChars(0).toString + say(rest)
    }
  }

  def repeatSay(stringToRead: String, count: Int) : Int  = {
    println(s"COUNT: ${count}")
    if (count == 0)
      stringToRead.length
    else {
      val oneRun = say(stringToRead)
      println(s"Res: ${oneRun.length}")
      repeatSay(oneRun, count - 1)
    }
  }

  def firstSameChars(stringToRead: String): (String, String) = {
    val sameChars = stringToRead.takeWhile(c ⇒ c == stringToRead(0))
    val restChars = stringToRead.drop(sameChars.length)
    (sameChars, restChars)
  }
}
