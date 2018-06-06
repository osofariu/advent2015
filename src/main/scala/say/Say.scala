package say

object Say {


  def repeatSay(stringToRead: String, count: Int) : Int  =
    repeatSayIt(stringToRead.toArray, count: Int)

  def repeatSayIt(stringToRead: Array[Char], count: Int) : Int  = {
    if (count == 0)
      stringToRead.length
    else {
      val oneRun = sayIt(stringToRead, Array[Char]())
      println(s"${count} Res: ${oneRun.length}")
      repeatSayIt(oneRun, count - 1)
    }
  }

  def say(stringToRead: String) = {
   sayIt(stringToRead.toArray, Array[Char]())
  }

  def sayIt(stringToRead: Array[Char], stringProcessed: Array[Char]): Array[Char] = {
    if (stringToRead.isEmpty)
      stringProcessed
    else {
      val (sameChars, rest) = firstSameChars(stringToRead)
      val repeats = sameChars.length.toString.toArray ++ Array(sameChars(0))
      sayIt(rest, stringProcessed ++ repeats)
    }
  }

  def firstSameChars(stringToRead: Array[Char]): (Array[Char], Array[Char]) = {
    val sameChars = stringToRead.takeWhile(c ⇒ c == stringToRead(0))
    val restChars = stringToRead.drop(sameChars.length)
    (sameChars, restChars)
  }
}
