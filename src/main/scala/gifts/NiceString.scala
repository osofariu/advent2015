package gifts

import scala.io.Source
import util.Ops

object NiceString {

  def isNice(str: String) : Boolean = {
    hasEnoughVowels(str) &&
    hasTwoConsecutiveLetters(str) &&
    doesNotContainForbiddenStrings(str)
  }

  def niceFile(filename: String) : Int = {
    val words = Source.fromFile(filename).getLines
    words.map(word ⇒ isNice(word))
      .filter(_ == true)
      .length
  }

  def hasEnoughVowels(str: String) =
    countVowels(str) >= 3

  def hasTwoConsecutiveLetters(str: String) = {
    Ops.makeCharPairs(str)
      .find(p ⇒ p._1 == p._2).isDefined
  }

  def doesNotContainForbiddenStrings(str: String) = {
    Seq("ab", "cd", "pq", "xy")
    .forall(seq ⇒ ! str.contains(seq))
  }

  def countVowels(str: String) = {
    str.collect{case 'a' | 'e' | 'i' | 'o' | 'u' ⇒ 1}.sum
  }
}
