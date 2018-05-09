package gifts

import  scala.collection.mutable.Map
import scala.io.Source
import util.Ops

object BetterNiceString {

  def niceFile(filename: String) : Int = {
    val words = Source.fromFile(filename).getLines
    words.map(word ⇒ isNice(word))
      .filter(_ == true)
      .length
  }

  def isNice(str: String) : Boolean =
    hasRepeatingPair(str) && hasSeparatedPair(str)

  def hasRepeatingPair(str: String): Boolean = {
    var occurrenceMap = Map[(Char, Char), Vector[Int]]()
    Ops.makeCharPairs(str)
      .zipWithIndex
      .foreach(buildOccurrenceMap(occurrenceMap))

    occurrenceMap.values
      .find(pairLocations ⇒ {
              atLeastThreeLocations(pairLocations) ||
                twoLocationsNotOverlapping(pairLocations)
            })
      .isDefined
  }

  def hasSeparatedPair(str: String) = {
    (str.length >= 4) && Ops.makeCharPairs(str, 2).find(pair ⇒ pair._1 == pair._2).isDefined
  }

  private def atLeastThreeLocations(locations: Vector[Int]) =
    locations.length >= 3

  private def twoLocationsNotOverlapping(locations: Vector[Int]) =
    locations.length == 2 && (locations(1) - locations(0) > 1)

  private def buildOccurrenceMap(occurrenceMap: Map[(Char, Char),Vector[Int]]) = {
    pairWithIndex : ((Char, Char), Int) ⇒ {
      val (charPair, index) = pairWithIndex
      val knownLocations = occurrenceMap.get(charPair)
      if (knownLocations.isDefined) {
        occurrenceMap.put (charPair, (knownLocations.get :+ index))
      } else {
        occurrenceMap.put(charPair, Vector(index))
      }
    }
  }
}
