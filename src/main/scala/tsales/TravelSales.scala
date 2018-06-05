package tsales

import scala.collection.mutable.Map

class TravelSales {
  val travelDistances = Map[(String, String), Int]()

  def this(filename: String) = {
    this()
    readFile(filename)
  }

  private def readFile(filename: String) = {
    val lines = io.Source.fromFile(filename).getLines
    lines.foreach(line ⇒ parseDistance(line))
  }

  def shortestRoute() = findBestRoute(chooseShorter)

  def longestRoute() = findBestRoute(chooseLonger)

  private def findBestRoute(routeChoice
                                : ((Int, List[String]), (Int, List[String])) ⇒ (Int, List[String])) = {
    Permute.permutations(distinctPlaces)
      .map(route ⇒ (routeDistance(route), route))
      .reduce(routeChoice)
  }

  def routeDistance(places: List[String]) : Int = {
    places
      .sliding(2, 1)
      .map(twoPlaces ⇒ distance(twoPlaces(0), twoPlaces(1)))
      .sum
  }

  private def chooseShorter(rp1: (Int, List[String]), rp2: (Int, List[String])) =
    if (rp1._1 < rp2._1) rp1 else rp2

  private def chooseLonger(rp1: (Int, List[String]), rp2: (Int, List[String])) =
    if (rp1._1 < rp2._1) rp2 else rp1

  def parseDistance(distanceStr: String) = {
    val distanceRe = "^([A-Za-z]+) to ([A-Za-z]+) = ([0-9]+)$".r

    distanceStr match {
      case distanceRe(from, to, distance) ⇒ {
        travelDistances((from, to)) = distance.toInt
        travelDistances((to, from)) = distance.toInt
      }
    }
  }

  def distance(from: String, to: String) : Int = {
    travelDistances((from, to))
  }

    private def distinctPlaces = {
    travelDistances
      .keys
      .map(p ⇒ List(p._1, p._2))
      .reduce((l1, l2) ⇒ l1 ::: l2)
      .toSet
      .toList
  }

}
