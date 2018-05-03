package delivery

import scala.io.Source

object GridDelivery {

  def movesFromFile(filename: String) = {
    Source.fromFile(filename).mkString
  }

  def moves(steps: String) : DeliveryResults = {
    makeMoves(steps, DeliveryResults())
  }

  def makeMoves(steps: String, currentResults: DeliveryResults) = {
    steps.foldLeft(currentResults)((results, step) =>
      results.makeOneMove(step, results)
    )
  }

  def twinMoves(steps: String) = {
    def extractMoves(filter: Int => Boolean) =
      steps.zipWithIndex
      .filter(p => filter(p._2))
      .map(_._1)
      .mkString
    val santaMoves = extractMoves(i => i % 2 == 0)
    val robotMoves = extractMoves(i => i % 2 == 1)

    val santaDeliveries = makeMoves(santaMoves, DeliveryResults())
    val robotDeliveries = makeMoves(robotMoves, DeliveryResults())

    DeliveryResults.combine(santaDeliveries, robotDeliveries).housesVisited
  }
}

