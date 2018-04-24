package santa

import scala.io.Source

object SantaDelivery {
  def movement(moves: String) : Vector[Position] = {
    moves.toList.zipWithIndex
      .foldLeft(Vector(Position(0, 0)))((acc: Vector[Position], move: (Char, Int)) =>
        move match {
          case ('(', i) => acc :+ Position(i+1, acc(i).floor + 1)
          case (')', i) => acc :+ Position(i+1, acc(i).floor - 1)
          case (_, i) => throw new UnsupportedOperationException()
        })
  }

  def floor(moves: String) : Int = {
    if (moves == null)
      throw new UnsupportedOperationException

    if (moves.length == 0)
      0
    else
      movement(moves).last.floor
  }

  def movementFromFile(filename: String) : Vector[Position] = {
    val moves = Source.fromFile(filename).mkString
    movement(moves)
  }
}
