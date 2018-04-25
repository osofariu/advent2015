package santa
import scala.io.Source

case class SantaDelivery(moves: String) {
  def finalFloorVisited : Int = {
    if (moves == null)
      throw new UnsupportedOperationException
    else if (moves.length == 0)
      0
    else
      movement.head
  }

  def firstTimeFloorVisited(floor: Int) : Option[Int] = {
    val visitIndex = movement.reverse.indexOf(floor)
    if (visitIndex == -1)
      None
    else
      Some(visitIndex)
  }

  private def movement : Vector[Int] = {
    moves.foldLeft(Vector(0))((acc: Vector[Int], move: Char) =>
        move match {
          case '(' => acc.+:(acc.head + 1)
          case ')' => acc.+:(acc.head - 1)
          case  _  => throw new UnsupportedOperationException()
        })
  }
}

object SantaDelivery {
  def fromFile(filename: String) = {
    val moves = Source.fromFile(filename).mkString
    SantaDelivery(moves)
  }
}
