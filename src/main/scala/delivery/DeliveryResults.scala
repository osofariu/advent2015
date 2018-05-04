package delivery

import scala.collection.immutable.HashMap

case class DeliveryResults(currentPosition: Tuple2[Int, Int], giftTally: HashMap[Tuple2[Int, Int], Int]) {
  def housesVisited = giftTally.keys.seq.size
  def presentsDelivered = giftTally.values.sum

  def makeOneMove(direction: Char) = direction match {
    case '>' => moveOneDirection(pos => (pos._1 + 1, pos._2))
    case '<' => moveOneDirection(pos => (pos._1 - 1, pos._2))
    case '^' => moveOneDirection(pos => (pos._1, pos._2 + 1))
    case 'v' => moveOneDirection(pos => (pos._1, pos._2 - 1))
  }

  def moveOneDirection(directionChange: Tuple2[Int, Int] => Tuple2[Int, Int]) = {
    val newPos = directionChange(currentPosition)
    val giftsAlreadyGiven = giftTally.getOrElse(newPos, 0)
    new DeliveryResults(newPos, giftTally + (newPos -> (giftsAlreadyGiven + 1)))
  }
}

object DeliveryResults {
  val initialTally : (Tuple2[Int, Int], Int) = ((0, 0) -> 1)
  def apply() = new DeliveryResults((0, 0), new HashMap[Tuple2[Int, Int], Int]() + initialTally)
  def combine(a: DeliveryResults, b: DeliveryResults) =
    new DeliveryResults(a.currentPosition, a.giftTally ++ b.giftTally)
}
