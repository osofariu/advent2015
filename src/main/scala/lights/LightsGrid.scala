package lights


case class LightsGrid(dimension: Int) {
  private val grid = Array.ofDim[Boolean](dimension, dimension)

  type Coord = (Int, Int)
  type Range = (Coord, Coord)

  def iter(c: Range): Seq[Coord] = {
    val ((x1, y1), (x2, y2)) = c
    for {
      x ← x1 to x2
      y ← y1 to y2
    } yield (x, y)
  }

  def act (rangeLower: Coord, rangeHigher: Coord) = {
    (f: (Coord) ⇒ Unit) ⇒ {
      iter((rangeLower, rangeHigher))
        .foreach((c: Coord) ⇒ f(c))
     }
    this
  }

  def turnOn(rangeLower: Coord, rangeHigher: Coord) = {
    act(rangeLower, rangeHigher)(pair ⇒ grid(pair._1)(pair._2) = true)
  }

  def turnOff(rangeLower: Coord, rangeHigher: Coord) = {
    act(rangeLower, rangeHigher)(pair ⇒ grid(pair._1)(pair._2) = false)
  }

  def toggle(rangeLower: Coord, rangeHigher: Coord) = {
    act(rangeLower, rangeHigher)(pair ⇒ grid(pair._1)(pair._2) = !grid(pair._1)(pair._2))
  }

  def lights: Int = {
    iter((0, 0), (dimension - 1, dimension - 1))
      .filter(c ⇒ grid(c._1)(c._2) == true).length
  }

  def instruction(action: String) : GridInstruction = {
    val regex = "^([a-z ]+)([0-9]+),([0-9]+) through ([0-9]+),([0-9]+)$".r
    action match {
      case(regex(act, lowerX, lowerY, higherX, higherY)) ⇒
        GridInstruction(actionStringToAction(act),
                        (lowerX.toInt, lowerY.toInt),
                        (higherX.toInt, higherY.toInt))
      case(_) ⇒ throw new UnsupportedOperationException(action)
    }
  }

  def actionStringToAction(action: String) = action match {
    case("turn on ") ⇒ "on"
    case(_) ⇒ throw new UnsupportedOperationException(action)
  }
}

object LightsGrid {
  def apply(dimension: Int) =
    new LightsGrid(dimension)
}
