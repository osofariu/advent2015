package lights

import scala.io.Source

case class LightsGrid(dimension: Int) {

  private val grid = Array.ofDim[Int](dimension, dimension)

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
    act(rangeLower, rangeHigher)(pair ⇒ grid(pair._1)(pair._2) = 1)
  }

  def turnOff(rangeLower: Coord, rangeHigher: Coord) = {
    act(rangeLower, rangeHigher)(pair ⇒ grid(pair._1)(pair._2) = 0)
  }

  def toggle(rangeLower: Coord, rangeHigher: Coord) = {
    act(rangeLower, rangeHigher)(pair ⇒ grid(pair._1)(pair._2) = 1 ^ grid(pair._1)(pair._2))
  }

  def lights: Int = {
    iter((0, 0), (dimension - 1, dimension - 1))
      .filter(c ⇒ grid(c._1)(c._2) == 1).length
  }

  def instructionsFromFile(filename: String): LightsGrid = {
    val instructions = Source.fromFile(filename).getLines()
    instructions.foreach(instruction => {
      val gridInstruction = processInstruction(instruction)
      gridInstruction.op match {
        case "on" => turnOn(gridInstruction.lowerCoord, gridInstruction.higherCoord)
        case "off" => turnOff(gridInstruction.lowerCoord, gridInstruction.higherCoord)
        case "toggle" => toggle(gridInstruction.lowerCoord, gridInstruction.higherCoord)
      }
    })
    this
  }

  def processInstruction(action: String) : GridInstruction = {
    val regex = "^(turn on|turn off|toggle) ([0-9]+),([0-9]+) through ([0-9]+),([0-9]+)$".r
    action match {
      case(regex(act, lowerX, lowerY, higherX, higherY)) ⇒
        GridInstruction(actionStringToAction(act),
                        (lowerX.toInt, lowerY.toInt),
                        (higherX.toInt, higherY.toInt))
      case(_) ⇒ throw new UnsupportedOperationException(action)
    }
  }

  def actionStringToAction(action: String) = action match {
    case("turn on") ⇒ "on"
    case("turn off") ⇒ "off"
    case("toggle") => "toggle"
  }
}

object LightsGrid {
  def apply(dimension: Int) =
    new LightsGrid(dimension)
}
