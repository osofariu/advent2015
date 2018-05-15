package lights

import types._

class LightsGridImproved(dimension: Int) extends LightsGrid(dimension) {

  override def turnOn(rangeLower: Coord, rangeHigher: Coord) = {
    act(rangeLower, rangeHigher)(coord ⇒ grid(coord._1)(coord._2) += 1
    )}

  override def turnOff(rangeLower: Coord, rangeHigher: Coord) = {
    act(rangeLower, rangeHigher)(
      coord ⇒ {
        val currectValue = grid(coord._1)(coord._2)
        grid(coord._1)(coord._2) = Math.max(0, currectValue - 1)
      }
    )
  }

  override def toggle(rangeLower: Coord, rangeHigher: Coord) = {
    turnOn(rangeLower, rangeHigher).turnOn(rangeLower, rangeHigher)
  }

  override def lights: Int = {
    iter((0, 0), (dimension - 1, dimension - 1))
      .map(coord ⇒ grid(coord._1)(coord._2))
      .sum
  }
}

object LightsGridImproved {
  def apply(dimension: Int) = new LightsGridImproved(dimension)
}
