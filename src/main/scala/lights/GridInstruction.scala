package lights

import types.Coord

case class GridInstruction(op: String, lowerCoord: Coord, higherCoord: Coord)
