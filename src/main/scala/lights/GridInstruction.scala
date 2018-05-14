package lights

case class GridInstruction(op: String, lowerCoord: (Int, Int),
                           higherCoord: (Int, Int))
