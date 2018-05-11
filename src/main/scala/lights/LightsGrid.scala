package lights

case class LightsGrid(dimension: Int) {
  private val grid = Array.ofDim[Boolean](dimension, dimension)

  type Coord = (Int, Int)
  type Range = (Coord, Coord)

  def iter(c: Range): Seq[Coord] = {
    for {
      x ← c._1._1 to c._2._1
      y ← c._1._2 to c._2._2
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

  def lights: Int = {
    println(s"DIM: ${dimension}")
    println("LEN: " + iter((0, dimension - 1), (0, dimension - 1)))
    val r = iter((0, dimension - 1), (0, dimension - 1))
      .map(coord ⇒ {
                val (x, y) = coord
                println(s"x = ${x}, y = ${y} = > ${grid(x)(y)}")
                grid(x)(y) == false
      })
    println(r)
    r.length
  }

  def printGrid = {
    act((0, 0), (dimension-1, dimension-1))(coord ⇒ {
      if (coord._2 == 0)
        println("")
        if (grid(coord._1)(coord._2) == true)
            print(" 1 ")
      else
        print(" 0 ")
     })
    println("")
  }
}

object LightsGrid {
  def apply(dimension: Int) =
    new LightsGrid(dimension)
}
