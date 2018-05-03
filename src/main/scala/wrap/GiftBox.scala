package wrap

case class GiftBox(length: Int, width: Int, height: Int) {

  def wrappingSize : Int = {
    2 * length * width + 2 * length * height + 2 * width * height +
    smallestTwoDimensions.reduce((a, b) => a * b)
  }

  def ribbonSize : Int = {
    length * width * height +
    smallestTwoDimensions.reduce((a, b) => 2 * a + 2 * b)
  }

  private def smallestTwoDimensions: List[Int] =
    List(length, width, height).sorted.take(2)
}

object GiftBox {
  def apply(dimensions: String) : GiftBox = {
    dimensions.split("x") match {
      case Array(first, second, third) =>
        GiftBox(first.toInt, second.toInt, third.toInt)
      case _ => throw new IllegalArgumentException
    }
  }
}
