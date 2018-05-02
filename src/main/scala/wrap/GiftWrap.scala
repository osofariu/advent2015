package wrap

import scala.io.Source

case class GiftWrap(fileName: String) {
  def processPackages(packageFun: GiftBox => Int) : Int = {
    val dimensions = Source.fromFile(fileName).getLines()
    dimensions.map(dimension => GiftBox(dimension))
      .map(packageFun)
      .sum
  }

  def paperNeeded : Int = processPackages(giftBox => giftBox.wrappingSize)

  def ribbonNeeded: Int = processPackages(giftBox => giftBox.ribbonSize)
}
