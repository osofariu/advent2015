package wrap

import org.scalatest.{path, Matchers}

class GiftWrapTest extends path.FunSpec with Matchers {

  describe("given a GiftWrap for a file of dimensions") {
    val wrapJob = GiftWrap("2_input.txt")

    it("adds up all the sizes to determine paper needed") {
      wrapJob.paperNeeded shouldEqual 1586300
    }

    it("adds up same to determine ribbon needed") {
      wrapJob.ribbonNeeded shouldEqual 3737498
    }
  }
}
