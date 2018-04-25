package santa
import org.scalatest.{Matchers, path}

class SantaDeliveryTest extends path.FunSpec with Matchers {

  describe("null moves are not allowed") {
    it("throws UnsupportedOperationException") {
      assertThrows[UnsupportedOperationException] {
        val delivery = new SantaDelivery(null)
        delivery.finalFloorVisited
      }
    }
  }

  describe("no movement") {
    val delivery = SantaDelivery("")
    it("keeps Santa on floor 0") {
      delivery.finalFloorVisited shouldEqual 0
    }
  }

  describe("move up") {
    val delivery = SantaDelivery("(")
    it("puts you on floor 1") {
      delivery.finalFloorVisited shouldEqual 1
    }
  }

  describe("move down") {
    val move_one_down = ")"
    it("puts you on floor -1") {
      val delivery = SantaDelivery(move_one_down)
      delivery.finalFloorVisited shouldEqual -1
    }
  }

  describe("find floor in movements") {
    val moves = "(()))"
    val delivery = SantaDelivery(moves)

    it("reports the first time basement was hit, when it was") {
      val firstBasement = delivery.firstTimeFloorVisited(-1)
      firstBasement.get shouldBe 5
    }

    it("reports that the third floor was never visited") {
      val thirdFloor = delivery.firstTimeFloorVisited(3)
      thirdFloor shouldBe None
    }
  }

  describe("processing a file with moves") {
    val delivery = SantaDelivery.fromFile("1a_input.txt")

    it("gives the final floor") {
      val positions = delivery.finalFloorVisited
      positions shouldEqual(232)
    }

    it("finds first basement run") {
      val res = delivery.firstTimeFloorVisited(-1)
      res.get shouldEqual 1783
    }
  }
}
