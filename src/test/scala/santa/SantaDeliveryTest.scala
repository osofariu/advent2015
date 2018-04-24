package santa

import org.scalatest.{Matchers, path}

class SantaDeliveryTest extends path.FunSpec with Matchers {
 
  val delivery = SantaDelivery

  describe("null moves") {
    it("throws UnsupportedOperationException") {
      assertThrows[UnsupportedOperationException] {
        delivery.floor(null)
      }
    }
  }

  describe("no movement") {
    val moves = ""
    it("keeps Santa on floor 0") {
      delivery.floor(moves) shouldEqual 0
    }
  }

  describe("move up") {
    val move_one_up = "("
    it("puts you on floor 1") {
      delivery.floor(move_one_up) shouldEqual 1
    }
  }

  describe("move down") {
    val move_one_down = ")"
    it("puts you on floor -1") {
      delivery.floor(move_one_down) shouldEqual -1
    }
  }

  describe("find basement on movements") {
    val moves = "(()))"
    it("reports the first time basement was hit, when it was") {
      val firstBasement = delivery.movement(moves).find(state => state.floor == -1)
      firstBasement.get.position shouldBe 5
    }
  }

  describe("processing a file with moves") {
    val fileName = "1a_input.txt"
    val positions = delivery.movementFromFile(fileName)

    it("gives the final floor") {
      positions.last.floor shouldEqual(232)
    }

    it("finds first basement run") {
      val res = positions.find(pos => pos.floor == -1)
      println("first basement: " + res.get.position)
    }
  }
}
