package wrap

import org.scalatest.{path, Matchers}

class GiftBoxTest extends path.FunSpec with Matchers {

  describe("Wrapping a box") {

    describe("With wrapping paper") {
      it("wrapping a cube 1-ft box") {
        val oneBox = GiftBox(1, 1, 1)
        oneBox.wrappingSize shouldEqual(7)
      }

      it("wrapping a larger box") {
        val oneBox = GiftBox(2, 3, 4)
        oneBox.wrappingSize shouldEqual(12 + 24 + 16 + 6)
      }

      describe("converting a string measurement into an object") {
        it("can be constructed from a string") {
          GiftBox("23x444x4254") shouldEqual GiftBox(23, 444, 4254)
        }

        it("throws exception if string doesn't have three dimensions") {
          assertThrows[IllegalArgumentException] {
            GiftBox("12x33")
          }
        }

        it("throws exception if string doesn't have two separators") {
          assertThrows[IllegalArgumentException] {
            GiftBox("12x33y44")
          }
        }
      }
    }

    describe("With ribbon") {
      it("wrapping a 1x2x3 box") {
        val box = GiftBox(1, 2, 3)
        box.ribbonSize shouldEqual(12)
      }
    }
  }
}
