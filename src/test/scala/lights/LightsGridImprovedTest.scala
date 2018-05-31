package lights

import org.scalatest.{path, Matchers}

class LightsGridImprovedTest extends path.FunSpec with Matchers {
  describe("improved turn on") {
    val grid = LightsGridImproved(10)

    it("turns on by incrementing from 0") {
      grid.turnOn((0, 0), (1, 1)).lights shouldEqual 4
    }

    it("turns on again by incrementing from 1") {
      grid.turnOn((0, 0), (1, 1)).
        turnOn((0, 0), (1, 1)).lights shouldEqual 8
    }
    describe("toggle") {
      it("toggles from 0, increasing by 2") {
        grid.toggle((0, 0), (1, 1)).lights shouldEqual 8
      }
   }
    describe("turn off") {
      it("from scratch, it doesn't go below 0") {
        grid.turnOff((0, 0), (9, 9)).lights shouldEqual 0
      }
      it("with initial values, it decreases them by 1") {
        grid.turnOn((0, 0), (2, 2))
          .turnOff((0, 0), (2, 2)).lights shouldEqual(0)
      }
    }

    describe("from File") {
      it("can add-up lights the new way") {
      val grid = LightsGridImproved(1000)
        grid.instructionsFromFile("input/6_input.txt").lights shouldEqual 17836115
      }
    }
  }
}
