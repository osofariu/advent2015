package lights

import org.scalatest.{path, Matchers}

class LightsGridTest extends path.FunSpec with Matchers {

  describe("turn on lights") {
    val grid = LightsGrid(10)

    it("has one lights on, when range is (0, 0), (0, 0)") {
      grid.turnOn((0, 0), (0, 0)).lights shouldEqual(1)
    }

    it("has four lights on") {
      grid.turnOn((0, 0), (1, 1)).lights shouldEqual(4)
    }

    it("turns first three columns on") {
      grid.turnOn((0, 0), (9, 2)).lights shouldEqual(30)
    }

    it("turons on lower right half of the grid") {
      grid.turnOn((5, 5),(9 ,9)).lights shouldEqual(25)
    }
  }
}
