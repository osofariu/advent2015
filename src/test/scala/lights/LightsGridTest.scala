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

    it("turns on lower right half of the grid") {
      grid.turnOn((5, 5),(9 ,9)).lights shouldEqual(25)
    }

    it("lights stay on when multiple grids are turned on") {
      grid.turnOn((0, 0), (4, 4))
        .turnOn((5, 5), (9, 9))
        .turnOn((5, 0), (9, 5))
        .turnOn((0, 5),(5, 9)).lights shouldEqual(100)
    }
  }

  describe("turn off lights") {
    val grid = LightsGrid(10).turnOn((0, 0), (9, 9))

    it("turning off a middle section, leaves other lights on") {
      grid.turnOff((3, 3), (7, 7)).lights shouldEqual(75)
    }

    it("lights stay off when multiple grids are turned off") {
      grid.turnOff((0, 3), (3, 5))
        .turnOff((5, 3), (9, 9))
        .lights shouldEqual(53)
    }
  }

  describe("toggle lights") {
    val grid = LightsGrid(10)
    it("turns on, then off grid") {
      grid.toggle((1, 1), (8, 9))
        .toggle((1, 1), (8, 9))
        .lights shouldEqual(0)
    }
  }

  describe("grid with instructions from file") {
    describe("can understand instructions from file") {
      val grid = LightsGrid(1000)

      it("can turn on from file") {
        val res = grid.processInstruction("turn on 943,30 through 990,907")
        res.op shouldEqual("on")
        res.lowerCoord shouldEqual((943, 30))
        res.higherCoord shouldEqual(990, 907)
      }

      it("can turn off from file") {
        val res = grid.processInstruction("turn off 3,330 through 991,407")
        res.op shouldEqual("off")
        res.lowerCoord shouldEqual((3, 330))
        res.higherCoord shouldEqual(991, 407)
      }
      it("can toggle from file") {
        val res = grid.processInstruction("toggle 1,2 through 3,4")
        res.op shouldEqual("toggle")
        res.lowerCoord shouldEqual((1, 2))
        res.higherCoord shouldEqual(3, 4)
      }
    }

    describe("read the instructions from file") {
      it("reads from file into a 1,000 X 1,000 grid") {
        val grid = LightsGrid(1000)
        grid.instructionsFromFile("input/6_input.txt").lights shouldEqual(569999)
      }
    }
  }
}
