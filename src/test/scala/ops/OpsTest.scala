package util

import org.scalatest.{path, Matchers}

class OpsTest extends path.FunSpec with Matchers {
  describe("makeIndexPairs") {
    it("handles empty string") {
      Ops.makeIndexPairs(0) shouldEqual(Vector())
    }

    it("handles one char") {
      Ops.makeIndexPairs(1) shouldEqual(Vector())
    }

    it("works with two chars") {
      Ops.makeIndexPairs(2) shouldEqual(Vector((0, 1)))
    }
    it("works with three chars") {
      Ops.makeIndexPairs(3) shouldEqual(Vector((0, 1), (1, 2)))
    }

    it("works with three chars, skipping one") {
      Ops.makeIndexPairs(3, 2) shouldEqual(Vector((0, 2)))
    }
    it("works with four chars, skipping one") {
      Ops.makeIndexPairs(4, 2) shouldEqual(Vector((0, 2), (1, 3)))
    }
  }

  describe("makeCharPairs") {
    it("for 2-char string") {
      Ops.makeCharPairs("hi") shouldEqual(Vector(('h', 'i')))
    }

    it("and with three chars") {
      Ops.makeCharPairs("hit") shouldEqual(Vector(('h', 'i'), ('i', 't')))
    }

    it("three with skipping one") {
      Ops.makeCharPairs("hit", 2) shouldEqual(Vector(('h', 't')))
    }

    it("and even more chars") {
      Ops.makeCharPairs("hint") shouldEqual(Vector(('h', 'i'), ('i', 'n'), ('n', 't')))
    }

    it("and even more chars, skipping one") {
      Ops.makeCharPairs("hint", 2) shouldEqual(Vector(('h', 'n'), ('i', 't')))
    }
  }
}
