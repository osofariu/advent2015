package say

import org.scalatest.{path, Matchers}

class SayTest extends path.FunSpec with Matchers {

  describe("say detects consecutive digits when no other digits present") {
    it("digits of the same kind") {
      Say.say("1") shouldEqual("11".toVector)
    }

    it("digits of two kinds") {
      Say.say("11222") shouldEqual("2132".toVector)
    }
  }

  describe("repetitively say chars") {

    it("a couple of times") {
      Say.repeatSay("11222", 3) shouldEqual("111231131112".toVector)
    }

    it("works on the expected input 40 times") {
      Say.repeatSay("1321131112", 40).length shouldEqual(492982)
    }

    it("works on the expected input 50 times") {
      Say.repeatSay("1321131112", 50).length shouldEqual(6989950)
    }
  }
}
