package say

import org.scalatest.{path, Matchers}

class SayTest extends path.FunSpec with Matchers {

//  describe("say detects consecutive digits when no other digits present") {
//    it("digits of the same kind") {
//      Say.say("1") shouldEqual("11")
//    }
//
//    it("digits of two kinds") {
//      Say.say("11222") shouldEqual("2132")
//    }
//  }

  describe("repetitively say chars") {

//    it("a couple of times") {
//      Say.repeatSay("11222", 3) shouldEqual("111231131112")
//    }

    it("works on the expected input") {
      Say.repeatSay("1321131112", 25) shouldEqual(10)
    }
  }
}
