package gifts

import org.scalatest.{path, Matchers}

class BetterNiceStringsTest extends path.FunSpec with Matchers {

  describe("repeating pair of letters") {

    it("next to each other") {
      BetterNiceString.isNice("hiababthere") shouldBe true
    }

    it("not next to each other") {
      BetterNiceString.isNice("hiabthereab") shouldBe true
    }

    it("but overlapping repeating pairs up to 3 dont count") {
      BetterNiceString.isNice("hiaaathere") shouldBe false
    }

    it("on the other hand, of thre are more than 3 it is cool") {
      BetterNiceString.isNice("hiaaaat") shouldBe true
    }
  }

  describe("one letter repeats with another in bwtween") {

    it("works with xyx") {
      BetterNiceString.isNice("hixyxhi") shouldBe true
    }

    it("fails of repeating pairs but no pair with letter in between") {
      BetterNiceString.isNice("hixyhi") shouldBe false
    }

    it("doesnt work with xyx of no repeating pairs") {
      BetterNiceString.isNice("xyx") shouldBe false
    }
  }

  describe("grand finale -- work on the file") {
    it("gives some answer, hope its right") {
      BetterNiceString.niceFile("input/5_input.txt") shouldEqual(51)
    }
  }
}
