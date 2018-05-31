package gifts

import org.scalatest.{path, Matchers}

class NiceStringTest extends path.FunSpec with Matchers {
  describe("Words with vowels") {
    it("have three vowels") {
      NiceString.isNice("aebbu") shouldBe true
    }

    it("two vowels are not enough") {
      NiceString.isNice("aebb") shouldBe(false)
    }
  }

  describe("Words that have two consecutive letters") {
    it("has two letters") {
     NiceString.isNice("aebu") shouldBe false
    }
  }

  describe("cannot contain ab, cd, pq, or xy even when other requirements met") {
    it("cannot contain ab") {
      NiceString.isNice("abbeu") shouldBe false
    }
    it("cannot contain cd") {
      NiceString.isNice("aebbucd") shouldBe false
    }
    it("cannot contain pq") {
      NiceString.isNice("aebbpqu") shouldBe false
    }
    it("cannot contain xy") {
      NiceString.isNice("apqebbu") shouldBe false
    }
  }

  describe("from file, expectation of nice words") {
    it("has nice Words") {
      val niceWords = NiceString.niceFile("input/5_input.txt")
      niceWords shouldEqual(236)
    }
  }
}
