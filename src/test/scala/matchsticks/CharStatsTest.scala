package matchsticks

import org.scalatest.{path, Matchers}

class CharStatsTest extends path.FunSpec with Matchers {

  describe("Calculate Char Stats for string") {

    it("stats for basic strings with no escaped or hex characters") {
      CharStats.space("\"stringone\"") shouldEqual SpaceStats(code = 11, memory = 9)
    }

    it("handled escaped characters") {
      CharStats.space(""""hi\"there\\you"""") shouldEqual SpaceStats(code = 16, memory = 12)
    }

    it("handles hex characters at beginning and middle of word") {
      CharStats.space(""""\xffhi\x12there"""") shouldEqual SpaceStats(code = 17, memory = 9)
    }
  }

  describe("Calculate Char Stats for file") {
    it("adds-up all Encoded Chars generated from file") {
      val statsForFile = CharStats.spaceForFile("8_input.txt")
      statsForFile.code - statsForFile.memory shouldEqual 1342
    }

    it("adds-up sll Encoded Chars generated from file after re-encoding them") {
      val reEncodedStatsForFile = CharStats.reEncodedStatsForFile("8_input.txt")
      reEncodedStatsForFile.code - reEncodedStatsForFile.memory shouldEqual 2074
    }
  }
}
