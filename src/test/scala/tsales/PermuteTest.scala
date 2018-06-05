package tsales

import org.scalatest.{path, Matchers}

class PermuteTest extends path.FunSpec with Matchers {

  describe("given a list, create unique permutations") {

    it("for three items") {
      Permute.permutations(List("1", "2", "3")) shouldEqual
      Set(List("1", "2", "3"),
          List("1", "3", "2"),
          List("2", "1", "3"),
          List("2", "3", "1"),
          List("3", "2", "1"),
          List("3", "1", "2"))
    }
  }
}
