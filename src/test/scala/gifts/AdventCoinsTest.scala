package gifts

import org.scalatest.{path, Matchers}

class AdventCoinsTest extends path.FunSpec with Matchers {
  describe("find number for secret so that MD5 hash starts with 00000") {
    val prefixForHash = "yzbqklnj"

    it("gets hash for secret") {
      val md5Hash = AdventCoins.md5Hash("hello")
      md5Hash shouldEqual("5d41402abc4b2a76b9719d911017c592")
    }

    it("keeps trying numeric strincs to find MD5 hash starting with 00000") {
      val prefixForAdventHash = AdventCoins.magicHash(prefixForHash, "00000")
      prefixForAdventHash.get shouldEqual(282749)
    }
    it("tries harder when the MD5 hash has to start with six Zeroes"){
      val prefixForAdventHash = AdventCoins.magicHash(prefixForHash, "000000")
      prefixForAdventHash.get shouldEqual(9962624)
    }
  }
}
