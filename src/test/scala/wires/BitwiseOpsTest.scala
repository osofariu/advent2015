package wires

import org.scalatest.{path, Matchers}

class BitwiseOpsTest extends path.FunSpec with Matchers {

  describe("bitwise operations") {
    it("will FLIP the bits for 123") {
      BitwiseOps.not(123) shouldEqual 65412
    }

    it("will AND 123 with 456") {
      BitwiseOps.and(123, 456) shouldEqual 72
    }
    it("will OR 123 with 456") {
      BitwiseOps.or(123, 456) shouldEqual 507
    }
    it("will XOR 123 with 456") {
      BitwiseOps.xor(123, 456) shouldEqual 435
    }
    it("will LSHIFT 123 by 2") {
      BitwiseOps.lshift(123, 2) shouldEqual 492
    }
    it("will RSHIFT 456 by 2") {
      BitwiseOps.rshift(456, 2).toInt shouldEqual 114
    }
  }
}
