package matchsticks

import org.scalatest.{path, Matchers}

class CharEncoderTest extends path.FunSpec with Matchers {

  describe("First round of encoding") {
    describe("Encode first character as regular string") {

      it("given a single character") {
        CharEncoder.encode("a") shouldEqual Seq(PlainChar("a"))
      }

      it("given multiple characters it encodes the, all as Plain Chars") {
        CharEncoder.encode("hi") shouldEqual Seq(PlainChar("h"), PlainChar("i"))
      }

      it("allows un-escaped double-quote as a character") {
        CharEncoder.encode("\"h") shouldEqual Seq(DoubleQuoteChar, PlainChar("h"))
      }
    }

    describe("using escaped character") {
      it("encodes the double quote") {
        CharEncoder.encode("\\\"a") shouldEqual Seq(EscapedChar("\""), PlainChar("a"))
      }
      it("encodes the backslash") {
        CharEncoder.encode("\\\\a") shouldEqual Seq(EscapedChar("\\"), PlainChar("a"))
      }
    }

    describe("hex-encoded chars") {
      it("encodes them as HexChar") {
        CharEncoder.encode("\\x12b") shouldEqual Seq(HexChar("12"), PlainChar("b"))
      }
    }
  }
  describe("re-encode strings") {
    it("regular ones") {
      CharEncoder.reencode("\"abc\"") shouldEqual(""""\"abc\""""")
    }
    it("escaped quote") {
      CharEncoder.reencode("\"go\"long\"") shouldEqual(""""\"go\"long\""""")
    }
    it("escaped double-quote") {
      CharEncoder.reencode("\"a\\b\"") shouldEqual(""""\"a\\b\""""")
    }
  }
}
