package zio.http

import zio.test._
import zio.test.Assertion._

object PathCodecJSPlatformSpecificSpec extends ZIOSpecDefault {

  def spec = suite("PathCodecPlatformSpecificSpec")(
    test("parseInt should correctly parse a valid integer from a CharSequence") {
      val charSequence = "12345"
      val result       = new PathCodecJSPlatformSpecific {}.parseInt(charSequence, 0, charSequence.length, 10)
      assert(result)(equalTo(12345))
    },
    test("parseInt should throw an error for an invalid radix") {
      val charSequence = "12345"
      assertThrows[NumberFormatException] {
        new PathCodecJSPlatformSpecific {}.parseInt(charSequence, 0, charSequence.length, Character.MAX_RADIX + 1)
      }
    },
    test("parseLong should correctly parse a valid long from a CharSequence") {
      val charSequence = "123456789012345"
      val result       = new PathCodecJSPlatformSpecific {}.parseLong(charSequence, 0, charSequence.length, 10)
      assert(result)(equalTo(123456789012345L))
    },
    test("parseLong should throw an error for an invalid input") {
      val charSequence = "invalid123"
      assertThrows[NumberFormatException] {
        new PathCodecJSPlatformSpecific {}.parseLong(charSequence, 0, charSequence.length, 10)
      }
    },
  )
}
