package zio.http.codec

import zio.test._
import zio.test.Assertion._

object PathCodecPlatformSpecificSpec extends ZIOSpecDefault {

  def spec = suite("PathCodecJSPlatformSpecificSpec")(
    test("parseInt should correctly parse a valid integer from a CharSequence") {
      val charSequence = "12345"
      val result       = new PathCodecPlatformSpecific {}.parseInt(charSequence, 0, charSequence.length, 10)
      assert(result)(equalTo(12345))
    },
    test("parseInt should throw an error for an invalid radix") {
      val charSequence = "12345"
      val exception    = intercept[NumberFormatException] {
        new PathCodecPlatformSpecific {}.parseInt(charSequence, 0, charSequence.length, Character.MAX_RADIX + 1)
      }
      assert(exception.getMessage)(containsString("radix"))
    },
    test("parseLong should correctly parse a valid long from a CharSequence") {
      val charSequence = "123456789012345"
      val result       = new PathCodecPlatformSpecific {}.parseLong(charSequence, 0, charSequence.length, 10)
      assert(result)(equalTo(123456789012345L))
    },
    test("parseLong should throw an error for an invalid input") {
      val charSequence = "invalid123"
      val exception    = intercept[NumberFormatException] {
        new PathCodecPlatformSpecific {}.parseLong(charSequence, 0, charSequence.length, 10)
      }
      assert(exception.getMessage)(containsString("Error at index"))
    },
  )
}
