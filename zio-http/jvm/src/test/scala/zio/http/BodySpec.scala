/*
 * Copyright 2021 - 2023 Sporta Technologies PVT LTD & the ZIO HTTP contributors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package zio.http

import java.io.File
import zio._
import zio.test.Assertion.equalTo
import zio.test.TestAspect.timeout
import zio.test._
import zio.stream.ZStream

object BodySpec extends ZIOHttpSpec {
  private val testFile = new File(getClass.getResource("/TestFile.txt").getPath)

  override def spec: Spec[TestEnvironment with Scope, Throwable] =
    suite("BodySpec")(
      suite("outgoing")(
        suite("encode")(
          suite("fromStream")(
            test("success") {
              check(Gen.string) { payload =>
                val stringBuffer    = payload.getBytes(Charsets.Http)
                val responseContent = ZStream.fromIterable(stringBuffer, chunkSize = 2)
                val res             = Body.fromStreamChunked(responseContent).asString(Charsets.Http)
                assertZIO(res)(equalTo(payload))
              }
            },
          ),
          suite("fromFile")(
            test("success") {
              lazy val file = testFile
              val res       = Body.fromFile(file).flatMap(_.asString(Charsets.Http))
              assertZIO(res)(equalTo("foo\nbar"))
            },
            test("success small chunk") {
              lazy val file = testFile
              val res       = Body.fromFile(file, 3).flatMap(_.asString(Charsets.Http))
              assertZIO(res)(equalTo("foo\nbar"))
            },
          ),
        ),
      ),
      suite("mediaType")(
        test("updates the Body media type with the provided value") {
          val body = Body.fromString("test").contentType(MediaType.text.plain)
          assertTrue(body.mediaType == Option(MediaType.text.plain))
        },
      ),
      suite("multipart form boundary")(
        test("generated boundary is sanitized and RFC 2046 compliant") {
          for {
            form <- ZIO.succeed(Form(FormField.simpleField("name", "test-name")))
            body <- Body.fromMultipartFormUUID(form)
            boundaryOpt = body.contentType.flatMap(_.boundary)
            _ <- ZIO.logInfo(s"Generated boundary: ${boundaryOpt.getOrElse("No boundary found")}")
          } yield assertTrue(boundaryOpt.isDefined) &&
            assertTrue(boundaryOpt.get.toString.matches("^[a-zA-Z0-9'()+_,-./:=?]+$"))
        },
      ),
    ) @@ timeout(10 seconds)
}
