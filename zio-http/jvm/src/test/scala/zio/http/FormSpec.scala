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

import java.nio.charset.StandardCharsets

import scala.annotation.nowarn

import zio._
import zio.test.Assertion._
import zio.test.TestAspect._
import zio.test._

import zio.stream.{ZStream, ZStreamAspect}

import zio.http.Header.ContentTransferEncoding
import zio.http.forms.Fixtures._

object FormSpec extends ZIOHttpSpec {
  def extractStatus(response: Response): Status = response.status

  val urlEncodedSuite =
    suite("application/x-www-form-urlencoded ")(
      test("encoding") {
        val form    = Form.fromStrings("name" -> "John", "age" -> "30")
        val encoded = form.urlEncoded
        assertTrue(
          encoded == "name=John&age=30",
          form.formData.size == 2,
        )
      },
      test("decoding") {
        val form = ZIO.fromEither(Form.fromURLEncoded("name=John&age=30", StandardCharsets.UTF_8))
        form.map { form =>
          assertTrue(
            form.get("age").get.stringValue.get == "30",
            form.get("name").get.stringValue.get == "John",
          )
        }
      },
    )

  @nowarn
  val multiFormSuite = suite("multipart/form-data")(
    test("encoding") {

      val form = Form(
        FormField.textField("csv-data", "foo,bar,baz", MediaType.text.csv),
        FormField.binaryField(
          "file",
          Chunk[Byte](0x50, 0x4e, 0x47),
          MediaType.image.png,
        ),
        FormField.binaryField(
          "corgi",
          Chunk.fromArray(base64Corgi.getBytes()),
          MediaType.image.png,
          Some(ContentTransferEncoding.Base64),
          Some("corgi.png"),
        ),
      )

      val boundary = Boundary("(((AaB03x)))")

      val actualByteStream = form.multipartBytes(boundary)

      for {
        form2       <- Form.fromMultipartBytes(multipartFormBytes2)
        actualBytes <- actualByteStream.runCollect
      } yield assertTrue(
        actualBytes == multipartFormBytes2,
        form2 == form,
      )
    },
    test("encoding with custom parameters [charset]") {

      val form = Form(
        FormField.textField(
          "csv-data",
          "foo,bar,baz",
          MediaType.text.csv.copy(parameters = Map("charset" -> "UTF-8")),
        ),
      )

      val actualByteStream = form.multipartBytes(Boundary("(((AaB03x)))"))

      def stringify(bytes: Chunk[Byte]): String =
        new String(bytes.toArray, StandardCharsets.UTF_8)

      for {
        form4       <- Form.fromMultipartBytes(multipartFormBytes4)
        actualBytes <- actualByteStream.runCollect
      } yield assertTrue(
        stringify(actualBytes) == stringify(multipartFormBytes4),
        form4.formData.head.contentType == form.formData.head.contentType,
      )
    },
    test("decoding") {
      val boundary = Boundary("AaB03x")

      for {
        form <- Form.fromMultipartBytes(multipartFormBytes1)
        encoding = form.multipartBytes(boundary)
        bytes <- encoding.runCollect
        (text: FormField.Text) :: (image1: FormField.Binary) :: (image2: FormField.Binary) :: Nil =
          form.formData.toList: @unchecked
      } yield assert(bytes)(equalTo(multipartFormBytes1)) &&
        assert(form.formData.size)(equalTo(3)) &&
        assert(text.name)(equalTo("submit-name")) &&
        assert(text.value)(equalTo("Larry")) &&
        assert(text.contentType)(equalTo(MediaType.text.plain)) &&
        assert(text.filename)(isNone) &&
        assert(image1.name)(equalTo("files")) &&
        assert(image1.data)(equalTo(Chunk[Byte](0x50, 0x4e, 0x47))) &&
        assert(image1.contentType)(equalTo(MediaType.image.png)) &&
        assert(image1.transferEncoding)(isNone) &&
        assert(image1.filename)(isSome(equalTo("file1.txt"))) &&
        assert(image2.name)(equalTo("corgi")) &&
        assert(image2.contentType)(equalTo(MediaType.image.png)) &&
        assert(image2.transferEncoding)(isSome(equalTo(ContentTransferEncoding.Base64))) &&
        assert(image2.data)(equalTo(Chunk.fromArray(base64Corgi.getBytes())))
    },
    test("decoding 2") {
      Form.fromMultipartBytes(multipartFormBytes3).map { form =>
        assertTrue(
          form.get("file").get.filename.get == "test.jsonl",
          form.get("file").get.stringValue.isEmpty,
          form.get("file").get.asInstanceOf[FormField.Binary].data.size == 69,
        )
      }

    },
    test("decoding no lf at the end") {
      val body = Chunk.fromArray(
        s"""|--(((AaB03x)))${CR}
            |Content-Disposition: form-data; name="hocon-data"${CR}
            |Content-Type: text/plain${CR}
            |${CR}
            |foos: []${CR}
            |--(((AaB03x)))${CR}
            |Content-Disposition: form-data; name="json-data"${CR}
            |Content-Type: text/plain${CR}
            |${CR}
            |{ "bars": [] }${CR}
            |--(((AaB03x)))--""".stripMargin.getBytes(),
      )

      val form = Form(
        FormField.textField("hocon-data", "foos: []", MediaType.text.`plain`),
        FormField.textField("json-data", """{ "bars": [] }""", MediaType.text.`plain`),
      )

      Form
        .fromMultipartBytes(body)
        .map(form2 => assertTrue(form2 == form))
    },
  )

  val multiFormStreamingSuite: Spec[Any, Throwable] =
    suite("multipart/form-data streaming")(
      test("encoding") {

        val form = Form(
          FormField.textField("csv-data", "foo,bar,baz", MediaType.text.csv),
          FormField.streamingBinaryField(
            "file",
            ZStream.fromChunk(Chunk[Byte](0x50, 0x4e, 0x47)) @@ ZStreamAspect.rechunk(3),
            MediaType.image.png,
          ),
          FormField.streamingBinaryField(
            "corgi",
            ZStream.fromChunk(Chunk.fromArray(base64Corgi.getBytes())) @@ ZStreamAspect.rechunk(8),
            MediaType.image.png,
            Some(ContentTransferEncoding.Base64),
            Some("corgi.png"),
          ),
        )

        val actualByteStream = form.multipartBytes(Boundary("(((AaB03x)))"))

        def stringify(bytes: Chunk[Byte]): String =
          new String(bytes.toArray, StandardCharsets.UTF_8)

        for {
          form2         <- Form.fromMultipartBytes(multipartFormBytes2)
          actualBytes   <- actualByteStream.runCollect
          collectedForm <- form.collectAll
          l = stringify(actualBytes)
          r = stringify(multipartFormBytes2)
        } yield assertTrue(
          l == r &&
            form2 == collectedForm,
        )
      },
      test("decoding") {
        val boundary = Boundary("AaB03x")

        val stream = ZStream.fromChunk(multipartFormBytes1) @@ ZStreamAspect.rechunk(4)
        val form   = StreamingForm(stream, boundary)

        form.fields
          .mapZIOPar(1) {
            case sb: FormField.StreamingBinary =>
              sb.collect
            case other: FormField              =>
              ZIO.succeed(other)
          }
          .runCollect
          .map { formData =>
            val (text: FormField.Text) :: (image1: FormField.Binary) :: (image2: FormField.Binary) :: Nil =
              formData.toList: @unchecked
            assert(formData.size)(equalTo(3)) &&
            assert(text.name)(equalTo("submit-name")) &&
            assert(text.value)(equalTo("Larry")) &&
            assert(text.contentType)(equalTo(MediaType.text.plain)) &&
            assert(text.filename)(isNone) &&
            assert(image1.name)(equalTo("files")) &&
            assert(image1.data)(equalTo(Chunk[Byte](0x50, 0x4e, 0x47))) &&
            assert(image1.contentType)(equalTo(MediaType.image.png)) &&
            assert(image1.transferEncoding)(isNone) &&
            assert(image1.filename)(isSome(equalTo("file1.txt"))) &&
            assert(image2.name)(equalTo("corgi")) &&
            assert(image2.contentType)(equalTo(MediaType.image.png)) &&
            assert(image2.transferEncoding)(isSome(equalTo(ContentTransferEncoding.Base64))) &&
            assert(image2.data)(equalTo(Chunk.fromArray(base64Corgi.getBytes())))
          }
      },
      test("decoding 2") {
        val boundary      = Boundary("X-INSOMNIA-BOUNDARY")
        val stream        = ZStream.fromChunk(multipartFormBytes3) @@ ZStreamAspect.rechunk(16)
        val streamingForm = StreamingForm(stream, boundary)
        streamingForm.collectAll.map { form =>
          val contents =
            new String(form.get("file").get.asInstanceOf[FormField.Binary].data.toArray, StandardCharsets.UTF_8)
          assertTrue(
            form.get("file").get.filename.get == "test.jsonl",
            form.get("file").get.stringValue.isEmpty,
            form.get("file").get.asInstanceOf[FormField.Binary].data.size == 69,
            contents == """{"prompt": "<prompt text>", "completion": "<ideal generated text>"}""" + "\r\n",
          )
        }
      },
      test("decoding large") {
        val N = 1024 * 1024
        for {
          bytes <- Random.nextBytes(N)
          form           = Form(
            Chunk(
              FormField.Simple("foo", "bar"),
              FormField.Binary("file", bytes, MediaType.image.png),
            ),
          )
          boundary       = Boundary("X-INSOMNIA-BOUNDARY")
          formByteStream = form.multipartBytes(boundary).rechunk(1024)
          streamingForm  = StreamingForm(formByteStream, boundary)
          collected <- streamingForm.collectAll
        } yield assertTrue(
          collected.map.contains("file"),
          collected.map.contains("foo"),
          collected.get("file").get.asInstanceOf[FormField.Binary].data == bytes,
        )
      },
      test("decoding large from single chunk") {
        val N = 1024 * 1024
        for {
          bytes <- Random.nextBytes(N)
          form     = Form(
            Chunk(
              FormField.Simple("foo", "bar"),
              FormField.Binary("file", bytes, MediaType.image.png),
            ),
          )
          boundary = Boundary("X-INSOMNIA-BOUNDARY")
          formBytes <- form.multipartBytes(boundary).runCollect
          formByteStream = ZStream.fromChunk(formBytes)
          streamingForm  = StreamingForm(formByteStream, boundary)
          collected <- streamingForm.collectAll
        } yield assertTrue(
          collected.map.contains("file"),
          collected.map.contains("foo"),
          collected.get("file").get.asInstanceOf[FormField.Binary].data == bytes,
        )
      },
      test("StreamingForm dynamically resizes") {
        val N        = 1000
        val expected = Chunk.fromArray(Array.fill(N)(scala.util.Random.nextInt()).map(_.toByte))
        val form     =
          Form(
            Chunk(
              FormField.binaryField(
                name = "identifier",
                data = Chunk(10.toByte),
                mediaType = MediaType.application.`octet-stream`,
              ),
              FormField.StreamingBinary(
                name = "blob",
                data = ZStream.fromChunk(expected),
                contentType = MediaType.application.`octet-stream`,
              ),
            ),
          )
        val boundary = Boundary("X-INSOMNIA-BOUNDARY")
        for {
          formBytes <- form.multipartBytes(boundary).runCollect
          formByteStream = ZStream.fromChunk(formBytes)
          streamingForm  = StreamingForm(formByteStream, boundary, 16)
          out <- streamingForm.collectAll
          res = out.get("blob").get.asInstanceOf[FormField.Binary].data
        } yield assertTrue(res == expected)
      } @@ timeout(3.seconds),
      test("decoding random form") {
        check(Gen.chunkOfBounded(2, 8)(formField)) { fields =>
          for {
            boundary <- Boundary.randomUUID
            stream = Form(fields.map(_._1): _*).multipartBytes(boundary).rechunk(100)
            form <- StreamingForm(stream, boundary).collectAll

            normalizedIn  <- ZIO.foreach(fields.map(_._1)) { field =>
              field.asChunk.map(field.name -> _)
            }
            normalizedOut <- ZIO.foreach(form.formData) { field =>
              field.asChunk.map(field.name -> _)
            }
          } yield assertTrue(
            normalizedIn == normalizedOut,
          )
        }
      },
      test("decoding large form with random chunk and buffer sizes") {
        val N = 1024 * 1024
        check(Gen.int(1, N), Gen.int(1, N)) { case (chunkSize, bufferSize) =>
          for {
            bytes <- Random.nextBytes(N)
            form = Form(
              Chunk(
                FormField.Simple("foo", "bar"),
                FormField.Binary("file", bytes, MediaType.image.png),
              ),
            )
            boundary <- Boundary.randomUUID
            stream        = form.multipartBytes(boundary).rechunk(chunkSize)
            streamingForm = StreamingForm(stream, boundary, bufferSize)
            collected <- streamingForm.collectAll
          } yield assertTrue(
            collected.map.contains("file"),
            collected.map.contains("foo"),
            collected.get("file").get.asInstanceOf[FormField.Binary].data == bytes,
          )
        }
      } @@ samples(10),
      test("output stream maintains the same chunk structure as the input stream") {
        val form           = Form(
          Chunk(
            FormField.StreamingBinary(
              name = "blob",
              data = ZStream.fromChunk(Chunk(1, 2).map(_.toByte)) ++
                ZStream.fromChunk(Chunk(3).map(_.toByte)),
              contentType = MediaType.application.`octet-stream`,
            ),
          ),
        )
        val boundary       = Boundary("X-INSOMNIA-BOUNDARY")
        val formByteStream = form.multipartBytes(boundary)
        val streamingForm  = StreamingForm(formByteStream, boundary)
        val expected       = Chunk(Chunk[Byte](1, 2), Chunk[Byte](3))
        streamingForm.fields.flatMap {
          case sb: FormField.StreamingBinary => sb.data
          case _                             => ZStream.empty
        }
          .mapChunks(Chunk.single)
          .filter(_.nonEmpty)
          .runCollect
          .map { c => assertTrue(c == expected) }
      },
      test("crlf not stripped") {
        val content        = "1,2\r\n3,4\r\n"
        val form           = Form(
          Chunk(
            FormField.textField("csv", content, MediaType.text.`csv`),
          ),
        )
        val boundary       = Boundary("X-INSOMNIA-BOUNDARY")
        val formByteStream = form.multipartBytes(boundary)
        val streamingForm  = StreamingForm(formByteStream, boundary)
        streamingForm.fields.mapZIO { field =>
          field.asChunk
        }.runCollect.map { chunks =>
          val s = chunks.headOption.map(_.asString)
          assertTrue(s.contains(content))
        }
      },
    ) @@ sequential

  def spec =
    suite("FormSpec")(urlEncodedSuite, multiFormSuite, multiFormStreamingSuite)
}
