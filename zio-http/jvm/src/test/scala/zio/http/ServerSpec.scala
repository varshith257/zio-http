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
import java.nio.file.Paths

import zio._
import zio.test.Assertion._
import zio.test.TestAspect._
import zio.test._

import zio.stream.{ZPipeline, ZStream}

import zio.http._
import zio.http.internal.{DynamicServer, HttpGen, RoutesRunnableSpec}
import zio.http.netty.NettyConfig
import zio.http.template.{body, div, id}

object ServerSpec extends RoutesRunnableSpec {

  private val nonEmptyContent = for {
    data    <- Gen.listOf(Gen.alphaNumericString)
    content <- HttpGen.nonEmptyBody(Gen.const(data))
  } yield (data.mkString(""), content)

  private val port    = 8080
  private val MaxSize = 1024 * 10
  val configApp       = Server.Config.default
    .requestDecompression(true)
    .disableRequestStreaming(MaxSize)
    .port(port)
    .responseCompression()

  private val app = serve

  def dynamicAppSpec = suite("DynamicAppSpec")(
    suite("success")(
      test("status is 200") {
        val status = Handler.ok.toRoutes.deploy.status.run()
        assertZIO(status)(equalTo(Status.Ok))
      },
      test("status is 200") {
        val res = Handler.text("ABC").toRoutes.deploy.status.run()
        assertZIO(res)(equalTo(Status.Ok))
      },
      test("content is set") {
        val res = Handler.text("ABC").toRoutes.deploy.body.mapZIO(_.asString).run()
        assertZIO(res)(containsString("ABC"))
      },
    ),
    suite("not found") {
      val app = Routes.empty
      test("status is 404") {
        val res = app.deploy.status.run()
        assertZIO(res)(equalTo(Status.NotFound))
      } +
        test("header is set") {
          val res = app.deploy.header(Header.ContentLength).run()
          assertZIO(res)(isSome(equalTo(Header.ContentLength(0L))))
        }
    } +
      suite("error") {
        val routes = Handler.fail(new Error("SERVER_ERROR")).sandbox.toRoutes
        test("status is 500") {
          val res = routes.deploy.status.run()
          assertZIO(res)(equalTo(Status.InternalServerError))
        } +
          test("content is empty") {
            val res = routes.deploy.body.mapZIO(_.asString).run()
            assertZIO(res)(isEmptyString)
          } +
          test("header is set") {
            val res = routes.deploy.header(Header.ContentLength).run()
            assertZIO(res)(isSome(anything))
          }
      } +
      suite("die") {
        val routes = Handler.die(new Error("SERVER_ERROR")).toRoutes
        test("status is 500") {
          val res = routes.deploy.status.run()
          assertZIO(res)(equalTo(Status.InternalServerError))
        } +
          test("content is empty") {
            val res = routes.deploy.body.mapZIO(_.asString).run()
            assertZIO(res)(isEmptyString)
          } +
          test("header is set") {
            val res = routes.deploy.header(Header.ContentLength).run()
            assertZIO(res)(isSome(anything))
          }
      } +
      suite("echo content") {
        val routes = (RoutePattern.any ->
          handler { (_: Path, req: Request) =>
            req.body.asString.map(text => Response.text(text))
          }).sandbox.toRoutes

        test("status is 200") {
          val res = routes.deploy.status.run()
          assertZIO(res)(equalTo(Status.Ok))
        } +
          test("body is ok") {
            val res = routes.deploy.body.mapZIO(_.asString).run(body = Body.fromString("ABC"))
            assertZIO(res)(equalTo("ABC"))
          } +
          test("empty string") {
            val res = routes.deploy.body.mapZIO(_.asString).run(body = Body.fromString(""))
            assertZIO(res)(equalTo(""))
          } +
          test("one char") {
            val res = routes.deploy.body.mapZIO(_.asString).run(body = Body.fromString("1"))
            assertZIO(res)(equalTo("1"))
          } +
          test("data") {
            val dataStream = ZStream.repeat("A").take(MaxSize.toLong)
            val app        =
              Routes(RoutePattern.any -> handler((_: Path, req: Request) => Response(body = req.body)))
            val res        =
              app.deploy.body.mapZIO(_.asChunk.map(_.length)).run(body = Body.fromCharSequenceStreamChunked(dataStream))
            assertZIO(res)(equalTo(MaxSize))
          }
      } +
      suite("headers") {
        val routes = Handler.ok.addHeader("Foo", "Bar").toRoutes
        test("headers are set") {
          val res = routes.deploy.rawHeader("Foo").run()
          assertZIO(res)(isSome(equalTo("Bar")))
        }
      } + suite("response") {
        val routes = Handler.fromResponse(Response(status = Status.Ok, body = Body.fromString("abc"))).toRoutes
        test("body is set") {
          val res = routes.deploy.body.mapZIO(_.asString).run()
          assertZIO(res)(equalTo("abc"))
        }
      } +
      suite("compression") {
        val body         = "some-text"
        val bodyAsStream = ZStream.fromChunk(Chunk.fromArray(body.getBytes))

        val routes = Routes(
          RoutePattern.any ->
            handler { (_: Path, req: Request) =>
              req.body.asString.map(body => Response.text(body))
            },
        ).sandbox.deploy.toRoutes.sandbox

        def roundTrip[R, E <: Throwable](
          routes: Routes[R, Response],
          headers: Headers,
          contentStream: ZStream[R, E, Byte],
          compressor: ZPipeline[R, E, Byte, Byte],
          decompressor: ZPipeline[R, E, Byte, Byte],
        ) = for {
          compressed <- contentStream.via(compressor).runCollect
          response   <- routes.run(body = Body.fromChunk(compressed), headers = headers)
          body       <- response.body.asChunk.flatMap(ch => ZStream.fromChunk(ch).via(decompressor).runCollect)
        } yield new String(body.toArray, StandardCharsets.UTF_8)

        test("should decompress request and compress response") {
          checkAll(
            Gen.fromIterable(
              List(
                // Content-Encoding,   Client Request Compressor, Accept-Encoding,      Client Response Decompressor
                (Header.ContentEncoding.GZip, ZPipeline.gzip(), Header.AcceptEncoding.GZip(), ZPipeline.gunzip()),
                (
                  Header.ContentEncoding.Deflate,
                  ZPipeline.deflate(),
                  Header.AcceptEncoding.Deflate(),
                  ZPipeline.inflate(),
                ),
                (Header.ContentEncoding.GZip, ZPipeline.gzip(), Header.AcceptEncoding.Deflate(), ZPipeline.inflate()),
                (Header.ContentEncoding.Deflate, ZPipeline.deflate(), Header.AcceptEncoding.GZip(), ZPipeline.gunzip()),
              ),
            ),
          ) { case (contentEncoding, compressor, acceptEncoding, decompressor) =>
            val result = roundTrip(
              routes.sandbox,
              Headers(acceptEncoding, contentEncoding),
              bodyAsStream,
              compressor,
              decompressor,
            )
            assertZIO(result)(equalTo(body))
          }
        } +
          test("pass through for unsupported accept encoding request") {
            val result = routes.run(
              body = Body.fromString(body),
              headers = Headers(Header.AcceptEncoding.Br()),
            )
            assertZIO(result.flatMap(_.body.asString))(equalTo(body))
          } +
          test("fail on getting compressed response") {
            checkAll(
              Gen.fromIterable(
                List(
                  Header.AcceptEncoding.GZip(),
                  Header.AcceptEncoding.Deflate(),
                  Header.AcceptEncoding(Header.AcceptEncoding.GZip(), Header.AcceptEncoding.Deflate()),
                ),
              ),
            ) { ae =>
              val result = routes.run(
                body = Body.fromString(body),
                headers = Headers(ae),
              )
              assertZIO(result.flatMap(_.body.asString))(not(equalTo(body)))
            }
          }
      } +
      suite("interruption")(
        test("interrupt closes the channel without response") {
          val routes = Handler.fromZIO {
            ZIO.interrupt.as(Response.text("not interrupted"))
          }.toRoutes
          assertZIO(routes.deploy.run().exit)(
            fails(hasField("class.simpleName", _.getClass.getSimpleName, equalTo("PrematureChannelClosureException"))),
          )
        },
      ) +
      suite("proxy") {
        val server = Routes(
          Method.ANY / "proxy" / trailing ->
            handler { (path: Path, req: Request) =>
              val url = URL.decode(s"http://localhost:$port/$path").toOption.get

              for {
                res <-
                  Client.batched(
                    Request(method = req.method, headers = req.headers, body = req.body, url = url),
                  )
              } yield res
            },
          Method.ANY / trailing           ->
            handler { (path: Path, req: Request) =>
              val method = req.method

              Response.text(s"Received ${method} query on ${path}")
            },
        ).sandbox

        test("should be able to directly return other request") {
          for {
            body1 <- server.deploy.body
              .run(path = Path.root / "test", method = Method.GET)
              .flatMap(_.asString(Charsets.Utf8))
            body2 <- server.deploy.body
              .run(path = Path.root / "proxy" / "test-proxy", method = Method.GET)
              .flatMap(_.asString(Charsets.Utf8))
          } yield assertTrue(body1 == "Received GET query on /test", body2 == "Received GET query on /test-proxy")
        }
      },
  )

  def requestSpec = suite("RequestSpec") {
    val routes: Routes[Any, Response] =
      Routes
        .singleton(handler { (_: Path, req: Request) =>
          Response.text(req.header(Header.ContentLength).map(_.length).getOrElse(-1).toString)
        })
        .sandbox

    test("has content-length") {
      check(Gen.alphaNumericString) { string =>
        val res = routes.deploy.body.mapZIO(_.asString).run(body = Body.fromString(string))
        assertZIO(res)(equalTo(string.length.toString))
      }
    } +
      test("POST Request.getBody") {
        val app = Routes
          .singleton(handler { (_: Path, req: Request) => req.body.asChunk.as(Response.ok) })
          .sandbox

        val res = app.deploy.status.run(path = Path.root, method = Method.POST, body = Body.fromString("some text"))
        assertZIO(res)(equalTo(Status.Ok))
      } +
      test("body can be read multiple times") {
        val app = Routes
          .singleton(handler { (_: Path, req: Request) =>
            (req.body.asChunk *> req.body.asChunk).as(Response.ok)
          })
          .sandbox

        val res = app.deploy.status.run(method = Method.POST, body = Body.fromString("some text"))
        assertZIO(res)(equalTo(Status.Ok))
      }
  }

  def responseSpec = suite("ResponseSpec")(
    test("data") {
      check(nonEmptyContent) { case (string, data) =>
        val res = Handler.fromBody(data).toRoutes.deploy.body.mapZIO(_.asString).run()
        assertZIO(res)(equalTo(string))
      }
    },
    test("data from file") {
      val res = Handler.fromResource("TestFile.txt").sandbox.toRoutes.deploy.body.mapZIO(_.asString).run()
      assertZIO(res)(equalTo("foo\nbar"))
    },
    test("content-type header on file response") {
      val res =
        Handler
          .fromResource("TestFile2.mp4")
          .sandbox
          .toRoutes
          .deploy
          .header(Header.ContentType)
          .run()
          .map(_.map(_.mediaType.fullType).getOrElse("Content type header not found."))
      assertZIO(res)(equalTo("video/mp4"))
    },
    test("status") {
      checkAll(HttpGen.status) { case status =>
        val res = Handler.status(status).toRoutes.deploy.status.run()
        assertZIO(res)(equalTo(status))
      }

    },
    test("header") {
      check(HttpGen.header) { header =>
        val res = Handler.ok.addHeader(header).toRoutes.deploy.rawHeader(header.headerName).run()
        assertZIO(res)(isSome(equalTo(header.renderedValue)))
      }
    },
    test("text streaming") {
      val res = Handler.fromStreamChunked(ZStream("a", "b", "c")).sandbox.toRoutes.deploy.body.mapZIO(_.asString).run()
      assertZIO(res)(equalTo("abc"))
    },
    test("echo streaming") {
      val res = Routes
        .singleton(handler { (_: Path, req: Request) =>
          Handler.fromStreamChunked(ZStream.fromZIO(req.body.asChunk).flattenChunks): Handler[
            Any,
            Throwable,
            (Path, Request),
            Response,
          ]
        })
        .sandbox
        .deploy
        .body
        .mapZIO(_.asString)
        .run(body = Body.fromString("abc"))
      assertZIO(res)(equalTo("abc"))
    },
    test("file-streaming") {
      val path = getClass.getResource("/TestFile.txt").getPath
      val res  =
        Handler
          .fromStreamChunked(ZStream.fromPath(Paths.get(path)))
          .sandbox
          .toRoutes
          .deploy
          .body
          .mapZIO(_.asString)
          .run()
      assertZIO(res)(equalTo("foo\nbar"))
    } @@ TestAspect.os(os => !os.isWindows),
    test("streaming failure - known content type") {
      val res =
        Handler
          .fromStream(ZStream.fromZIO(ZIO.attempt(throw new Exception("boom"))), 42)
          .sandbox
          .toRoutes
          .deploy
          .body
          .mapZIO(_.asString)
          .run()
          .exit
      assertZIO(res)(fails(anything))
    } @@ TestAspect.timeout(10.seconds),
    test("streaming failure - unknown content type") {
      val res =
        Handler
          .fromStreamChunked(ZStream.fromZIO(ZIO.attempt(throw new Exception("boom"))))
          .sandbox
          .toRoutes
          .deploy
          .body
          .mapZIO(_.asString)
          .run()
          .exit
      assertZIO(res)(fails(anything))
    } @@ TestAspect.timeout(10.seconds),
    suite("html")(
      test("body") {
        val res =
          Handler
            .html(zio.http.template.html(body(div(id := "foo", "bar"))))
            .sandbox
            .toRoutes
            .deploy
            .body
            .mapZIO(_.asString)
            .run()
        assertZIO(res)(equalTo("""<!DOCTYPE html><html><body><div id="foo">bar</div></body></html>"""))
      },
      test("content-type") {
        val app = Handler.html(zio.http.template.html(body(div(id := "foo", "bar")))).sandbox
        val res = app.toRoutes.deploy.header(Header.ContentType).run()
        assertZIO(res)(isSome(equalTo(Header.ContentType(MediaType.text.html))))
      },
    ),
    suite("content-length")(
      suite("string") {
        test("unicode text") {
          val res = Handler.text("äöü").sandbox.toRoutes.deploy.contentLength.run()
          assertZIO(res)(isSome(equalTo(Header.ContentLength(6L))))
        } +
          test("provided content-length is overwritten by actual length") {
            val res =
              Handler
                .text("1234567890")
                .addHeader(Header.ContentLength(4L))
                .sandbox
                .toRoutes
                .deploy
                .contentLength
                .run()
            assertZIO(res)(isSome(equalTo(Header.ContentLength(10L))))
          } +
          test("provided content-length is used for HEAD requests") {
            val res =
              Handler.ok
                .addHeader(Header.ContentLength(4L))
                .sandbox
                .toRoutes
                .deploy
                .contentLength
                .run(method = Method.HEAD)
            assertZIO(res)(isSome(equalTo(Header.ContentLength(4L))))
          } +
          test("provided content-length is used for HEAD requests with stream body") {
            // NOTE: Unlikely use-case, but just in case some 3rd party integration
            // uses streams as a generalised way to provide content
            val res =
              Handler
                .fromStream(ZStream.empty, 0L)
                .addHeader(Header.ContentLength(4L))
                .sandbox
                .toRoutes
                .deploy
                .contentLength
                .run(method = Method.HEAD)
            assertZIO(res)(isSome(equalTo(Header.ContentLength(4L))))
          }
      },
    ),
    suite("memoize")(
      test("concurrent") {
        val size     = 100
        val expected = (0 to size) map (_ => Status.Ok)
        val response = Response.text("abc")
        for {
          actual <- ZIO.foreachPar(0 to size)(_ => Handler.fromResponse(response).toRoutes.deploy.status.run())
        } yield assertTrue(actual == expected)
      },
      test("update after cache") {
        val server = "ZIO-Http"
        val res    = Response.text("abc")
        for {
          actual <- Handler
            .fromResponse(res)
            .addHeader(Header.Server(server))
            .toRoutes
            .deploy
            .header(Header.Server)
            .run()
        } yield assertTrue(actual.get == Header.Server(server))
      },
    ),
  )

  def requestBodySpec = suite("RequestBodySpec")(
    test("POST Request stream") {
      val routes: Routes[Any, Response] = Routes.singleton {
        handler { (_: Path, req: Request) =>
          Response(body = Body.fromStreamChunked(req.body.asStream))
        }
      }

      check(Gen.alphaNumericString) { c =>
        assertZIO(
          routes.deploy.body.mapZIO(_.asString).run(path = Path.root, method = Method.POST, body = Body.fromString(c)),
        )(
          equalTo(c),
        )
      }
    },
  )

  def serverErrorSpec = suite("ServerErrorSpec") {
    val routes = Handler.fail(new Error("SERVER_ERROR")).sandbox.toRoutes
    test("status is 500") {
      val res = routes.deploy.status.run()
      assertZIO(res)(equalTo(Status.InternalServerError))
    } +
      test("content is empty") {
        val res = routes.deploy.body.mapZIO(_.asString).run()
        assertZIO(res)(isEmptyString)
      } +
      test("header is set") {
        val res = routes.deploy.headers.run().map(_.header(Header.ContentLength))
        assertZIO(res)(isSome(anything))
      } +
      test("should send 100 Continue before 101 Switching Protocols when both Upgrade and Expect headers are present") {
        val continueHandler = Handler.fromZIO {
          ZIO.succeed(Response.status(Status.Continue))
        }

        val switchingProtocolsHandler = Handler.fromZIO {
          ZIO.succeed(
            Response
              .status(Status.SwitchingProtocols)
              .addHeader(Header.Connection.KeepAlive)
              .addHeader(Header.Upgrade.Protocol("https", "1.1")),
          )
        }
        val app                       = Routes(
          Method.POST / "upgrade" -> continueHandler,
          Method.GET / "switch"   -> switchingProtocolsHandler,
        )
        val initialRequest            = Request
          .post("/upgrade", Body.empty)
          .addHeader(Header.Expect.`100-continue`)
          .addHeader(Header.Connection.KeepAlive)
          .addHeader(Header.Upgrade.Protocol("https", "1.1"))

        val followUpRequest = Request.get("/switch")

        for {
          firstResponse  <- app.runZIO(initialRequest)
          secondResponse <- app.runZIO(followUpRequest)

        } yield assertTrue(
          firstResponse.status == Status.Continue,            // Checks first response is 100 Continue
          secondResponse.status == Status.SwitchingProtocols, // Checks second response is 101 Switching Protocols
          secondResponse.headers.contains(Header.Upgrade.name),
          secondResponse.headers.contains(Header.Connection.name),
        )
      } +
      test("should not send body for HEAD requests") {
        val route       = Routes(
          Method.GET / "test"  -> Handler.fromResponse(Response.text("This is the body")),
          Method.HEAD / "test" -> Handler.fromResponse(Response(status = Status.Ok)),
        )
        val app         = route
        val headRequest = Request.head("/test")
        for {
          response <- app.runZIO(headRequest)
        } yield assertTrue(
          response.status == Status.Ok, // Ensure we get a 200 OK status
          response.body.isEmpty,        // Ensure no body is sent for HEAD request
        )
      } +
      test("should not include Content-Length header for 2XX CONNECT responses") {
        val app = Routes(
          Method.CONNECT / "" -> Handler.fromResponse(
            Response.status(Status.Ok),
          ),
        )

        val decodedUrl = URL.decode("https://example.com:443")

        val request = decodedUrl match {
          case Right(url) => Request(method = Method.CONNECT, url = url)
          case Left(_)    => throw new RuntimeException("Failed to decode the URL") // Handle URL decoding failure
        }

        for {
          response <- app.runZIO(request)
        } yield assertTrue(
          response.status == Status.Ok,
          !response.headers.contains(Header.ContentLength.name),
        )
      } +
      test("should send Upgrade header with 426 Upgrade Required response") {
        val app = Routes(
          Method.GET / "test" -> Handler.fromResponse(
            Response
              .status(Status.UpgradeRequired)
              .addHeader(Header.Upgrade.Protocol("https", "1.1")),
          ),
        )

        val request = Request.get("/test")

        for {
          response <- app.runZIO(request)
        } yield assertTrue(
          response.status == Status.UpgradeRequired,
          response.headers.contains(Header.Upgrade.name),
        )
      } +
      test("should send Upgrade header with 101 Switching Protocols response") {
        val app = Routes(
          Method.GET / "switch" -> Handler.fromResponse(
            Response
              .status(Status.SwitchingProtocols)
              .addHeader(Header.Upgrade.Protocol("https", "1.1")),
          ),
        )

        val request = Request.get("/switch")

        for {
          response <- app.runZIO(request)
        } yield assertTrue(
          response.status == Status.SwitchingProtocols,
          response.headers.contains(Header.Upgrade.name),
        )
      } +
      test("should not switch to a protocol not indicated by the client in the Upgrade header") {
        val app = Routes(
          Method.GET / "switch" -> Handler.fromFunctionZIO { (request: Request) =>
            val clientUpgrade = request.headers.get(Header.Upgrade.name)

            ZIO.succeed {
              clientUpgrade match {
                case Some("https/1.1") =>
                  Response
                    .status(Status.SwitchingProtocols)
                    .addHeader(Header.Upgrade.Protocol("https", "1.1"))
                case Some(_)           =>
                  Response.status(Status.BadRequest)
                case None              =>
                  Response.status(Status.Ok)
              }
            }
          },
        )

        val requestWithUpgrade = Request
          .get("/switch")
          .addHeader(Header.Upgrade.Protocol("https", "1.1"))

        val requestWithUnsupportedUpgrade = Request
          .get("/switch")
          .addHeader(Header.Upgrade.Protocol("unsupported", "1.0"))

        val requestWithoutUpgrade = Request.get("/switch")

        for {
          responseWithUpgrade            <- app.runZIO(requestWithUpgrade)
          responseWithUnsupportedUpgrade <- app.runZIO(requestWithUnsupportedUpgrade)
          responseWithoutUpgrade         <- app.runZIO(requestWithoutUpgrade)

        } yield assertTrue(
          responseWithUpgrade.status == Status.SwitchingProtocols,
          responseWithUpgrade.headers.contains(Header.Upgrade.name),
          responseWithUnsupportedUpgrade.status == Status.BadRequest,
          responseWithoutUpgrade.status == Status.Ok,
        )
      } +
      test("should not send body for 204 No Content responses") {
        val app = Routes(
          Method.GET / "no-content" -> Handler.fromResponse(
            Response.status(Status.NoContent),
          ),
        )

        val request = Request.get("/no-content")

        for {
          response <- app.runZIO(request)
        } yield assertTrue(
          response.status == Status.NoContent,
          response.body.isEmpty,
        )
      } +
      test("should not send body for 205 Reset Content responses") {
        val app = Routes(
          Method.GET / "reset-content" -> Handler.fromResponse(
            Response.status(Status.ResetContent),
          ),
        )

        val request = Request.get("/reset-content")

        for {
          response <- app.runZIO(request)
        } yield assertTrue(response.status == Status.ResetContent, response.body.isEmpty)
      } +
      test("should not generate a bare CR in headers for HTTP/1.1") {
        val app = Routes(
          Method.GET / "test" -> Handler.fromZIO {
            ZIO.succeed(
              Response
                .status(Status.Ok)
                .addHeader(Header.Custom("A", "1\r\nB: 2")),
            )
          },
        )

        val request = Request
          .get("/test")
          .copy(version = Version.Http_1_1)

        for {
          response <- app.runZIO(request)
          headersString = response.headers.toString
          isValid       = !headersString.contains("\r") || headersString.contains("\r\n")
        } yield assertTrue(isValid)
      } +
      test("should return 400 Bad Request if Host header is missing") {
        val route              = Method.GET / "test" -> Handler.ok
        val app                = Routes(route)
        val requestWithoutHost = Request.get("/test")

        for {
          response <- app.runZIO(requestWithoutHost)
        } yield assertTrue(response.status == Status.BadRequest)
      } +
      test("should return 200 OK if Host header is present") {
        val route           = Method.GET / "test" -> Handler.ok
        val app             = Routes(route)
        val requestWithHost = Request.get("/test").addHeader(Header.Host("localhost"))
        for {
          response <- app.runZIO(requestWithHost)
        } yield assertTrue(response.status == Status.Ok)
      } +
      test("should return 400 Bad Request if header contains CR, LF, or NULL") {
        val route = Method.GET / "test" -> Handler.ok
        val app   = Routes(route)

        // Crafting a request with invalid headers containing CR, LF, and NULL
        val requestWithCRLFHeader = Request.get("/test").addHeader("InvalidHeader", "Value\r\n")
        val requestWithNullHeader = Request.get("/test").addHeader("InvalidHeader", "Value\u0000")

        for {
          responseCRLF <- app.runZIO(requestWithCRLFHeader)
          responseNull <- app.runZIO(requestWithNullHeader)
        } yield {
          assertTrue(responseCRLF.status == Status.BadRequest) &&
          assertTrue(responseNull.status == Status.BadRequest)
        }
      } +
      test("should return 400 Bad Request if there is whitespace between start-line and first header field") {
        val route = Method.GET / "test" -> Handler.ok
        val app   = Routes(route)

        // Create a malformed request with extra whitespace between start-line and first header field
        val malformedRequest =
          Request.get("/test").copy(headers = Headers.empty).withBody(Body.fromString("\r\nHost: localhost"))

        for {
          response <- app.runZIO(malformedRequest)
        } yield assertTrue(response.status == Status.BadRequest)
      } +
      test("should return 400 Bad Request if there is whitespace between header field and colon") {
        val route = Method.GET / "test" -> Handler.ok
        val app   = Routes(route)

        // Crafting a request with a whitespace between the header field name and the colon
        // val requestWithWhitespaceHeader = Request.get("/test").addHeader("Invalid Header : value")
        val requestWithWhitespaceHeader = Request.get("/test").addHeader(Header.Custom("Invalid Header ", "value"))

        for {
          response <- app.runZIO(requestWithWhitespaceHeader)
        } yield {
          assertTrue(response.status == Status.BadRequest) // Expecting a 400 Bad Request
        }
      } +
      test("should not include Content-Length header for 1xx and 204 No Content responses") {
        // Defining routes for different status codes
        val route1xxContinue = Method.GET / "continue" -> Handler.fromResponse(Response(status = Status.Continue))
        val route1xxSwitch   =
          Method.GET / "switching-protocols" -> Handler.fromResponse(Response(status = Status.SwitchingProtocols))
        val route1xxProcess   = Method.GET / "processing" -> Handler.fromResponse(Response(status = Status.Processing))
        val route204NoContent = Method.GET / "no-content" -> Handler.fromResponse(Response(status = Status.NoContent))

        // Combining routes into a single application
        val app = Routes(route1xxContinue, route1xxSwitch, route1xxProcess, route204NoContent)

        // Creating corresponding requests
        val requestContinue  = Request.get("/continue")
        val requestSwitch    = Request.get("/switching-protocols")
        val requestProcess   = Request.get("/processing")
        val requestNoContent = Request.get("/no-content")

        for {
          // Executing requests
          responseContinue  <- app.runZIO(requestContinue)
          responseSwitch    <- app.runZIO(requestSwitch)
          responseProcess   <- app.runZIO(requestProcess)
          responseNoContent <- app.runZIO(requestNoContent)

          // Asserting that no responses have a Content-Length header
        } yield assertTrue(
          !responseContinue.headers.contains(Header.ContentLength.name),
          !responseSwitch.headers.contains(Header.ContentLength.name),
          !responseProcess.headers.contains(Header.ContentLength.name),
          !responseNoContent.headers.contains(Header.ContentLength.name),
        )
      } +
      test("should not include Content-Length header for 204 No Content responses") {
        val route = Method.GET / "no-content" -> Handler.fromResponse(Response(status = Status.NoContent))
        val app   = Routes(route)

        val request = Request.get("/no-content")
        for {
          response <- app.runZIO(request)
        } yield assertTrue(!response.headers.contains(Header.ContentLength.name))
      }
  }

  override def spec =
    suite("ServerSpec") {
      val spec = dynamicAppSpec + responseSpec + requestSpec + requestBodySpec + serverErrorSpec
      suite("app without request streaming") { app.as(List(spec)) }
    }.provideShared(
      DynamicServer.live,
      ZLayer.succeed(configApp),
      Server.customized,
      ZLayer.succeed(NettyConfig.defaultWithFastShutdown),
      Client.default,
    ) @@ sequential @@ withLiveClock

}
