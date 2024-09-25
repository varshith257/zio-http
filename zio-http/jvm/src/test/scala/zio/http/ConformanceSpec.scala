package zio.http

import zio._
import zio.test.Assertion._
import zio.test.TestAspect._
import zio.test._

import zio.http._
import zio.http.Header._

object ConformanceSpec extends ZIOHttpSpec {

  override def spec =
    suite("ConformanceSpec")(
      suite("Statuscodes")(
        test("should not send body for 204 No Content responses(code_204_no_additional_content)") {
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
        },
        test("should not send body for 205 Reset Content responses(code_205_no_content_allowed)") {
          val app = Routes(
            Method.GET / "reset-content" -> Handler.fromResponse(
              Response.status(Status.ResetContent),
            ),
          )

          val request = Request.get("/reset-content")

          for {
            response <- app.runZIO(request)
          } yield assertTrue(response.status == Status.ResetContent, response.body.isEmpty)
        },
        test("should include Content-Range for 206 Partial Content response(code_206_content_range)") {
          val app = Routes(
            Method.GET / "partial" -> Handler.fromResponse(
              Response
                .status(Status.PartialContent)
                .addHeader(Header.ContentRange.StartEnd("bytes", 0, 14)),
            ),
          )

          val request = Request.get("/partial")

          for {
            response <- app.runZIO(request)
          } yield assertTrue(
            response.status == Status.PartialContent,
            response.headers.contains(Header.ContentRange.name),
          )
        },
        test(
          "should not include Content-Range in header for multipart/byteranges response(code_206_content_range_of_multiple_part_response)",
        ) {
          val boundary = zio.http.Boundary("A12345")

          val app = Routes(
            Method.GET / "partial" -> Handler.fromResponse(
              Response
                .status(Status.PartialContent)
                .addHeader(Header.ContentType(MediaType("multipart", "byteranges"), Some(boundary))),
            ),
          )

          val request = Request.get("/partial")

          for {
            response <- app.runZIO(request)
          } yield assertTrue(
            response.status == Status.PartialContent,
            !response.headers.contains(Header.ContentRange.name),
            response.headers.contains(Header.ContentType.name),
          )
        },
        test("should include necessary headers in 206 Partial Content response(code_206_headers)") {
          val app = Routes(
            Method.GET / "partial" -> Handler.fromResponse(
              Response
                .status(Status.PartialContent)
                .addHeader(Header.ETag.Strong("abc"))
                .addHeader(Header.CacheControl.MaxAge(3600)),
            ),
            Method.GET / "full"    -> Handler.fromResponse(
              Response
                .status(Status.Ok)
                .addHeader(Header.ETag.Strong("abc"))
                .addHeader(Header.CacheControl.MaxAge(3600)),
            ),
          )

          val requestWithRange    =
            Request.get("/partial").addHeader(Header.Range.Single("bytes", 0, Some(14)))
          val requestWithoutRange = Request.get("/full")

          for {
            responseWithRange    <- app.runZIO(requestWithRange)
            responseWithoutRange <- app.runZIO(requestWithoutRange)
          } yield assertTrue(
            responseWithRange.status == Status.PartialContent,
            responseWithRange.headers.contains(Header.ETag.name),
            responseWithRange.headers.contains(Header.CacheControl.name),
            responseWithoutRange.status == Status.Ok,
          )
        },
        test("should include WWW-Authenticate header for 401 Unauthorized response(code_401_www_authenticate)") {
          val app = Routes(
            Method.GET / "unauthorized" -> Handler.fromResponse(
              Response
                .status(Status.Unauthorized)
                .addHeader(Header.WWWAuthenticate.Basic(Some("simple"))),
            ),
          )

          val request = Request.get("/unauthorized")

          for {
            response <- app.runZIO(request)
          } yield assertTrue(
            response.status == Status.Unauthorized,
            response.headers.contains(Header.WWWAuthenticate.name),
          )
        },
        test("should include Allow header for 405 Method Not Allowed response(code_405_allow)") {
          val app = Routes(
            Method.POST / "not-allowed" -> Handler.fromResponse(
              Response
                .status(Status.Ok),
            ),
          )

          val request = Request.get("/not-allowed")

          for {
            response <- app.runZIO(request)
          } yield assertTrue(
            response.status == Status.MethodNotAllowed,
            response.headers.contains(Header.Allow.name),
          )
        },
        test(
          "should include Proxy-Authenticate header for 407 Proxy Authentication Required response(code_407_proxy_authenticate)",
        ) {
          val app = Routes(
            Method.GET / "proxy-auth" -> Handler.fromResponse(
              Response
                .status(Status.ProxyAuthenticationRequired)
                .addHeader(
                  Header.ProxyAuthenticate(AuthenticationScheme.Basic, Some("proxy")),
                ),
            ),
          )

          val request = Request.get("/proxy-auth")

          for {
            response <- app.runZIO(request)
          } yield assertTrue(
            response.status == Status.ProxyAuthenticationRequired,
            response.headers.contains(Header.ProxyAuthenticate.name),
          )
        },
        test("should return 304 without content(code_304_no_content)") {
          val app = Routes(
            Method.GET / "no-content" -> Handler.fromResponse(
              Response
                .status(Status.NotModified)
                .copy(body = Body.empty),
            ),
          )

          val request = Request.get("/no-content")

          for {
            response <- app.runZIO(request)
          } yield assertTrue(
            response.status == Status.NotModified,
            response.body.isEmpty,
          )
        },
        test("should return 304 with correct headers(code_304_headers)") {
          val headers = Headers(
            Header.ETag.Strong("abc"),
            Header.CacheControl.MaxAge(3600),
            Header.Vary("Accept-Encoding"),
          )

          val app = Routes(
            Method.GET / "with-headers" -> Handler.fromResponse(
              Response
                .status(Status.NotModified)
                .addHeaders(headers),
            ),
          )

          val request = Request.get("/with-headers")

          for {
            response <- app.runZIO(request)
          } yield assertTrue(
            response.status == Status.NotModified,
            response.headers.contains(Header.ETag.name),
            response.headers.contains(Header.CacheControl.name),
            response.headers.contains(Header.Vary.name),
          )
        },
      ),
      suite("HTTP Headers")(
        suite("code_400_after_bad_host_request")(
          test("should return 400 Bad Request if Host header is missing") {
            val route              = Method.GET / "test" -> Handler.ok
            val app                = Routes(route)
            val requestWithoutHost = Request.get("/test")

            for {
              response <- app.runZIO(requestWithoutHost)
            } yield assertTrue(response.status == Status.BadRequest)
          },
          test("should return 400 Bad Request if there are multiple Host headers") {
            val route               = Method.GET / "test" -> Handler.ok
            val app                 = Routes(route)
            val requestWithTwoHosts = Request
              .get("/test")
              .addHeader(Header.Host("example.com"))
              .addHeader(Header.Host("another.com"))

            for {
              response <- app.runZIO(requestWithTwoHosts)
            } yield assertTrue(response.status == Status.BadRequest)
          },
          test("should return 400 Bad Request if Host header is invalid") {
            val route                  = Method.GET / "test" -> Handler.ok
            val app                    = Routes(route)
            val requestWithInvalidHost = Request
              .get("/test")
              .addHeader(Header.Host("invalid_host"))

            for {
              response <- app.runZIO(requestWithInvalidHost)
            } yield assertTrue(response.status == Status.BadRequest)
          },
        ),
        test("should not include Content-Length header for 2XX CONNECT responses(content_length_2XX_connect)") {
          val app = Routes(
            Method.CONNECT / "" -> Handler.fromResponse(
              Response.status(Status.Ok),
            ),
          )

          val decodedUrl = URL.decode("https://example.com:443")

          val request = decodedUrl match {
            case Right(url) => Request(method = Method.CONNECT, url = url)
            case Left(_)    => throw new RuntimeException("Failed to decode the URL")
          }

          for {
            response <- app.runZIO(request)
          } yield assertTrue(
            response.status == Status.Ok,
            !response.headers.contains(Header.ContentLength.name),
          )
        },
        test("should not include Transfer-Encoding header for 2XX CONNECT responses(transfer_encoding_2XX_connect)") {
          val app = Routes(
            Method.CONNECT / "" -> Handler.fromResponse(
              Response.status(Status.Ok),
            ),
          )

          val decodedUrl = URL.decode("https://example.com:443")

          val request = decodedUrl match {
            case Right(url) => Request(method = Method.CONNECT, url = url)
            case Left(_)    => throw new RuntimeException("Failed to decode the URL")
          }

          for {
            response <- app.runZIO(request)
          } yield assertTrue(
            response.status == Status.Ok,
            !response.headers.contains(Header.TransferEncoding.name),
          )
        },
        suite("sts")(
          // TODO: Strict-Transport-Security Header to be Added
          // suite("sts_directives_only_allowed_once")(
          //   test("should return valid if each directive appears only once in STS header") {
          //     val app = Routes(
          //       Method.GET / "secure" -> Handler.fromResponse(
          //         Response.ok.addHeader(Header.StrictTransportSecurity("max-age=31536000; includeSubDomains")),
          //       ),
          //     )

          //     // val request = Request.get("/secure").withSecure(true)
          //     val request = Request.get(URL.decode("https://localhost/secure").toOption.get)

          //     for {
          //       response <- app.runZIO(request)
          //     } yield assertTrue(response.status == Status.Ok) &&
          //       assertTrue(response.headers.contains(Header.StrictTransportSecurity.name))
          //   },
          //   test("should return invalid if STS header contains duplicate directives") {
          //     val app     = Routes(
          //       Method.GET / "secure" -> Handler.fromResponse(
          //         Response.ok.addHeader(Header.StrictTransportSecurity("max-age=31536000; max-age=31536000")),
          //       ),
          //     )
          //     val request = Request.get(URL.decode("http://localhost/non-secure").toOption.get)

          //     // val decodedUrl = URL.decode("https://example.com:443")

          //     for {
          //       response <- app.runZIO(request)
          //     } yield assertTrue(response.status == Status.Ok) &&
          //       assertTrue(response.headers.contains(Header.StrictTransportSecurity.name))
          //   },
          // ),
          // suite("only_one_sts_header_allowed")(
          //   test("should return valid if only one STS header is present") {
          //     val app = Routes(
          //       Method.GET / "secure" -> Handler.fromResponse(
          //         Response.ok.addHeader(Header.StrictTransportSecurity("max-age=31536000")),
          //       ),
          //     )

          //     val request = Request.get(URL.decode("https://localhost/secure").toOption.get)

          //     for {
          //       response <- app.runZIO(request)
          //     } yield assertTrue(response.status == Status.Ok) &&
          //       assertTrue(response.headers.count(Header.StrictTransportSecurity.name) == 1)
          //   },
          //   test("should return invalid if more than one STS header is present") {
          //     val app = Routes(
          //       Method.GET / "secure" -> Handler.fromResponse(
          //         Response.ok
          //           .addHeader(Header.StrictTransportSecurity("max-age=31536000"))
          //           .addHeader(Header.StrictTransportSecurity("max-age=31536000")),
          //       ),
          //     )

          //     val request = Request.get(URL.decode("https://localhost/secure").toOption.get)
          //     // val request = Request.get("/secure").withSecure(true)

          //     for {
          //       response <- app.runZIO(request)
          //     } yield assertTrue(response.status == Status.Ok) &&
          //       assertTrue(response.headers.count(Header.StrictTransportSecurity.name) > 1)
          //   },
          // ),
          // suite("sts_header_http")(
          //   test("should not include STS header in HTTP response") {
          //     val app = Routes(
          //       Method.GET / "non-secure" -> Handler.fromResponse(
          //         Response.ok.addHeader(Header.StrictTransportSecurity("max-age=31536000")),
          //       ),
          //     )

          //     // val request = Request.get("/non-secure").withSecure(false)
          //     val request = Request.get(URL.decode("http://localhost/non-secure").toOption.get)

          //     for {
          //       response <- app.runZIO(request)
          //     } yield assertTrue(response.status == Status.Ok) &&
          //       assertTrue(!response.headers.contains(Header.StrictTransportSecurity.name))
          //   },
          //   test("should include STS header in HTTPS response") {
          //     val app = Routes(
          //       Method.GET / "secure" -> Handler.fromResponse(
          //         Response.ok.addHeader(Header.StrictTransportSecurity("max-age=31536000")),
          //       ),
          //     )

          //     val request = Request.get(URL.decode("https://localhost/secure").toOption.get)
          //     // val request = Request.get("/secure").withSecure(true)

          //     for {
          //       response <- app.runZIO(request)
          //     } yield assertTrue(response.status == Status.Ok) &&
          //       assertTrue(response.headers.contains(Header.StrictTransportSecurity.name))
          //   },
          // ),
          // suite("sts-maxage")(
          // ),
          suite("Transfer-Encoding")(
            suite("no_transfer_encoding_1xx_204")(
              test("should return valid when Transfer-Encoding is not present for 1xx or 204 status") {
                val app = Routes(
                  Method.GET / "no-content" -> Handler.fromResponse(
                    Response.status(Status.NoContent),
                  ),
                  Method.GET / "continue"   -> Handler.fromResponse(
                    Response.status(Status.Continue),
                  ),
                )
                for {
                  responseNoContent <- app.runZIO(Request.get("/no-content"))
                  responseContinue  <- app.runZIO(Request.get("/continue"))
                } yield assertTrue(responseNoContent.status == Status.NoContent) &&
                  assertTrue(!responseNoContent.headers.contains(Header.TransferEncoding.name)) &&
                  assertTrue(responseContinue.status == Status.Continue) &&
                  assertTrue(!responseContinue.headers.contains(Header.TransferEncoding.name))
              },
              test("should return invalid when Transfer-Encoding is present for 1xx or 204 status") {
                val app = Routes(
                  Method.GET / "no-content" -> Handler.fromResponse(
                    Response.status(Status.NoContent).addHeader(Header.TransferEncoding.Chunked),
                  ),
                  Method.GET / "continue"   -> Handler.fromResponse(
                    Response.status(Status.Continue).addHeader(Header.TransferEncoding.Chunked),
                  ),
                )

                for {
                  responseNoContent <- app.runZIO(Request.get("/no-content"))
                  responseContinue  <- app.runZIO(Request.get("/continue"))
                } yield assertTrue(responseNoContent.status == Status.NoContent) &&
                  assertTrue(responseNoContent.headers.contains(Header.TransferEncoding.name)) &&
                  assertTrue(responseContinue.status == Status.Continue) &&
                  assertTrue(responseContinue.headers.contains(Header.TransferEncoding.name))
              },
            ),
            suite("transfer_encoding_http11")(
              test("should not send Transfer-Encoding in response if request HTTP version is below 1.1") {
                val app = Routes(
                  Method.GET / "test" -> Handler.fromResponse(
                    Response.ok.addHeader(Header.TransferEncoding.Chunked),
                  ),
                )

                val request = Request.get("/test").copy(version = Version.`HTTP/1.0`)

                for {
                  response <- app.runZIO(request)
                } yield assertTrue(
                  response.status == Status.Ok,
                  !response.headers.contains(Header.TransferEncoding.name),
                )
              },
              test("should send Transfer-Encoding in response if request HTTP version is 1.1 or higher") {
                val app = Routes(
                  Method.GET / "test" -> Handler.fromResponse(
                    Response.ok.addHeader(Header.TransferEncoding.Chunked),
                  ),
                )

                val request = Request.get("/test").copy(version = Version.`HTTP/1.1`)

                for {
                  response <- app.runZIO(request)
                } yield assertTrue(
                  response.status == Status.Ok,
                  response.headers.contains(Header.TransferEncoding.name),
                )
              },
            ),
          ),
        ),
      ),
      suite("conformance")(
        test(
          "should send 100 Continue before 101 Switching Protocols when both Upgrade and Expect headers are present",
        ) {
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
        },
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
        },
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
        },
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
        },
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
        },
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
        },
        test("should return 200 OK if Host header is present") {
          val route           = Method.GET / "test" -> Handler.ok
          val app             = Routes(route)
          val requestWithHost = Request.get("/test").addHeader(Header.Host("localhost"))
          for {
            response <- app.runZIO(requestWithHost)
          } yield assertTrue(response.status == Status.Ok)
        },
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
        },
        test("should return 400 Bad Request if there is whitespace between start-line and first header field") {
          val route = Method.GET / "test" -> Handler.ok
          val app   = Routes(route)

          // Create a malformed request with extra whitespace between start-line and first header field
          val malformedRequest =
            Request.get("/test").copy(headers = Headers.empty).withBody(Body.fromString("\r\nHost: localhost"))

          for {
            response <- app.runZIO(malformedRequest)
          } yield assertTrue(response.status == Status.BadRequest)
        },
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
        },
        test("should not include Content-Length header for 1xx and 204 No Content responses") {
          // Defining routes for different status codes
          val route1xxContinue = Method.GET / "continue" -> Handler.fromResponse(Response(status = Status.Continue))
          val route1xxSwitch   =
            Method.GET / "switching-protocols" -> Handler.fromResponse(Response(status = Status.SwitchingProtocols))
          val route1xxProcess = Method.GET / "processing" -> Handler.fromResponse(Response(status = Status.Processing))
          val route204NoContent = Method.GET / "no-content" -> Handler.fromResponse(Response(status = Status.NoContent))

          val app = Routes(route1xxContinue, route1xxSwitch, route1xxProcess, route204NoContent)

          val requestContinue  = Request.get("/continue")
          val requestSwitch    = Request.get("/switching-protocols")
          val requestProcess   = Request.get("/processing")
          val requestNoContent = Request.get("/no-content")

          for {
            responseContinue  <- app.runZIO(requestContinue)
            responseSwitch    <- app.runZIO(requestSwitch)
            responseProcess   <- app.runZIO(requestProcess)
            responseNoContent <- app.runZIO(requestNoContent)

          } yield assertTrue(
            !responseContinue.headers.contains(Header.ContentLength.name),
            !responseSwitch.headers.contains(Header.ContentLength.name),
            !responseProcess.headers.contains(Header.ContentLength.name),
            !responseNoContent.headers.contains(Header.ContentLength.name),
          )
        },
        test("should not include Content-Length header for 204 No Content responses") {
          val route = Method.GET / "no-content" -> Handler.fromResponse(Response(status = Status.NoContent))
          val app   = Routes(route)

          val request = Request.get("/no-content")
          for {
            response <- app.runZIO(request)
          } yield assertTrue(!response.headers.contains(Header.ContentLength.name))
        },
        test("should not send content for 304 Not Modified responses") {
          val app = Routes(
            Method.GET / "not-modified" -> Handler.fromResponse(
              Response.status(Status.NotModified),
            ),
          )

          val request = Request.get("/not-modified")

          for {
            response <- app.runZIO(request)
          } yield assertTrue(
            response.status == Status.NotModified,
            response.body.isEmpty,
            !response.headers.contains(Header.ContentLength.name),
            !response.headers.contains(Header.TransferEncoding.name),
          )
        },
      ),
    )
}
