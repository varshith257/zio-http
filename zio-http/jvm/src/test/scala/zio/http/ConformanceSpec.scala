package zio.http

import zio._
import zio.test.Assertion._
import zio.test.TestAspect._
import zio.test._

import zio.http.Header._
import zio.http._

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
          test("should return 200 OK if Host header is present") {
            val route           = Method.GET / "test" -> Handler.ok
            val app             = Routes(route)
            val requestWithHost = Request.get("/test").addHeader(Header.Host("localhost"))
            for {
              response <- app.runZIO(requestWithHost)
            } yield assertTrue(response.status == Status.Ok)
          },
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
      ),
      suite("sts")(
        // TODO: Strict-Transport-Security Header to be Added in Header.Scala

      ),
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
      suite("HTTP-Methods")(
        test("should not send body for HEAD requests(content_head_request)") {
          val route       = Routes(
            Method.GET / "test"  -> Handler.fromResponse(Response.text("This is the body")),
            Method.HEAD / "test" -> Handler.fromResponse(Response(status = Status.Ok)),
          )
          val app         = route
          val headRequest = Request.head("/test")
          for {
            response <- app.runZIO(headRequest)
          } yield assertTrue(
            response.status == Status.Ok,
            response.body.isEmpty,
          )
        },
        test("should not return 206, 304, or 416 status codes for POST requests(post_invalid_response_codes)") {

          val app = Routes(
            Method.POST / "test" -> Handler.fromResponse(Response.status(Status.Ok)),
          )

          for {
            res <- app.runZIO(Request.post("/test", Body.empty))

          } yield assertTrue(
            res.status != Status.PartialContent,
            res.status != Status.NotModified,
            res.status != Status.RequestedRangeNotSatisfiable,
            res.status == Status.Ok,
          )
        },
      ),
      suite("HTTP/1.1")(
        test("should return 400 Bad Request if there is whitespace between start-line and first header field") {
          val route = Method.GET / "test" -> Handler.ok
          val app   = Routes(route)

          val malformedRequest =
            Request.get("/test").copy(headers = Headers.empty).withBody(Body.fromString("\r\nHost: localhost"))

          for {
            response <- app.runZIO(malformedRequest)
          } yield assertTrue(response.status == Status.BadRequest)
        },
        test("should return 400 Bad Request if there is whitespace between header field and colon") {
          val route = Method.GET / "test" -> Handler.ok
          val app   = Routes(route)

          val requestWithWhitespaceHeader = Request.get("/test").addHeader(Header.Custom("Invalid Header ", "value"))

          for {
            response <- app.runZIO(requestWithWhitespaceHeader)
          } yield {
            assertTrue(response.status == Status.BadRequest)
          }
        },
        test("should not generate a bare CR in headers for HTTP/1.1(no_bare_cr)") {
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
      ),
      suite("HTTP")(
        test("should return 400 Bad Request if header contains CR, LF, or NULL(reject_fields_contaning_cr_lf_nul)") {
          val route = Method.GET / "test" -> Handler.ok
          val app   = Routes(route)

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
        test("should send Upgrade header with 426 Upgrade Required response(send_upgrade_426)") {
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
        test("should send Upgrade header with 101 Switching Protocols response(send_upgrade_101)") {
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
        test("should not include Content-Length header for 1xx and 204 No Content responses(content_length_1XX_204)") {
          val route1xxContinue = Method.GET / "continue" -> Handler.fromResponse(Response(status = Status.Continue))
          val route1xxSwitch   =
            Method.GET / "switching-protocols" -> Handler.fromResponse(Response(status = Status.SwitchingProtocols))
          val route1xxProcess =
            Method.GET / "processing" -> Handler.fromResponse(Response(status = Status.Processing))
          val route204NoContent =
            Method.GET / "no-content" -> Handler.fromResponse(Response(status = Status.NoContent))

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
        test(
          "should not switch to a protocol not indicated by the client in the Upgrade header(switch_protocol_without_client)",
        ) {
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
        test(
          "should send 100 Continue before 101 Switching Protocols when both Upgrade and Expect headers are present(continue_before_upgrade)",
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
            firstResponse.status == Status.Continue,
            secondResponse.status == Status.SwitchingProtocols,
            secondResponse.headers.contains(Header.Upgrade.name),
            secondResponse.headers.contains(Header.Connection.name),
          )
        },
        test("should not return forbidden duplicate headers in response(duplicate_fields)") {
          val app = Routes(
            Method.GET / "test" -> Handler.fromResponse(
              Response
                .status(Status.Ok)
                .addHeader(Header.XFrameOptions.Deny)
                .addHeader(Header.XFrameOptions.SameOrigin),
            ),
          )
          for {
            response <- app.runZIO(Request.get("/test"))
          } yield assertTrue(
            response.headers.values(Header.XFrameOptions.name).length == 1,
          )
        },
      ),
      suite("cache-control")(
        test("Cache-Control should not have quoted string for max-age directive(response_directive_max_age)") {
          val validResponse = Response
            .status(Status.Ok)
            .addHeader(Header.CacheControl.MaxAge(5))

          val invalidResponse = Response
            .status(Status.Ok)
            .addHeader(Header.Custom("Cache-Control", """max-age="5""""))

          val app = Routes(
            Method.GET / "valid"   -> Handler.fromResponse(validResponse),
            Method.GET / "invalid" -> Handler.fromResponse(invalidResponse),
          )

          for {
            responseValid   <- app.runZIO(Request.get("/valid"))
            responseInvalid <- app.runZIO(Request.get("/invalid"))
          } yield assertTrue(
            responseValid.headers.get(Header.CacheControl.name).contains("max-age=5"),
            responseInvalid.headers.get(Header.CacheControl.name).contains("""max-age="5""""),
          )
        },
        test("Cache-Control should not have quoted string for s-maxage directive(response_directive_s_maxage)") {
          val validResponse = Response
            .status(Status.Ok)
            .addHeader(Header.CacheControl.SMaxAge(10))

          val invalidResponse = Response
            .status(Status.Ok)
            .addHeader(Header.Custom("Cache-Control", """s-maxage="10""""))

          val app = Routes(
            Method.GET / "valid"   -> Handler.fromResponse(validResponse),
            Method.GET / "invalid" -> Handler.fromResponse(invalidResponse),
          )

          for {
            responseValid   <- app.runZIO(Request.get("/valid"))
            responseInvalid <- app.runZIO(Request.get("/invalid"))
          } yield assertTrue(
            responseValid.headers.get(Header.CacheControl.name).contains("s-maxage=10"),
            responseInvalid.headers.get(Header.CacheControl.name).contains("""s-maxage="10""""),
          )
        },
      ),
      suite("conformance")(
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
