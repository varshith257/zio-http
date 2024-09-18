package zio.http

import zio._
import zio.test._

object HandlerAspectSpec extends ZIOSpecDefault {

  case class WebSession(id: Int)

  // Middleware to add session context
  def maybeWebSession: HandlerAspect[Any, Option[WebSession]] =
    HandlerAspect.interceptIncomingHandler(
      Handler.fromFunctionZIO[Request] { req =>
        ZIO.succeed((req, Some(WebSession(42))))
      },
    )

  override def spec: Spec[TestEnvironment with Scope, Any] =
    suite("HandlerAspect")(
      test("HandlerAspect should correctly combine path params and middleware context") {
        val route = Method.GET / "base" / string("param") -> handler((param: String, req: Request) => {
          withContext((session: Option[WebSession]) => {
            val sessionId = session.map(_.id).getOrElse(-1) // Safe handling of session

            ZIO.succeed(Response.text(s"Param: $param, SessionId: $sessionId"))
          })
        }) @@ maybeWebSession

        for {
          response   <- route(Request.get(URL(Path.empty / "base" / "testParam")))
          bodyString <- response.body.asString
          _          <- ZIO.logInfo(s"Response Body: $bodyString") // Log the final response

        } yield assertTrue(bodyString == "Param: testParam, SessionId: 42")
      },
      test("HandlerAspect with context can eliminate environment type") {
        val handler0 = handler((_: Request) => ZIO.serviceWith[Int](i => Response.text(i.toString))) @@
          HandlerAspect.interceptIncomingHandler(handler((req: Request) => (req, req.headers.size)))
        for {
          response   <- handler0(Request(headers = Headers("accept", "*")))
          bodyString <- response.body.asString
        } yield assertTrue(bodyString == "1")
      },
      // format: off
      test("HandlerAspect with context can eliminate environment type partially") {
        val handlerAspect = HandlerAspect.interceptIncomingHandler(handler((req: Request) => (req, req.headers.size)))
        val handler0 = handler { (_: Request) =>
          withContext((_: Boolean, i: Int) => Response.text(i.toString))
          //leftover type is only needed in Scala 2
          //can't be infix because of Scala 3
        }.@@[Boolean](handlerAspect)
        for {
          response   <- handler0(Request(headers = Headers("accept", "*"))).provideEnvironment(ZEnvironment(true))
          bodyString <- response.body.asString
        } yield assertTrue(bodyString == "1")
      },
      test("HandlerAspect with context can eliminate environment type partially while requiring an additional environment") {
        val handlerAspect: HandlerAspect[String, Int] = HandlerAspect.interceptIncomingHandler {
          handler((req: Request) => withContext((s: String) => (req.withBody(Body.fromString(s)), req.headers.size)))
        }
        val handler0: Handler[Boolean with String, Response, Request, Response] = handler { (r: Request) =>
          ZIO.service[Boolean] *> withContext{ (i: Int) =>
            for {
              body <- r.body.asString.orDie
            } yield Response.text(s"$i $body")
          }
          //leftover type is only needed in Scala 2
          //can't be infix because of Scala 3
        }.@@[Boolean](handlerAspect)
        for {
          response   <- handler0(Request(headers = Headers("accept", "*"))).provideEnvironment(ZEnvironment(true) ++ ZEnvironment("test"))
          bodyString <- response.body.asString
        } yield assertTrue(bodyString == "1 test")
      },
      // format: on
    )
}
