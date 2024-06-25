package example

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter.ISO_LOCAL_TIME

import zio._

import zio.stream.ZStream

import zio.http._
import zio.http.codec.HttpCodec
import zio.http.endpoint.Endpoint
import zio.http.endpoint.EndpointMiddleware.None

object ServerSentEventEndpoint extends ZIOAppDefault {
  import HttpCodec._

  val stream: ZStream[Any, Nothing, ServerSentEvent] =
    ZStream.repeatWithSchedule(ServerSentEvent(ISO_LOCAL_TIME.format(LocalDateTime.now)), Schedule.spaced(1.second))

  // Define the route that handles SSE
  val app = Routes(
    Method.GET / "sse" -> handler { request =>
      ZIO.succeed(Response(body = ServerSentEventBody(stream, None, MediaType.text.plain)))
    },
  ).toHttpApp

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = {
    Server.serve(routes).provide(Server.default).exitCode
  }

}
