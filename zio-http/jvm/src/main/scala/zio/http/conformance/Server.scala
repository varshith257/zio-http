package zio.http.conformance

import zio._
import zio.http._

object Server extends ZIOAppDefault {
  // Define your routes
  val routes = Http.collect[Request] {
    case Method.GET -> !! / "hello" => Response.text("Hello from ZIO HTTP!")
  }

  // Start the server on port 8080
  def run = Server.serve(routes).provide(Server.default)
}
