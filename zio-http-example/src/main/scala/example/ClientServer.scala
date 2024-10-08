package example

import zio.ZIOAppDefault

import zio.http._

object ClientServer extends ZIOAppDefault {
  val url = URL.decode("http://localhost:8080/hello").toOption.get

  val app = Routes(
    Method.GET / "hello" -> handler(Response.text("hello")),
    Method.GET / ""      -> handler(ZClient.batched(Request.get(url))),
  ).sandbox

  val run =
    Server.serve(app).provide(Server.default, Client.default).exitCode
}
