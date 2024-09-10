/*
 * Copyright Sporta Technologies PVT LTD & the ZIO HTTP contributors.
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

import zio._

import zio.test._

import zio.http.codec.PathCodec

object RoutesSpec extends ZIOHttpSpec {
  def extractStatus(response: Response): Status = response.status

  val authContext: HandlerAspect[Any, String] = HandlerAspect.customAuthProviding[String] { request =>
    request.headers.get(Header.Authorization).flatMap {
      case Header.Authorization.Basic(uname, secret) if uname.reverse == secret.value.mkString =>
        Some(uname)
      case _                                                                                   =>
        None
    }
  }

  def spec = suite("RoutesSpec")(
    test("empty not found") {
      val app = Routes.empty

      for {
        result <- app.run()
      } yield assertTrue(extractStatus(result) == Status.NotFound)
    },
    test("compose empty not found") {
      val app = Routes.empty ++ Routes.empty

      for {
        result <- app.run()
      } yield assertTrue(extractStatus(result) == Status.NotFound)
    },
    test("run identity") {
      val body = Body.fromString("foo")

      val app = handler { (req: Request) =>
        Response(body = req.body)
      }

      for {
        result <- app.runZIO(Request(body = body))
      } yield assertTrue(result.body == body)
    },
    test("routes with different path parameter arities should all be handled") {
      val one    = Method.GET / string("first") -> Handler.ok
      val getone = Request.get("/1")

      val two    = Method.GET / string("prefix") / string("second") -> Handler.internalServerError
      val gettwo = Request.get("/2/two")

      val onetwo = Routes(one, two)
      val twoone = Routes(two, one)

      for {
        onetwoone <- onetwo.runZIO(getone)
        onetwotwo <- onetwo.runZIO(gettwo)
        twooneone <- twoone.runZIO(getone)
        twoonetwo <- twoone.runZIO(gettwo)
      } yield {
        assertTrue(
          extractStatus(onetwoone) == Status.Ok,
          extractStatus(onetwotwo) == Status.InternalServerError,
          extractStatus(twooneone) == Status.Ok,
          extractStatus(twoonetwo) == Status.InternalServerError,
        )
      }
    },
    test("nest routes") {
      import PathCodec._
      import zio._
      case object IdFormatError
      val routes = literal("to") / Routes(
        Method.GET / "other"             -> handler(ZIO.fail(IdFormatError)),
        Method.GET / "do" / string("id") -> handler { (id: String, _: Request) => Response.text(s"GET /to/do/${id}") },
      )
      routes.handleError { case IdFormatError =>
        Response.badRequest
      }
        .run(
          path = Path.root / "to" / "do" / "123",
        )
        .map(response => assertTrue(response.status == Status.Ok))
    },
    test("alternative path segments") {
      val app = Routes(
        Method.GET / anyOf("foo", "bar", "baz") -> Handler.ok,
      )

      for {
        foo <- app.runZIO(Request.get("/foo"))
        bar <- app.runZIO(Request.get("/bar"))
        baz <- app.runZIO(Request.get("/baz"))
        box <- app.runZIO(Request.get("/box"))
      } yield {
        assertTrue(
          extractStatus(foo) == Status.Ok,
          extractStatus(bar) == Status.Ok,
          extractStatus(baz) == Status.Ok,
          extractStatus(box) == Status.NotFound,
        )
      }
    },
    test("HandlerAspect works with multiple dependencies in Scala 2") {
      val routeWithMultipleDeps = Method.GET / "multiple-deps" -> handler { _: Request =>
        for {
          intDep  <- ZIO.service[Int]
          longDep <- ZIO.service[Long]
        } yield Response.text(s"Int: $intDep, Long: $longDep")
      }

      val routesWithAspect = Routes(routeWithMultipleDeps).@@[Int with Long](authContext)

      for {
        result <- routesWithAspect
          .runZIO(Request.get("/multiple-deps"))
          .provideLayer(ZLayer.succeed(42) ++ ZLayer.succeed(100L))
      } yield assertTrue(result.status == Status.Ok && result.body.asString == "Int: 42, Long: 100")
    },
  )
}
