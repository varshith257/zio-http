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

package zio.http.endpoint.internal

import zio._
import zio.stacktracer.TracingImplicits.disableAutoTrace

import zio.http._
import zio.http.codec._
import zio.http.endpoint._
import zio.http.Status._

private[endpoint] final case class EndpointClient[P, I, E, O, M <: EndpointMiddleware](
  endpointRoot: URL,
  endpoint: Endpoint[P, I, E, O, M],
  codecMapping: Map[Int, Codec[Any, Throwable, E]],
) {
  def execute(client: Client, invocation: Invocation[P, I, E, O, M])(
    mi: invocation.middleware.In,
  )(implicit alt: Alternator[E, invocation.middleware.Err], trace: Trace): ZIO[Scope, alt.Out, O] = {
    val request0 = endpoint.input.encodeRequest(invocation.input)
    val request  = request0.copy(url = endpointRoot ++ request0.url)

    val requestPatch            = invocation.middleware.input.encodeRequestPatch(mi)
    val patchedRequest          = request.patch(requestPatch)
    val withDefaultAcceptHeader =
      if (patchedRequest.headers.exists(_.headerName == Header.Accept.name))
        patchedRequest
      else
        patchedRequest.addHeader(
          Header.Accept(MediaType.application.json, MediaType.parseCustomMediaType("application/protobuf").get),
        )

    client
      .request(withDefaultAcceptHeader)
      .orDie
      .flatMap { response =>
        val decoder = if (response.status.isSuccess) {
          endpoint.output.decodeResponse(response)
        } else {
          // Handle errors based on status code using codecMapping
          val statusCode = response.status.code
          codecMapping.get(statusCode) match {
            case Some(codec) =>
              codec
                .decodeResponse(response)
                .mapError(t => new IllegalStateException(s"Cannot decode response for status $statusCode", t))

            case None =>
              ZIO.fail(new IllegalStateException(s"No codec found for status $statusCode"))
          }
        }

        decoder.orDie
      }
      .catchAll { cause =>
        ZIO.fail(new IllegalStateException("Error decoding response", cause))
      }
  }
}
