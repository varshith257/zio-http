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

package zio.http.netty

import io.netty.channel.{ChannelFactory, EventLoopGroup, ServerChannel}
import zio._
import zio.http._

import java.util.concurrent.atomic.AtomicReference // scalafix:ok;
import zio.stacktracer.TracingImplicits.disableAutoTrace
package object server {
  private[server] type AppRef = AtomicReference[(HttpApp[Any], ZEnvironment[Any])]
  private[server] type EnvRef = AtomicReference[ZEnvironment[Any]]

  val live: ZLayer[Server.Config, Throwable, Driver] =
    NettyDriver.live

  val manual: ZLayer[EventLoopGroup & ChannelFactory[ServerChannel] & Server.Config & NettyConfig, Nothing, Driver] =
    NettyDriver.manual
}