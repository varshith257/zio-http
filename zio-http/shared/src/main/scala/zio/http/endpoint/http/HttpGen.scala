package zio.http.endpoint.http

import zio.http.MediaType
import zio.http.codec._
import zio.http.endpoint.Endpoint
import zio.http.endpoint.openapi.OpenAPIGen.{AtomizedMetaCodecs, MetaCodec}
import zio.http.endpoint.openapi.{JsonSchema, OpenAPIGen}

object HttpGen {

  private val PathWildcard = "pathWildcard"

  def fromEndpoints(
    endpoint1: Endpoint[_, _, _, _, _],
    endpoints: Endpoint[_, _, _, _, _]*,
  ): HttpFile = {
    HttpFile((endpoint1 +: endpoints).map(fromEndpoint).toList)
  }

  def fromEndpoint(endpoint: Endpoint[_, _, _, _, _]): HttpEndpoint = {
    val atomizedInput = AtomizedMetaCodecs.flatten(endpoint.input)
    HttpEndpoint(
      OpenAPIGen.method(atomizedInput.method),
      buildPath(endpoint.input),
      headersVariables(atomizedInput).map(_.name),
      bodySchema(atomizedInput),
      variables(atomizedInput),
      doc(endpoint),
    )
  }

  private def bodySchema(inAtoms: AtomizedMetaCodecs) = {
    // currently only json support. No multipart/form-data or x-www-form-urlencoded
    if (inAtoms.content.size != 1) None
    else
      inAtoms.content.collect {
        case MetaCodec(HttpCodec.Content(codec, _, _), _) if codec.choices.contains(MediaType.application.json) =>
          val schema     = codec.choices(MediaType.application.json).schema
          val jsonSchema = JsonSchema.fromZSchema(schema)
          jsonSchema
      }.headOption
  }

  private def doc(endpoint: Endpoint[_, _, _, _, _]) =
    if (endpoint.doc == Doc.empty) None else Some(endpoint.doc.toPlaintext(color = false))

  def variables(inAtoms: AtomizedMetaCodecs): Seq[HttpVariable] =
    pathVariables(inAtoms) ++ queryVariables(inAtoms) ++ headersVariables(inAtoms) ++ bodyVariables(inAtoms)

  def bodyVariables(inAtoms: AtomizedMetaCodecs): Seq[HttpVariable] = {
    val bodySchema0 = bodySchema(inAtoms)

    def loop(schema: JsonSchema, name: Option[String]): Seq[HttpVariable] = schema match {
      case JsonSchema.AnnotatedSchema(schema, _) => loop(schema, name)
      case JsonSchema.RefSchema(_)               => throw new Exception("RefSchema not supported")
      case JsonSchema.OneOfSchema(_)             => throw new Exception("OneOfSchema not supported")
      case JsonSchema.AllOfSchema(_)             => throw new Exception("AllOfSchema not supported")
      case JsonSchema.AnyOfSchema(_)             => throw new Exception("AnyOfSchema not supported")
      case JsonSchema.Number(format)             =>
        val typeHint = format match {
          case JsonSchema.NumberFormat.Float  => "type: Float"
          case JsonSchema.NumberFormat.Double => "type: Double"
        }
        Seq(HttpVariable(getName(name), None, Some(typeHint)))
      case JsonSchema.Integer(format)            =>
        val typeHint = format match {
          case JsonSchema.IntegerFormat.Int32     => "type: Int"
          case JsonSchema.IntegerFormat.Int64     => "type: Long"
          case JsonSchema.IntegerFormat.Timestamp => "type: Timestamp in milliseconds"
        }
        Seq(HttpVariable(getName(name), None, Some(typeHint)))
      case JsonSchema.String(format, pattern)    =>
        val formatHint: String  = format match {
          case Some(value) => s" format: ${value.value}"
          case None        => ""
        }
        val patternHint: String = pattern match {
          case Some(value) => s" pattern: ${value.value}"
          case None        => ""
        }
        Seq(HttpVariable(getName(name), None, Some(s"type: String$formatHint$patternHint")))
      case JsonSchema.Boolean                    => Seq(HttpVariable(getName(name), None, Some("type: Boolean")))
      case JsonSchema.ArrayType(items)           =>
        val typeHint =
          items match {
            case Some(schema) =>
              loop(schema, Some("notUsed")).map(_.render).mkString(";")
            case None         =>
              ""
          }

        Seq(HttpVariable(getName(name), None, Some(s"type: array of $typeHint")))
      case JsonSchema.Object(properties, _, _)   =>
        properties.flatMap { case (key, value) => loop(value, Some(key)) }.toSeq
      case JsonSchema.Enum(values) => Seq(HttpVariable(getName(name), None, Some(s"enum: ${values.mkString(",")}")))
      case JsonSchema.Null         => Seq.empty
      case JsonSchema.AnyJson      => Seq.empty
    }

    bodySchema0 match {
      case Some(schema) => loop(schema, None)
      case None         => Seq.empty
    }
  }

  private def getName(name: Option[String]) = { name.getOrElse(throw new IllegalArgumentException("name is required")) }

  def headersVariables(inAtoms: AtomizedMetaCodecs): Seq[HttpVariable] =
    inAtoms.header.collect { case mc @ MetaCodec(HttpCodec.Header(name, codec, _), _) =>
      HttpVariable(
        name.capitalize,
        mc.examples.values.headOption.map(e => codec.asInstanceOf[TextCodec[Any]].encode(e)),
      )
    }

  def queryVariables(inAtoms: AtomizedMetaCodecs): Seq[HttpVariable] = {
    inAtoms.query.collect { case mc @ MetaCodec(HttpCodec.Query(name, _, _, _), _) =>
      HttpVariable(
        name,
        mc.examples.values.headOption.map(_.toString),
      )
//      OpenAPI.ReferenceOr.Or(
//        OpenAPI.Parameter.queryParameter(
//          name = name,
//          description = mc.docsOpt,
//          schema = Some(OpenAPI.ReferenceOr.Or(JsonSchema.fromTextCodec(codec))),
//          deprecated = mc.deprecated,
//          style = OpenAPI.Parameter.Style.Form,
//          explode = false,
//          allowReserved = false,
//          examples = mc.examples.map { case (name, value) =>
//            name -> OpenAPI.ReferenceOr.Or(OpenAPI.Example(value = Json.Str(value.toString)))
//          },
//          required = mc.required,
//          ),
//        )
    }
  }

  private def pathVariables(inAtoms: AtomizedMetaCodecs) = {
    inAtoms.path.collect {
      case mc @ MetaCodec(codec, _) if codec != SegmentCodec.Empty && !codec.isInstanceOf[SegmentCodec.Literal] =>
        HttpVariable(
          mc.name.getOrElse(throw new Exception("Path parameter must have a name")),
          mc.examples.values.headOption.map(_.toString),
        )
      //        OpenAPI.ReferenceOr.Or(
      //          OpenAPI.Parameter.pathParameter(
      //            name = mc.name.getOrElse(throw new Exception("Path parameter must have a name")),
      //            description = mc.docsOpt.flatMap(_.flattened.filterNot(_ == pathDoc).reduceOption(_ + _)),
      //            definition = Some(OpenAPI.ReferenceOr.Or(JsonSchema.fromSegmentCodec(codec))),
      //            deprecated = mc.deprecated,
      //            style = OpenAPI.Parameter.Style.Simple,
      //            examples = mc.examples.map { case (name, value) =>
      //              name -> OpenAPI.ReferenceOr.Or(OpenAPI.Example(segmentToJson(codec, value)))
      //            },
      //            ),
      //          )
    }
  }

  def buildPath(in: HttpCodec[_, _]): String = {

    def pathCodec(in1: HttpCodec[_, _]): Option[HttpCodec.Path[_]] = in1 match {
      case atom: HttpCodec.Atom[_, _]            =>
        atom match {
          case codec @ HttpCodec.Path(_, _) => Some(codec)
          case _                            => None
        }
      case HttpCodec.Annotated(in, _)            => pathCodec(in)
      case HttpCodec.TransformOrFail(api, _, _)  => pathCodec(api)
      case HttpCodec.Empty                       => None
      case HttpCodec.Halt                        => None
      case HttpCodec.Combine(left, right, _)     => pathCodec(left).orElse(pathCodec(right))
      case HttpCodec.Fallback(left, right, _, _) => pathCodec(left).orElse(pathCodec(right))
    }

    val atomizedInput = AtomizedMetaCodecs.flatten(in)
    val queryNames    = queryVariables(atomizedInput).map(_.name)

    val pathString = {
      val codec = pathCodec(in).getOrElse(throw new Exception("No path found.")).pathCodec
      if (codec.render("{{", "}}").endsWith(SegmentCodec.Trailing.render))
        codec.renderIgnoreTrailing("{{", "}}") + s"{{$PathWildcard}}"
      else codec.render("{{", "}}")
    }

    if (queryNames.nonEmpty) pathString + "?" + queryNames.map(name => s"$name={{$name}}").mkString("&")
    else pathString
  }

}
