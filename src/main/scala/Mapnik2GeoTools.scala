package me.winslow.d.mn2gt
import util.parsing.combinator._
import xml._
import xml.transform._

object Mapnik2GeoTools {
  import CSS.colors

  private def attsToParams(elem: Elem): Seq[Node] =
    (
      for ((k, v) <- elem.attributes.asAttrMap) yield
        <CssParameter name={k}>{v}</CssParameter>
    ).toSeq

  val PointSymTransformer = me.winslow.d.mn2gt.PointSymbolizerTransformer

  val MarkersSymTransformer = me.winslow.d.mn2gt.MarkersSymbolizerTransformer

  object LineSymTransformer extends RewriteRule {
    override def transform(node: Node): Seq[Node] =
      node match {
        case e: Elem if e.label == "LineSymbolizer" =>
          <LineSymbolizer>
            <Stroke>{ attsToParams(e) ++ e.child }</Stroke>
          </LineSymbolizer>
        case n => n
      }
  }

  object PolygonSymTransformer extends RewriteRule {
    def convertPatternSymbolizer(e: Elem): Seq[Node] = {
      val attrs = e.attributes.asAttrMap

      val format =
        Mime.guessMime(attrs.get("file"), attrs.get("type"))

      <PolygonSymbolizer>
        <Fill>
          <GraphicFill>
            <Graphic>
              <ExternalGraphic>
                <OnlineResource xlink:href={ attrs("file") }/>
                <Format>{ format }</Format>
              </ExternalGraphic>
              { for (h <- attrs.get("height").toSeq) yield
                  <Size>{ h }</Size>
              }
            </Graphic>
          </GraphicFill>
        </Fill>
      </PolygonSymbolizer>
    }

    override def transform(node: Node): Seq[Node] =
      node match {
        case e: Elem if e.label == "PolygonSymbolizer" =>
          <PolygonSymbolizer>
            <Fill>{ attsToParams(e) ++ e.child }</Fill>
          </PolygonSymbolizer>
        case e: Elem if e.label == "PolygonPatternSymbolizer" =>
          convertPatternSymbolizer(e)
        case n => n
      }
  }

  object RasterSymTransformer extends RewriteRule {
    def convertRasterSymbolizer(e: Elem) = {
      val opacity = e.attributes.asAttrMap.get("opacity")
      val content = opacity.toSeq.map { x =>
        <Opacity>{x}</Opacity>
      }

      <RasterSymbolizer>{ content }</RasterSymbolizer>
    }

    override def transform(node: Node): Seq[Node] = {
      node match {
        case e: Elem if e.label == "RasterSymbolizer" =>
          convertRasterSymbolizer(e)
        case n => n
      }
    }
  }

  /**
   * In order to be valid against the official schema, the elements of an SLD
   * rule have to be in a particular order.  This rewrite rule rearranges the
   * Rule elements properly.
   */
  object RuleCleanup extends RewriteRule {
    override def transform(node: Node): Seq[Node] =
      node match {
        case rule: Elem if rule.label == "Rule" =>
          val ordered =
            (rule \ "Filter") ++
            (rule \ "ElseFilter") ++
            (rule \ "MinScaleDenominator") ++
            (rule \ "MaxScaleDenominator") ++
            (rule \ "PolygonSymbolizer") ++
            (rule \ "LineSymbolizer") ++
            (rule \ "PointSymbolizer") ++
            (rule \ "TextSymbolizer")

          // for easier debugging, throw the things that *didn't* get sorted in
          // at the end
          val child = (ordered ++ (rule.child diff ordered)).flatMap(transform)

          rule.copy(child = child)
        case param: Elem if param.label == "CssParameter"
          && param.attributes.asAttrMap.get("name") == Some("stroke") 
          =>
          val color = param.text.trim
          val cleaned = 
            if (colors contains color)
              colors(color)
            else if (color.length == 4)
              color.take(1) + color.tail.flatMap(x => Seq(x, x))
            else
              color
          param.copy(child = Text(cleaned))
        case param: Elem if param.label == "CssParameter"
          && param.attributes.asAttrMap.get("name") == Some("fill") 
          =>
          val color = param.text.trim
          val cleaned = 
            if (color.length == 4) {
              color.take(1) + color.tail.flatMap(x => Seq(x, x))
            } else {
              color
            }
          param.copy(child = Text(cleaned))
        case param: Elem if param.label == "CssParameter"
          && param.attributes.asAttrMap.get("name") == Some("stroke-dasharray")
          =>
          param.copy(child = Text(param.text.replaceAll(",", " ")))
        case n => n
      }
  }

  object FilterParser extends RegexParsers {
    val property =
      """\[\p{Graph}+\]""".r map (s =>
        <PropertyName>{s.substring(1, s.length -1)}</PropertyName>
      )

    val literal =
      """'.*?'""".r ^^ { s => s.substring(1, s.length -1) } |
      """\d+""".r

    val value = literal map (s => <Literal>{s}</Literal>)

    val isNull = (property <~ "=" <~ "(?i)null".r) map {
        case a => <PropertyIsNull>{a}</PropertyIsNull>
      }

    val equal =
      (property <~ "=") ~ value map { case a ~ b =>
        <PropertyIsEqualTo>{a}{b}</PropertyIsEqualTo>
      }

    val greater =
      (property <~ ">") ~ value map { case a ~ b =>
        <PropertyIsGreaterThan>{a}{b}</PropertyIsGreaterThan>
      }

    val greaterOrEqual =
      (property <~ ">=") ~ value map { case a ~ b =>
        <PropertyIsGreaterThanOrEqualTo>{a}{b}</PropertyIsGreaterThanOrEqualTo>
      }

    val less =
      (property <~ "<") ~ value map { case a ~ b =>
        <PropertyIsLessThan>{a}{b}</PropertyIsLessThan>
      }

    val notEqualTo =
      (property <~ ("<>" | "!=")) ~ value map { case a ~ b =>
        <PropertyIsNotEqualTo>{a}{b}</PropertyIsNotEqualTo>
      }

    def rewriteRegex(e: xml.Elem): xml.Elem =
      if (e.label == "PropertyName")
        e
      else
        e.copy(child=Text(
          e.text
           .replaceAll("\\.\\*", "%")
           .replaceAll("\\.", "_")
           .replaceAll("^\\^", "")
        ))

    val like =
      (property <~ ("." ~ "match" ~ "(")) ~ (value <~ ")") map { case a ~ b =>
        <PropertyIsLike wildCard="%" singleChar="_" escape="\">
          {a}{rewriteRegex(b)}
        </PropertyIsLike>
      }

    val comparison = isNull | equal | greater | greaterOrEqual | less | notEqualTo | like

    val negated = "(?i:not|!)".r ~> child map (c => <Not>{c}</Not>)

    val conjunction = "(?i:or|and)".r map (_.toLowerCase)

    lazy val nested: Parser[Node] = "(" ~> expression <~ ")"

    lazy val child: Parser[Node] = nested | negated | comparison

    val expression =
      child ~ rep(conjunction ~ child) map {
        case start ~ clauses =>
          clauses.foldLeft(start) {
            case (a, "or" ~ b) => <Or>{a}{b}</Or>
            case (a, "and" ~ b) => <And>{a}{b}</And>
          }
      }

    def toXML(text: String): Seq[Node] = {
      val result = parseAll(expression, text)
      if (result.successful) {
        Seq(result.get)
      } else {
        Seq(
          Comment("Unparsed filter - " + text),
          Comment(result.toString)
        )
      }
    }
  }

  object FilterTransformer extends RewriteRule {
    override def transform(node: Node): Seq[Node] =
      node match {
        case e: Elem if e.label == "Filter" =>
          val translated = e.child flatMap {
            case Text(text) => FilterParser.toXML(text)
            case n => n
          }
          val ogc =
            new NamespaceBinding(null, "http://www.opengis.net/ogc", e.scope)
          e.copy(scope = ogc, child = translated)
        case n => n
      }
  }

  def rulesFor(mapnikXml: xml.Elem): Seq[RewriteRule] = {
    val Version = """(\d+)\.(\d+)\.(\d+)""".r
    
    val versionString = {
      val mapnik1VersionString = 
        (mapnikXml \ "Map" \ "@minimum_version" headOption) map(_.text)
      val mapnik2VersionString = 
        (mapnikXml \ "Map" \ "@minimum-version" headOption) map(_.text)
      mapnik2VersionString orElse mapnik1VersionString
    }

    versionString match {
      case None =>
        // No version string found, just go with the latest and greatest.
        // @todo: Give the user some feedback about that decision, maybe we
        // make rulesFor return an Option[Seq[RewriteRule]] or an
        // Either[Seq[RewriteRule], String] with the error message.
        Seq(
          FilterTransformer,
          PointSymbolizerTransformer,
          MarkersSymbolizerTransformer,
          LineSymTransformer,
          PolygonSymTransformer,
          RasterSymTransformer,
          new TextSymbolizerTransformer(mapnikXml \\ "FontSet")
        )
      case Some(Version(major, minor, patch)) =>
        Seq(
          FilterTransformer,
          PointSymbolizerTransformer,
          MarkersSymbolizerTransformer,
          LineSymTransformer,
          PolygonSymTransformer,
          RasterSymTransformer,
          new TextSymbolizerTransformer(mapnikXml \\ "FontSet")
        )
      case Some(v) => sys.error("I don't understand the version number \"%s\"".format(v))
    }
  }
}
