import util.parsing.combinator._
import xml._
import xml.transform._

object Mapnik2GeoTools {
  val gsDataDir = new java.net.URL("file:///home/dwins/Projects/osm_cpk_data/styles/")

  object PointSymTransformer extends RewriteRule {
    def convertPointSymbolizer(point: Elem): Node = {
      val attmap = point.attributes.asAttrMap
      val path = attmap.get("file")
      val format = attmap.getOrElse("type", "image/png") match {
        case "png" => "image/png"
        case "gif" => "image/gif"
        case "jpeg" => "image/jpeg"
      }

      <PointSymbolizer> {
        if (path.isDefined) {
          <Graphic>
            <ExternalGraphic>
              <OnlineResource xlink:href={new java.net.URL(gsDataDir, path.get).toString}/>
              <Format>{ format }</Format>
            </ExternalGraphic>
          </Graphic>
        }
      } </PointSymbolizer>
    }

    override def transform(node: Node): Seq[Node] =
      node match {
        case e: Elem if e.label == "PointSymbolizer" =>
          convertPointSymbolizer(e)
        case n => n
      }
  }

  class TextSymTransformer(fontsets: NodeSeq) extends RewriteRule {
    val fonts: Map[String, Seq[String]] =
      (
        for {
          fset <- fontsets
          name = fset.attributes.asAttrMap("name")
          faces = fset \\ "@face_name" map(_.text)
        } yield { name -> faces }
      ) toMap

    def convertTextSymbolizer(text: Elem): Node = {
      val attmap = text.attributes.asAttrMap

      <TextSymbolizer> {
        if (attmap contains "name")
          <Label>
            { if (attmap.get("text_convert") == Some("toupper"))
                <ogc:Function name="strToUpperCase">
                  <ogc:PropertyName>{ attmap("name") }</ogc:PropertyName>
                </ogc:Function>
              else
                <ogc:PropertyName>{ attmap("name") }</ogc:PropertyName>
            }
          </Label>
        }

        <Font>
          { if (attmap.contains("fontset_name") && fonts.contains(attmap("fontset_name"))) 
              { for (face <- fonts(attmap("fontset_name")))
                yield <CssParameter name="font-family">{ face }</CssParameter>
              } flatten
          }
          <CssParameter name="font-size">{ attmap("size") }</CssParameter>
          { if (attmap("fontset_name") contains "bold")
              <CssParameter name="font-weight">bold</CssParameter>
          }
          { if (attmap("fontset_name") contains "oblique")
              <CssParameter name="font-style">italic</CssParameter>
          }
        </Font>

        <LabelPlacement>
        { if (attmap.get("placement") == Some("line"))
            <LinePlacement/>
          else
            <PointPlacement>
              <AnchorPoint>
                <AnchorPointX>
                  <ogc:Literal>0.5</ogc:Literal>
                </AnchorPointX>
                <AnchorPointY>
                  <ogc:Literal>0.5</ogc:Literal>
                </AnchorPointY>
              </AnchorPoint>
              { if ((attmap contains "dx") && (attmap contains "dy"))
                <Displacement>
                  <DisplacementX>
                    <ogc:Literal>{ attmap("dx") }</ogc:Literal>
                  </DisplacementX>
                  <DisplacementY>
                    <ogc:Literal>{ attmap("dy") }</ogc:Literal>
                  </DisplacementY>
                </Displacement>
              }
              <Rotation>
                <ogc:Literal>0</ogc:Literal>
              </Rotation>
            </PointPlacement>
        }
        </LabelPlacement>

        { if (attmap contains "halo_fill") {
            val fill = attmap("halo_fill").dropRight(1).drop(5)  // trims off "rgba(" and ")"
            val rgb = fill.split(",").take(3).map(_.toInt)
            val colorcode = "#%2x%2x%2x".format(rgb(0), rgb(1), rgb(2))
            val opacity = fill.split(",").last.toDouble
            <Halo>
              <Radius>
                <ogc:Literal> { attmap.getOrElse("halo_radius", "1") } </ogc:Literal>
              </Radius>
              <Fill>
                <CssParameter name="fill">{colorcode}</CssParameter>
                <CssParameter name="fill-opacity">{opacity}</CssParameter>
              </Fill>
            </Halo>
          }
        }

        <Fill>
          <CssParameter name="fill">{ 
            attmap.getOrElse("fill", "#000000") 
          }</CssParameter>
        </Fill>

        { if (attmap.get("placement") == Some("line"))
            <VendorOption name="followLine">true</VendorOption>
        }

        { Comment(attmap.toString) }
      </TextSymbolizer>
    }

    def convertShieldSymbolizer(shield: Elem): Seq[Node] = {
      val attmap = shield.attributes.asAttrMap

      <TextSymbolizer>
        { if (attmap contains "name")
            <Label><ogc:PropertyName>{ attmap("name") }</ogc:PropertyName></Label>
        }
        <Font>
          <CssParameter name="font-family">SansSerif</CssParameter>
          { if (attmap("fontset_name") == Some("bold-fonts"))
              <CssParameter name="font-weight">bold</CssParameter>
          }
          { if (attmap contains "size")
              <CssParameter name="font-size">{ attmap("size") }</CssParameter>
          }
        </Font>
        <LabelPlacement>
          <PointPlacement>
            <AnchorPoint>
              <AnchorPointX>
                <ogc:Literal>0.5</ogc:Literal>
              </AnchorPointX>
              <AnchorPointY>
                <ogc:Literal>0.5</ogc:Literal>
              </AnchorPointY>
            </AnchorPoint>
            { if ((attmap contains "dx") && (attmap contains "dy"))
              <Displacement>
                <DisplacementX>
                  <ogc:Literal>{ attmap("dx") }</ogc:Literal>
                </DisplacementX>
                <DisplacementY>
                  <ogc:Literal>{ attmap("dy") }</ogc:Literal>
                </DisplacementY>
              </Displacement>
            }
            <Rotation>
              <ogc:Literal>0</ogc:Literal>
            </Rotation>
          </PointPlacement>
        </LabelPlacement>
        <Halo>
        </Halo>
        <Fill>
        </Fill>
        <Graphic>
          <ExternalGraphic>
            <OnlineResource xlink:href={new java.net.URL(gsDataDir, attmap("file")).toString}/>
            <Format>{ 
              attmap.getOrElse("type", "image/png") match {
                case "png" => "image/png"
                case "jpeg" => "image/jpeg"
                case "gif" => "image/gif"
                case other => other
              }
            }</Format>
            { if (attmap contains "height")
                <Size>{ attmap("height") }</Size>
            }
          </ExternalGraphic>
        </Graphic>

      </TextSymbolizer>
    }

    override def transform(node: Node): Seq[Node] =
      node match {
        case e: Elem if e.label == "TextSymbolizer" =>
          convertTextSymbolizer(e)
        case e: Elem if e.label == "ShieldSymbolizer" =>
          convertShieldSymbolizer(e)
        case n => n
      }
  }

  object LineSymTransformer extends RewriteRule {
    override def transform(node: Node): Seq[Node] =
      node match {
        case e: Elem if e.label == "LineSymbolizer" =>
          e.copy(child = <Stroke>{e.child}</Stroke>)
        case n => n
      }
  }

  object PolygonSymTransformer extends RewriteRule {
    def convertPatternSymbolizer(e: Elem): Seq[Node] = {
      val attrs = e.attributes.asAttrMap

      val format =
        attrs.getOrElse("type", "image/png") match {
          case "png" => "image/png"
          case "gif" => "image/gif"
          case "jpeg" => "image/jpeg"
          case other => other
        }

      <PolygonSymbolizer>
        <Fill>
          <GraphicFill>
            <Graphic>
              <ExternalGraphic>
                <OnlineResource xlink:href={new java.net.URL(gsDataDir, attrs("file")).toString}/>
                <Format>{ format }</Format>
              </ExternalGraphic>
              <Size>{ attrs("height") }</Size>
            </Graphic>
          </GraphicFill>
        </Fill>
      </PolygonSymbolizer>
    }

    override def transform(node: Node): Seq[Node] =
      node match {
        case e: Elem if e.label == "PolygonSymbolizer" =>
          e.copy(child = <Fill>{e.child}</Fill>)
        case e: Elem if e.label == "PolygonPatternSymbolizer" =>
          convertPatternSymbolizer(e)
        case n => n
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
            (rule \ "MinScaleDenominator") ++
            (rule \ "MaxScaleDenominator") ++
            (rule \ "PolygonSymbolizer") ++
            (rule \ "LineSymbolizer") ++
            (rule \ "PointSymbolizer") ++
            (rule \ "TextSymbolizer")

          // for easier debugging, throw the things that *didn't* get sorted in
          // at the end
          val child = ordered ++ (rule.child diff ordered)

          rule.copy(child = child)
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

    val comparison = equal | greater | greaterOrEqual | less | like

    val negated = "not" ~> comparison map (c => <Not>{c}</Not>)

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

  trait Output {
    def writeStyle(style: Node): Unit
    def writeLayers(layers: NodeSeq): Unit
  }

  def main(args: Array[String]) {
    for (arg <- args) {
      val source = new java.io.File(arg)
      val outdir = new java.io.File(source.getParent(), "output")
      val sink: Output = 
        new GeoServer("http://localhost:8080/geoserver/rest", ("admin", "geoserver"))
        // new FileSystem(outdir)

      val original = XML.loadFile(source)
      val convert =
        new RuleTransformer(
          FilterTransformer,
          PointSymTransformer,
          LineSymTransformer,
          PolygonSymTransformer,
          new TextSymTransformer(original \\ "FontSet"),
          RuleCleanup
        )
      val doc = convert(XML.loadFile(source))
      for (style <- doc \\ "Style") sink.writeStyle(style)
      sink.writeLayers(doc \\ "Layer")
    }
  }
}
