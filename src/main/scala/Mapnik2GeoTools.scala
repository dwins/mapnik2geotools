import util.parsing.combinator._
import xml._
import xml.transform._

object Mapnik2GeoTools {
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
              <OnlineResource xlink:href={path.get}/>
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

  object TextSymTransformer extends RewriteRule {
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

        { if (attmap.get("fontset_name") == Some("oblique-fonts"))
            <Font>
              <CssParameter name="font-family">SansSerif</CssParameter>
              <CssParameter name="font-size">{ attmap("size") }</CssParameter>
            </Font>
        }

        { if (attmap.get("fontset_name") == Some("bold-fonts"))
            <Font>
              <CssParameter name="font-family">SansSerif</CssParameter>
              <CssParameter name="font-size"> { attmap("size") }</CssParameter>
              <CssParameter name="font-style">bold</CssParameter>
            </Font>
        }

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

        { if (attmap contains "fill")
            <Fill>
              <CssParameter name="fill">{attmap("fill") }</CssParameter>
            </Fill>
        }

        { Comment(attmap.toString) }
      </TextSymbolizer>
    }

    def convertShieldSymbolizer(shield: Elem): Seq[Node] = {
      val attmap = shield.attributes.asAttrMap

      <TextSymbolizer>
        { if (attmap contains "name") 
            <Label>{ attmap("name") }</Label>
        }
        <Font>
          <CssParameter name="font-family">SansSerif</CssParameter>
          { if (attmap("fontset_name") == Some("bold-fonts"))
              <CssParameter name="font-style">bold</CssParameter>
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
    override def transform(node: Node): Seq[Node] =
      node match {
        case e: Elem if e.label == "PolygonSymbolizer" =>
          e.copy(child = <Fill>{e.child}</Fill>)
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

    val like =
      (property <~ ("." ~ "match" ~ "(")) ~ (value <~ ")") map { case a ~ b =>
        <PropertyIsLike wildCard="%" singleChar="_" escape="\">
          {a}{b}
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

  def save(file: java.io.File, xml: Node) {
    val writer = new java.io.FileWriter(file)
    writer.write(new PrettyPrinter(80, 2).format(xml))
    writer.close()
  }

  def writeStyle(out: java.io.File, style: Node) {
      val name = style.attribute("name").map(_.text).getOrElse("style")
      val wrapper =
        <StyledLayerDescriptor
          version="1.0.0"
          xmlns="http://www.opengis.net/sld"
          xmlns:ogc="http://www.opengis.net/ogc"
          xmlns:xlink="http://www.w3.org/1999/xlink"
        >
          <NamedLayer>
            <Name>{name}</Name>
            <UserStyle>
              <Name>{name}</Name>
              <FeatureTypeStyle>
                {style.child}
              </FeatureTypeStyle>
            </UserStyle>
          </NamedLayer>
        </StyledLayerDescriptor>
      save(new java.io.File(out, name + ".sld"), wrapper)
  }

  def writeLayer(out: java.io.File, layers: Seq[Node]) {
    val writer = new java.io.FileWriter(new java.io.File(out, "tables.sql"))

    def params(datastore: NodeSeq): Map[String, String] =
      datastore \ "Parameter" map {
        p => (p.attributes.asAttrMap("name"), p.text)
      } toMap

    val selectPattern = """(?si:\(SELECT\s+(.*)\)\s+AS)""".r

    for (layer <- layers if params(layer \ "Datasource") contains "table") {
      val name = layer.attributes.asAttrMap("name")
      val table = params(layer \ "Datasource")("table")

      val select =
        (
          for (s <- selectPattern.findFirstMatchIn(table)) yield s.group(1).trim
        ) getOrElse table

      val wrapper =
        """
        CREATE OR REPLACE VIEW """ + name + """ AS """ + select + """;
        SELECT AddGeometryColumn('','""" + name + """','way',900913,'LINESTRING',2);
        """

      writer.write(wrapper)
    }

    writer.close()
  }

  def main(args: Array[String]) {
    val convert = 
      new RuleTransformer(
        FilterTransformer,
        PointSymTransformer,
        LineSymTransformer,
        PolygonSymTransformer,
        TextSymTransformer,
        RuleCleanup
      )
    for (arg <- args) {
      val source = new java.io.File(arg)
      val outdir = new java.io.File(source.getParent(), "output")
      outdir.mkdirs()
      val doc = convert(XML.loadFile(source))
      for (style <- doc \\ "Style") writeStyle(outdir, style)
      writeLayer(outdir, doc \\ "Layer")
      save(new java.io.File(arg.replaceAll(".xml$", "") + ".sld"), doc)
    }
  }
}
