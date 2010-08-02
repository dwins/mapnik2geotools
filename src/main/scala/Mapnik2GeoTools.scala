import xml._
import xml.transform._

object Mapnik2GeoTools {
  object PointSymTransformer extends RewriteRule {
    def convertPointSymbolizer(point: Elem): Node = {
      val attmap = point.attributes.asAttrMap
      val path = attmap.get("file")

      <PointSymbolizer> {
        if (path.isDefined) {
          <Graphic>
            <ExternalGraphic>
              <OnlineResource href={path.get}/>
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

    override def transform(node: Node): Seq[Node] =
      node match {
        case e: Elem if e.label == "TextSymbolizer" =>
          convertTextSymbolizer(e)
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
            (rule \ "MinScaleDenominator") ++
            (rule \ "MaxScaleDenominator") ++
            (rule \ "Filter") ++
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
    val convert = new RuleTransformer(
      PointSymTransformer,
      TextSymTransformer,
      LineSymTransformer,
      PolygonSymTransformer,
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
