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

  def main(args: Array[String]) {
    val convert = new RuleTransformer(
      PointSymTransformer,
      LineSymTransformer,
      PolygonSymTransformer,
      RuleCleanup
    )
    for (arg <- args) {
      val source = new java.io.File(arg)
      val outdir = new java.io.File(source.getParent(), "output")
      outdir.mkdirs()
      val doc = convert(XML.loadFile(source))
      for (style <- doc \\ "Style") {
        writeStyle(outdir, style)
      }
      XML.save(arg.replaceAll(".xml$", "") + ".sld", doc)
    }
  }
}
