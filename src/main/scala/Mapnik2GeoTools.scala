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
      XML.save(new java.io.File(out, name + ".sld").getPath(), wrapper)
  }

  def main(args: Array[String]) {
    val convert = new RuleTransformer(
      PointSymTransformer,
      LineSymTransformer,
      PolygonSymTransformer
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
