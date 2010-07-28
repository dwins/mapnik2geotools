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

  def main(args: Array[String]) {
    val convert = new RuleTransformer(PointSymTransformer)
    for (arg <- args) {
      val doc = convert(XML.loadFile(arg))
      XML.save(arg.replaceAll(".xml$", "") + ".sld", doc)
    }
  }
}
