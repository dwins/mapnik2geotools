package me.winslow.d.mn2gt

import scala.xml.{ Elem, Node }
import scala.xml.transform.RewriteRule

object PolygonSymTransformer extends RewriteRule {
  private def attsToParams(e: Elem): Seq[Node] =
    e.attributes.asAttrMap.toSeq.map { case (k, v) => 
      <CssParameter name={k}>{v}</CssParameter>
    }

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
