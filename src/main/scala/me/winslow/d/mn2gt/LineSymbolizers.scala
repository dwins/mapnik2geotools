package me.winslow.d.mn2gt

import xml.{ Elem, Node }, xml.transform.RewriteRule

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

object LinePatternSymTransformer extends RewriteRule {
  override def transform(node: Node): Seq[Node] =
    node match {
      case e: Elem if e.label == "LinePatternSymbolizer" =>
        val attrs = e.attributes.asAttrMap

        // blow up if the xml doesn't meet expectations
        require(! (attrs contains "base"), "'base' property for LinePatternSymbolizer not recognized!")
        require(attrs contains "file", "Image path is required for LinePatternSymbolizer conversion")

        val file = attrs("file")
        val width = attrs.get("width")
        val height = attrs.get("height")

        val mime = Mime.guessMime(Some(file), attrs.get("type"))
        val size = (width orElse height).map(_.toInt).getOrElse(4)

        <LineSymbolizer>
          <Stroke>
            <GraphicStroke>
              <Graphic>
                <ExternalGraphic>
                  <OnlineResource xlink:type="simple" xlink:href={"file:" + file}/>
                  <Format>{mime}</Format>
                </ExternalGraphic>
                <Size>{size}</Size>
              </Graphic>
            </GraphicStroke>
          </Stroke>
        </LineSymbolizer>
      case n => n
    }
}
