package me.winslow.d.mn2gt

import xml._
import transform.RewriteRule

object PointSymbolizerTransformer extends RewriteRule {
  val knownMimeTypes = Map(
    "png" -> "image/png",
    "gif" -> "image/gif",
    "jpeg" -> "image/jpeg",
    "svg" -> "image/svg+xml"
  )

  def identifyMimeType(extension: String): String =
    knownMimeTypes.getOrElse(extension, extension)

  def convertPointSymbolizer(point: Elem): Node = {
    val attmap = point.attributes.asAttrMap
    val path = attmap.get("file")
    val mimeType =
      attmap.get("type")
        .map(identifyMimeType)
        .getOrElse("image/png")

    val graphic = path.toSeq map { p =>
      <Graphic>
        <ExternalGraphic>
          <OnlineResource xlink:type="simple" xlink:href={p}/>
          <Format>{mimeType}</Format>
        </ExternalGraphic>
      </Graphic>
    }

    <PointSymbolizer>{ graphic }</PointSymbolizer>
  }

  override def transform(node: Node): Seq[Node] =
    node match {
      case e: Elem if e.label == "PointSymbolizer" =>
        convertPointSymbolizer(e)
      case n => n
    }
}

object MarkersSymbolizerTransformer extends RewriteRule {
    // Map of Mapnik symbol names to GeoTools/SLD symbol names
    // TODO: Add support for ellipses in GeoTools (somehow; the SLD spec doesn't make it straightforward)
    private val MarkerMapping =
      Map("ellipse" -> "circle", "arrow" -> "arrow")

    private val DefaultMarkerTypes =
      Map("point" -> "ellipse", "line" -> "arrow")

    def convertMarkersSymbolizer(e: Elem): Seq[Node] = {
      val attrs = e.attributes.asAttrMap
      val allowOverlap =
        attrs.get("allow_overlap").map(_.toBoolean).getOrElse(false)
      val spacing = 
        attrs.get("spacing").map(_.toInt).getOrElse(100)
      val maxError = 
        attrs.get("max_error").map(_.toDouble).getOrElse(0.2)
      val filename =
        attrs.get("filename")
      val transform =
        attrs.get("transform")
      val opacity =
        attrs.get("opacity").map(_.toDouble).getOrElse(1d)
      val fill = attrs.get("fill")
      val stroke = attrs.get("stroke")
      val strokeWidth =
        attrs.get("stroke-width").map(_.toDouble).getOrElse(1d)
      val strokeOpacity =
        attrs.get("stroke-opacity").map(_.toDouble).getOrElse(1d)
      val width =
        attrs.get("width").map(_.toDouble).getOrElse(5d)
      val height =
        attrs.get("height").map(_.toDouble).getOrElse(5d)
      val placement =
        attrs.get("placement").getOrElse("line") // line or point
      val markerType =
        attrs.get("marker-type").getOrElse(DefaultMarkerTypes(placement)) // ellipse or arrow

      require(Seq(filename, fill, stroke).exists(_ isDefined))
      require(Set("point", "line") contains placement)
      require(Set("ellipse", "arrow") contains markerType)

      val graphic =
        filename match {
          case Some(filename) =>
            // Note: the Mapnik docs say only SVG is supported here, we don't
            // explicitly check
            <Graphic>
              <ExternalGraphic>
                <OnlineResource xlink:type="simple" xlink:href={filename}/>
                <Format>image/svg</Format>
              </ExternalGraphic>
              <Size>{ math.max(width, height) }</Size>
            </Graphic>
          case None =>
            val fillXml = 
              <Fill>
                <CssParameter name="fill">{fill.get}</CssParameter>
                <CssParameter name="fill-opacity">{opacity}</CssParameter>
              </Fill>

            val strokeXml = 
              <Stroke>
                <CssParameter name="stroke">{stroke.get}</CssParameter>
                <CssParameter name="stroke-opacity">{strokeOpacity}</CssParameter>
                <CssParameter name="stroke-width">{strokeWidth}</CssParameter>
              </Stroke>;

            <Graphic>
              <Mark>
                <WellKnownName>{ MarkerMapping.getOrElse(markerType, markerType) }</WellKnownName>
                { fillXml ++ strokeXml }
              </Mark>
              <Size>{ math.max(width, height) }</Size>
            </Graphic>
        }

        <PointSymbolizer>{ graphic }</PointSymbolizer>
    }

    override def transform(node: Node): Seq[Node] =
      node match {
        case e @ Elem(_, "MarkersSymbolizer", _, _, _*) =>
          convertMarkersSymbolizer(e.asInstanceOf[Elem])
        case n => n
      }
}
