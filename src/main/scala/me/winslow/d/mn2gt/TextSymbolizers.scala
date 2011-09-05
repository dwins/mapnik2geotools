package me.winslow.d.mn2gt

import xml._
import transform.RewriteRule

trait LabelHandling {
  def fonts: Map[String, Seq[String]]
}

class TextSymbolizerTransformer(fontsets: NodeSeq) extends RewriteRule {
  val fonts: Map[String, Seq[String]] =
    (
      for {
        fset <- fontsets
        name = fset.attributes.asAttrMap("name")
        faces = fset \\ "@face_name" map(_.text)
      } yield { name -> faces }
    ) toMap

  private def extractLabel(atts: Map[String, String]) =
    <Label>
      { for (
          name <- atts.get("name").toSeq;
          trimmed = name.replaceFirst("^\\[", "").replaceFirst("\\]$", "")
        ) yield
          { if (atts.get("text_convert") == Some("toupper"))
              <ogc:Function name="strToUpperCase">
                <ogc:PropertyName>{trimmed}</ogc:PropertyName>
              </ogc:Function>
            else
              <ogc:PropertyName>{trimmed}</ogc:PropertyName>
          }
      }
    </Label>

  private def extractFont(atts: Map[String, String]) =
    <Font>
      { for {
          fontset <- atts.get("fontset-name").toSeq
          fontList <- fonts.get(fontset).toSeq
          family <- fontList
        } yield <CssParameter name="font-family">{ family }</CssParameter>
      }
      { for (size <- atts.get("size").toSeq) yield
          <CssParameter name="font-size">{size}</CssParameter>
      }
      { if (atts("fontset-name") contains "bold")
          <CssParameter name="font-weight">bold</CssParameter>
      }
      { if (atts("fontset-name") contains "oblique")
          <CssParameter name="font-style">italic</CssParameter>
      }
    </Font>

  private def pointPlacement(atts: Map[String, String]) =
    <PointPlacement>
      <AnchorPoint>
        <AnchorPointX>
          <ogc:Literal>0.5</ogc:Literal>
        </AnchorPointX>
        <AnchorPointY>
          <ogc:Literal>0.5</ogc:Literal>
        </AnchorPointY>
      </AnchorPoint>
      { for (dx <- atts.get("dx").toSeq; dy <- atts.get("dy").toSeq) yield
          <Displacement>
            <DisplacementX>
              <ogc:Literal>{dx}</ogc:Literal>
            </DisplacementX>
            <DisplacementY>
              <ogc:Literal>{dy}</ogc:Literal>
            </DisplacementY>
          </Displacement>
      }
      <Rotation>
        <ogc:Literal>0</ogc:Literal>
      </Rotation>
    </PointPlacement>

  private def extractLabelPlacement(atts: Map[String, String]) =
    <LabelPlacement>
      { if (atts.get("placement") == Some("line"))
          <LinePlacement/>
        else
          { pointPlacement(atts) }
      }
    </LabelPlacement>

  private def extractHalo(atts: Map[String, String]) =
    for (radius <- atts.get("halo_radius").toSeq) yield
      <Halo>
        <Radius>
          <ogc:Literal>{ radius }</ogc:Literal>
        </Radius>
        {
          val fill = atts.getOrElse("halo_fill", "#ffffff")
          if (fill startsWith "#") {
            <Fill>
              <CssParameter name="fill">{fill}</CssParameter>
            </Fill>
          } else {
            val trimmed = fill.drop(5).dropRight(1).split(",")
            val rgb = trimmed.take(3).map(_.toInt)
            val colorcode = "#%2x%2x%2x".format(rgb: _*)
            val opacity = trimmed.last.toDouble
            <Fill>
              <CssParameter name="fill">{ colorcode }</CssParameter>
              <CssParameter name="fill-opacity">{ opacity }</CssParameter>
            </Fill>
          }
        }
      </Halo>

  private def extractFill(atts: Map[String, String]) =
    <Fill>
      { atts.get("fill").toSeq map {
          case hex if hex.startsWith("#") && hex.length == 7
            => <CssParameter name="fill">{ hex }</CssParameter>
          case hex if hex.startsWith("#") && hex.length == 4
            => <CssParameter name="fill">{ "#" + hex.tail.flatMap(c => c.toString * 2)}</CssParameter>
          case _
            => <CssParameter name="fill">#000000</CssParameter>
        }
      }
    </Fill>

  private def extractGraphic(atts: Map[String, String]) =
    for (file <- atts.get("file").toSeq) yield
      <Graphic>
        <ExternalGraphic>
          <OnlineResource xlink:href={ file }/>
          <Format>{
            Mime.guessMime(atts.get("file"), atts.get("type"))
          }</Format>
        </ExternalGraphic>
        { for (height <- atts.get("height").toSeq) yield
            <Size>{ height }</Size>
        }
      </Graphic>

  def extractVendorParams(atts: Map[String, String]) = {
    val knownParams =
      Seq(
        "min_distance" -> "spaceAround",
        "spacing" -> "minGroupDistance"
      )

    for {
      (mapnikName, gtName) <- knownParams
      value <- atts.get(mapnikName)
    } yield
      <VendorOption name={gtName}>{ value }</VendorOption>
  }

  def convertTextSymbolizer(text: Elem): Node = {
    val attmap = text.attributes.asAttrMap

    <TextSymbolizer>
      { extractLabel(attmap) }
      { extractFont(attmap) }
      { extractLabelPlacement(attmap) }
      { extractHalo(attmap) }
      { extractFill(attmap) }

      { if (attmap.get("placement") == Some("line"))
          <VendorOption name="followLine">true</VendorOption>
      }

      { extractVendorParams(attmap) }

      { Comment(attmap.toString) }
    </TextSymbolizer>
  }

  def convertShieldSymbolizer(shield: Elem): Seq[Node] = {
    val attmap = shield.attributes.asAttrMap

    <TextSymbolizer>
      { extractLabel(attmap) }
      { extractFont(attmap) }
      <LabelPlacement>
        { pointPlacement(attmap) }
      </LabelPlacement>
      { extractHalo(attmap) }
      { extractFill(attmap) }
      { extractGraphic(attmap) }
      { extractVendorParams(attmap) }
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
