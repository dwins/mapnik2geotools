package me.winslow.d.mn2gt

import xml._
import transform.RewriteRule

trait LabelHandling {
  def fonts: Map[String, Seq[String]]
}

trait TextProperties {
  def fontsets: NodeSeq

  val fonts: Map[String, Seq[String]] =
    (
      for {
        fset <- fontsets
        name = fset.attributes.asAttrMap("name")
        faces = fset \\ "@face-name" map(_.text)
      } yield { name -> faces }
    ) toMap

  private def extractLabel(atts: Map[String, String], text: String) =
    <Label>
      { for {
          name <- if (text.isEmpty) atts.get("name").toSeq else List(text)
          trimmed = name.replaceFirst("^\\[", "").replaceFirst("\\]$", "")
          prop = <ogc:PropertyName>{ trimmed }</ogc:PropertyName>
        } yield
          { atts.get("text-transform") match {
              case Some("uppercase") => 
                <ogc:Function name="strToUpperCase">{ prop }</ogc:Function>
              case Some("lowercase") =>
                <ogc:Function name="strToLowerCase">{ prop }</ogc:Function>
              case Some("capitalize") =>
                <ogc:Function name="strCapitalize">{ prop }</ogc:Function>
              case _ => prop
          }}
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
      { val dx = atts.get("dx")
        val dy = atts.get("dy")
        if (dx.isDefined || dy.isDefined)
          <Displacement>
            <DisplacementX>
              <ogc:Literal>{ dx.getOrElse(0) }</ogc:Literal>
            </DisplacementX>
            <DisplacementY>
              <ogc:Literal>{ dy.getOrElse(0) }</ogc:Literal>
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
    for (radius <- atts.get("halo-radius").toSeq) yield
      <Halo>
        <Radius>
          <ogc:Literal>{ radius }</ogc:Literal>
        </Radius>
        {
          val fill = atts.getOrElse("halo-fill", "#ffffff")
          val color = new Color(fill)
          <Fill>
            <CssParameter name="fill">{ color.hex }</CssParameter>
            { if (color.hasAlpha())
                <CssParameter name="fill-opacity">{ color.alpha }</CssParameter>
            }
          </Fill>
        }
      </Halo>

  private def extractFill(atts: Map[String, String]) = {
    val fill = atts.getOrElse("fill", "#000000")
    val color = new Color(fill)
    <Fill>
      <CssParameter name="fill">{ color.hex }</CssParameter>
      {
        if (color.hasAlpha())
          <CssParameter name="fill-opacity">{ color.alpha }</CssParameter>
      }
    </Fill>
  }

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
    Seq(
      atts.get("minimum-distance").map(x => <VendorOption name="spaceAround">{x}</VendorOption>),
      atts.get("spacing").map(x => <VendorOption name="minGroupDistance">{x}</VendorOption>),
      atts.get("wrap-width").map(x => <VendorOption name="autoWrap">{x.toInt * 5}</VendorOption>)
    ).flatten
  }

  def convertTextSymbolizer(text: Elem): Node = {
    val attmap = text.attributes.asAttrMap

    <TextSymbolizer>
      { extractLabel(attmap, text.text) }
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
      { extractLabel(attmap, shield.text) }
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
}

class TextSymbolizerTransformer(val fontsets: NodeSeq)
extends RewriteRule with TextProperties
{
  override def transform(node: Node): Seq[Node] =
    node match {
      case e: Elem if e.label == "TextSymbolizer" =>
        convertTextSymbolizer(e)
      case e: Elem if e.label == "ShieldSymbolizer" =>
        convertShieldSymbolizer(e)
      case n => n
    }
}
