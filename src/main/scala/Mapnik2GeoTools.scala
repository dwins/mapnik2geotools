import util.parsing.combinator._
import xml._
import xml.transform._

object Mapnik2GeoTools {
  private val colors = Map(
    "aliceblue" -> "#f0f8ff",
    "antiquewhite" -> "#faebd7",
    "aqua" -> "#00ffff",
    "aquamarine" -> "#7fffd4",
    "azure" -> "#f0ffff",
    "beige" -> "#f5f5dc",
    "bisque" -> "#ffe4c4",
    "black" -> "#000000",
    "blanchedalmond" -> "#ffebcd",
    "blue" -> "#0000ff",
    "blueviolet" -> "#8a2be2",
    "brown" -> "#a52a2a",
    "burlywood" -> "#deb887",
    "cadetblue" -> "#5f9ea0",
    "chartreuse" -> "#7fff00",
    "chocolate" -> "#d2691e",
    "coral" -> "#ff7f50",
    "cornflowerblue" -> "#6495ed",
    "cornsilk" -> "#fff8dc",
    "crimson" -> "#dc143c",
    "cyan" -> "#00ffff",
    "darkblue" -> "#00008b",
    "darkcyan" -> "#008b8b",
    "darkgoldenrod" -> "#b8860b",
    "darkgray" -> "#a9a9a9",
    "darkgreen" -> "#006400",
    "darkgrey" -> "#a9a9a9",
    "darkkhaki" -> "#bdb76b",
    "darkmagenta" -> "#8b008b",
    "darkolivegreen" -> "#556b2f",
    "darkorange" -> "#ff8c00",
    "darkorchid" -> "#9932cc",
    "darkred" -> "#8b0000",
    "darksalmon" -> "#e9967a",
    "darkseagreen" -> "#8fbc8f",
    "darkslateblue" -> "#483d8b",
    "darkslategray" -> "#2f4f4f",
    "darkslategrey" -> "#2f4f4f",
    "darkturquoise" -> "#00ced1",
    "darkviolet" -> "#9400d3",
    "deeppink" -> "#ff1493",
    "deepskyblue" -> "#00bfff",
    "dimgray" -> "#696969",
    "dimgrey" -> "#696969",
    "dodgerblue" -> "#1e90ff",
    "firebrick" -> "#b22222",
    "floralwhite" -> "#fffaf0",
    "forestgreen" -> "#228b22",
    "fuchsia" -> "#ff00ff",
    "gainsboro" -> "#dcdcdc",
    "ghostwhite" -> "#f8f8ff",
    "gold" -> "#ffd700",
    "goldenrod" -> "#daa520",
    "gray" -> "#808080",
    "grey" -> "#808080",
    "green" -> "#008000",
    "greenyellow" -> "#adff2f",
    "honeydew" -> "#f0fff0",
    "hotpink" -> "#ff69b4",
    "indianred" -> "#cd5c5c",
    "indigo" -> "#4b0082",
    "ivory" -> "#fffff0",
    "khaki" -> "#f0e68c",
    "lavender" -> "#e6e6fa",
    "lavenderblush" -> "#fff0f5",
    "lawngreen" -> "#7cfc00",
    "lemonchiffon" -> "#fffacd",
    "lightblue" -> "#add8e6",
    "lightcoral" -> "#f08080",
    "lightcyan" -> "#e0ffff",
    "lightgoldenrodyellow" -> "#fafad2",
    "lightgray" -> "#d3d3d3",
    "lightgreen" -> "#90ee90",
    "lightgrey" -> "#d3d3d3",
    "lightpink" -> "#ffb6c1",
    "lightsalmon" -> "#ffa07a",
    "lightseagreen" -> "#20b2aa",
    "lightskyblue" -> "#87cefa",
    "lightslategray" -> "#778899",
    "lightslategrey" -> "#778899",
    "lightsteelblue" -> "#b0c4de",
    "lightyellow" -> "#ffffe0",
    "lime" -> "#00ff00",
    "limegreen" -> "#32cd32",
    "linen" -> "#faf0e6",
    "magenta" -> "#ff00ff",
    "maroon" -> "#800000",
    "mediumaquamarine" -> "#66cdaa",
    "mediumblue" -> "#0000cd",
    "mediumorchid" -> "#ba55d3",
    "mediumpurple" -> "#9370db",
    "mediumseagreen" -> "#3cb371",
    "mediumslateblue" -> "#7b68ee",
    "mediumspringgreen" -> "#00fa9a",
    "mediumturquoise" -> "#48d1cc",
    "mediumvioletred" -> "#c71585",
    "midnightblue" -> "#191970",
    "mintcream" -> "#f5fffa",
    "mistyrose" -> "#ffe4e1",
    "moccasin" -> "#ffe4b5",
    "navajowhite" -> "#ffdead",
    "navy" -> "#000080",
    "oldlace" -> "#fdf5e6",
    "olive" -> "#808000",
    "olivedrab" -> "#6b8e23",
    "orange" -> "#ffa500",
    "orangered" -> "#ff4500",
    "orchid" -> "#da70d6",
    "palegoldenrod" -> "#eee8aa",
    "palegreen" -> "#98fb98",
    "paleturquoise" -> "#afeeee",
    "palevioletred" -> "#db7093",
    "papayawhip" -> "#ffefd5",
    "peachpuff" -> "#ffdab9",
    "peru" -> "#cd853f",
    "pink" -> "#ffc0cb",
    "plum" -> "#dda0dd",
    "powderblue" -> "#b0e0e6",
    "purple" -> "#800080",
    "red" -> "#ff0000",
    "rosybrown" -> "#bc8f8f",
    "royalblue" -> "#4169e1",
    "saddlebrown" -> "#8b4513",
    "salmon" -> "#fa8072",
    "sandybrown" -> "#f4a460",
    "seagreen" -> "#2e8b57",
    "seashell" -> "#fff5ee",
    "sienna" -> "#a0522d",
    "silver" -> "#c0c0c0",
    "skyblue" -> "#87ceeb",
    "slateblue" -> "#6a5acd",
    "slategray" -> "#708090",
    "slategrey" -> "#708090",
    "snow" -> "#fffafa",
    "springgreen" -> "#00ff7f",
    "steelblue" -> "#4682b4",
    "tan" -> "#d2b48c",
    "teal" -> "#008080",
    "thistle" -> "#d8bfd8",
    "tomato" -> "#ff6347",
    "turquoise" -> "#40e0d0",
    "violet" -> "#ee82ee",
    "wheat" -> "#f5deb3",
    "white" -> "#ffffff",
    "whitesmoke" -> "#f5f5f5",
    "yellow" -> "#ffff00",
    "yellowgreen" -> "#9acd32"
  )

  private def attsToParams(elem: Elem): Seq[Node] =
    (
      for ((k, v) <- elem.attributes.asAttrMap) yield
        <CssParameter name={k}>{v}</CssParameter>
    ).toSeq

  object PointSymTransformer extends RewriteRule {
    def convertPointSymbolizer(point: Elem): Node = {
      val attmap = point.attributes.asAttrMap
      val path = attmap.get("file")
      val format = attmap.getOrElse("type", "image/png") match {
        case "png" => "image/png"
        case "gif" => "image/gif"
        case "jpeg" => "image/jpeg"
        case other => other
      }

      <PointSymbolizer> {
        if (path.isDefined) {
          <Graphic>
            <ExternalGraphic>
              <OnlineResource xlink:href={ path.get }/>
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

  class TextSymTransformer(fontsets: NodeSeq) extends RewriteRule {
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
              atts.getOrElse("type", "image/png") match {
                case "png" => "image/png"
                case "jpeg" => "image/jpeg"
                case "gif" => "image/gif"
                case "svg" => "image/svg"
                case other => other
              }
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

  object PolygonSymTransformer extends RewriteRule {
    def convertPatternSymbolizer(e: Elem): Seq[Node] = {
      val attrs = e.attributes.asAttrMap

      val format =
        attrs.getOrElse("type", "image/png") match {
          case "png" => "image/png"
          case "gif" => "image/gif"
          case "jpeg" => "image/jpeg"
          case other => other
        }

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
            (rule \ "ElseFilter") ++
            (rule \ "MinScaleDenominator") ++
            (rule \ "MaxScaleDenominator") ++
            (rule \ "PolygonSymbolizer") ++
            (rule \ "LineSymbolizer") ++
            (rule \ "PointSymbolizer") ++
            (rule \ "TextSymbolizer")

          // for easier debugging, throw the things that *didn't* get sorted in
          // at the end
          val child = (ordered ++ (rule.child diff ordered)).flatMap(transform)

          rule.copy(child = child)
        case param: Elem if param.label == "CssParameter"
          && param.attributes.asAttrMap.get("name") == Some("stroke") 
          =>
          val color = param.text.trim
          val cleaned = 
            if (colors contains color)
              colors(color)
            else if (color.length == 4)
              color.take(1) + color.tail.flatMap(x => Seq(x, x))
            else
              color
          param.copy(child = Text(cleaned))
        case param: Elem if param.label == "CssParameter"
          && param.attributes.asAttrMap.get("name") == Some("fill") 
          =>
          val color = param.text.trim
          val cleaned = 
            if (color.length == 4) {
              color.take(1) + color.tail.flatMap(x => Seq(x, x))
            } else {
              color
            }
          param.copy(child = Text(cleaned))
        case param: Elem if param.label == "CssParameter"
          && param.attributes.asAttrMap.get("name") == Some("stroke-dasharray")
          =>
          param.copy(child = Text(param.text.replaceAll(",", " ")))
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

    val isNull = (property <~ "=" <~ "(?i)null".r) map {
        case a => <PropertyIsNull>{a}</PropertyIsNull>
      }

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

    val notEqualTo =
      (property <~ ("<>" | "!=")) ~ value map { case a ~ b =>
        <PropertyIsNotEqualTo>{a}{b}</PropertyIsNotEqualTo>
      }

    def rewriteRegex(e: xml.Elem): xml.Elem =
      if (e.label == "PropertyName")
        e
      else
        e.copy(child=Text(
          e.text
           .replaceAll("\\.\\*", "%")
           .replaceAll("\\.", "_")
           .replaceAll("^\\^", "")
        ))

    val like =
      (property <~ ("." ~ "match" ~ "(")) ~ (value <~ ")") map { case a ~ b =>
        <PropertyIsLike wildCard="%" singleChar="_" escape="\">
          {a}{rewriteRegex(b)}
        </PropertyIsLike>
      }

    val comparison = isNull | equal | greater | greaterOrEqual | less | notEqualTo | like

    val negated = "not" ~> child map (c => <Not>{c}</Not>)

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

  trait Output {
    def writeStyle(style: Node): Unit
    def writeLayers(layers: NodeSeq): Unit
  }
}
