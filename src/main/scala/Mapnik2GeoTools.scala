import util.parsing.combinator._
import xml._
import xml.transform._

object Mapnik2GeoTools {
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
            fontset <- atts.get("fontset_name").toSeq
            fontList <- fonts.get(fontset).toSeq
            family <- fontList
          } yield <CssParameter name="font-family">{ family }</CssParameter>
        }
        { for (size <- atts.get("size").toSeq) yield
            <CssParameter name="font-size">{size}</CssParameter>
        }
        { if (atts("fontset_name") contains "bold")
            <CssParameter name="font-weight">bold</CssParameter>
        }
        { if (atts("fontset_name") contains "oblique")
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
      for (fill <- atts.get("halo_fill").toSeq) yield
        <Halo>
          <Radius>
            <ogc:Literal>{ atts.getOrElse("halo_radius", "1") }</ogc:Literal>
          </Radius>
          {
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
            if (color.length == 4) {
              color.take(1) + color.tail.flatMap(x => Seq(x, x))
            } else {
              color
            }
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

    val comparison = equal | greater | greaterOrEqual | less | notEqualTo | like

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
