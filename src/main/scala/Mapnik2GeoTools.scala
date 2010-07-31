import util.parsing.combinator._
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
            (rule \ "Filter") ++
            (rule \ "MinScaleDenominator") ++
            (rule \ "MaxScaleDenominator") ++
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
        <PropertyIsGreaterOrEqual>{a}{b}</PropertyIsGreaterOrEqual>
      }

    val less =
      (property <~ "<") ~ value map { case a ~ b =>
        <PropertyIsLessThan>{a}{b}</PropertyIsLessThan>
      }

    val like =
      (property <~ ("." ~ "match" ~ "(")) ~ (value <~ ")") map { case a ~ b =>
        <PropertyIsLike>{a}{b}</PropertyIsLike>
      }

    val comparison = equal | greater | greaterOrEqual | less | like

    val negated = "not" ~> comparison map (c => <Not>{c}</Not>)

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
    val convert = 
      new RuleTransformer(
        FilterTransformer,
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
