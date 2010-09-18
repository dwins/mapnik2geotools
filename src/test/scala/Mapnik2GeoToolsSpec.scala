import org.specs._
import specification.PendingUntilFixed
import scala.xml._
import Mapnik2GeoTools._

object Mapnik2GeoToolsSpec extends Specification with PendingUntilFixed {
  "osm.xml should work" in {}

  "line symbolizers" should {
    val tx = new transform.RuleTransformer(LineSymTransformer, RuleCleanup)
    "support patterns" in {
      val transformed =
        tx(<LinePatternSymbolizer file="symbols/chair_lift.png"></LinePatternSymbolizer>)

      transformed.label must_== "LineSymbolizer"
    } pendingUntilFixed

    "expand stroke colors" in {
      val shorthex =
        tx(
          <LineSymbolizer>
            <CssParameter name="stroke">#888</CssParameter>
          </LineSymbolizer>
        )

      shorthex must \\(<CssParameter name="stroke">#888888</CssParameter>)

      val namedcolor =
        tx(
          <LineSymbolizer>
            <CssParameter name="stroke">salmon</CssParameter>
          </LineSymbolizer>
        )

      namedcolor must \\(<CssParameter name="stroke">#fa8072</CssParameter>)
    }

    "translate dasharrays" in {
      val transformed =
        tx(
          <LineSymbolizer>
            <CssParameter name="stroke-dasharray">2,2</CssParameter>
          </LineSymbolizer>
        )

      transformed must \\(<CssParameter name="stroke-dasharray">2 2</CssParameter>)
    }

    "extract CssParameters from attributes" in {
      val transformed =
        tx(<LineSymbolizer stroke="#b4b4b4" stroke-width="0.5"/>)

      transformed must ==/(
        <LineSymbolizer>
          <Stroke>
            <CssParameter name="stroke">#b4b4b4</CssParameter>
            <CssParameter name="stroke-width">0.5</CssParameter>
          </Stroke>
        </LineSymbolizer>
      )
    }
  }

  "point symbolizers" should {
    val tx = new transform.RuleTransformer(PointSymTransformer)
    "not require a 'type' attribute" in {
      tx(<PointSymbolizer file="symbols/lock_gate.png" />) must \("Graphic")
    }
  }

  "polygon symbolizers" should {
    val tx = new transform.RuleTransformer(PolygonSymTransformer)
    "not require a 'height' attribute" in {
      val transformed =
        tx(<PolygonPatternSymbolizer file="symbols/glacier.png" />)
      transformed must \("Fill") \("GraphicFill")
      transformed must not(\\("Size"))
    }

    "extract CssParameters from attributes" in {
      val transformed =
        tx(<PolygonSymbolizer fill-opacity=".25" fill="#999999"/>)

      transformed must ==/(
        <PolygonSymbolizer>
          <Fill>
            <CssParameter name="fill-opacity">.25</CssParameter>
            <CssParameter name="fill">#999999</CssParameter>
          </Fill>
        </PolygonSymbolizer>
      )
    }
  }

  "text symbolizers" should {
    val tx =
      new transform.RuleTransformer(new TextSymTransformer(Nil))

    "not require a 'type' attribute for shield images" in {
      val transformed =
        tx(<ShieldSymbolizer name="ref" fontset_name="bold-fonts" size="10" fill="#fff" placement="line" file="&symbols;/mot_shield1.png" min_distance="30" spacing="750"/>)
      transformed must \("Graphic")
    }

    "keep the <Size> element outside the ExternalGraphic" in {
      val transformed = 
        tx(<ShieldSymbolizer file="foo.png" placement="line" fontset_name="bold-fonts" height="12" width="12"/>)
      transformed must \\("Graphic").\("Size")
    }

    "create halos from only a halo_radius property" in {
      val transformed = 
        tx(<TextSymbolizer halo_radius="10" name="name" fontset_name="bold-fonts"/>)

      transformed must \\(
        <Halo>
          <Radius><ogc:Literal>10</ogc:Literal></Radius>
          <Fill>
            <CssParameter name="fill">#ffffff</CssParameter>
          </Fill>
        </Halo>
      )
    }

    "handle shields" in {
      "with placement='line'" >> {
        val transformed = 
          tx(
            <Rule>
              <ShieldSymbolizer name="[shield_name]" placement="line" fontset_name="bold-fonts" size="10" fill="#fff" file="symbols/shield_fwy_us3.png" type="png" width="30" height="30" min_distance="20" spacing="600"/>
            </Rule>
          )

        transformed must \\("TextSymbolizer").\("Graphic")
        transformed must not(\\("PointSymbolizer"))
      }

      "with placement='point'" >> {
        val transformed = 
          tx(
            <Rule>
              <ShieldSymbolizer name="[name]" placement="point" fontset_name="bold-fonts" size="13" fill="#2b2b2b" dy="-13" halo_radius="2" halo_fill="rgba(255,255,255,0.25)" wrap_width="0" line_spacing="2" file="&symbols;/large-city.png" type="png" width="10" height="10" min_distance="20"/>
            </Rule>
          )

        transformed must \\("TextSymbolizer")
        transformed must not(\\("TextSymbolizer").\("Graphic"))
        transformed must \\("PointSymbolizer").\("Graphic")
      }

      "with no specified placement" >> {
        // should be equivalent to placement='point'
        val transformed = 
          tx(
            <Rule>
              <ShieldSymbolizer name="[name]" placement="point" fontset_name="bold-fonts" size="13" fill="#2b2b2b" dy="-13" halo_radius="2" halo_fill="rgba(255,255,255,0.25)" wrap_width="0" line_spacing="2" file="&symbols;/large-city.png" type="png" width="10" height="10" min_distance="20"/>
            </Rule>
          )

        transformed must \\("TextSymbolizer")
        transformed must not(\\("TextSymbolizer").\("Graphic"))
        transformed must \\("PointSymbolizer").\("Graphic")
      }
    }

    "support halo_fill in different formats" in {
      "hex" >> {
        val transformed =
          tx(<TextSymbolizer halo_fill="#fed7a5" name="name" fontset_name="book-fonts" size="8" fill="black" halo_radius="1" placement="line"/>)

        transformed must \("Halo") \("Fill")
      }

      "rgba" >> {
        val transformed =
          tx(<TextSymbolizer halo_fill="rgba(255,255,255,0.25)" name="name" fontset_name="bold-fonts" size="12" fill="#2b2b2b" halo_radius="2" dy="0" placement="line" max_char_angle_delta="40" text_convert="toupper"/>)

        transformed must \("Halo") \("Fill")
      }
    }
  }
}
