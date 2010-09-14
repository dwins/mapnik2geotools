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
      val transformed =
        tx(
          <LineSymbolizer>
            <CssParameter name="stroke">#888</CssParameter>
            <CssParameter name="stroke-width">1</CssParameter>
            <CssParameter name="stroke-dasharray">2,4</CssParameter>
          </LineSymbolizer>
        )

      transformed must \\(<CssParameter name="stroke">#888888</CssParameter>)
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
