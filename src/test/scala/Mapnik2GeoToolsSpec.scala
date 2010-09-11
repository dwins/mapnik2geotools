import org.specs._
import scala.xml._
import Mapnik2GeoTools._

object Mapnik2GeoToolsSpec extends Specification {
  "osm.xml should work" in {}

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
  }

  "text symbolizers" should {
    val tx =
      new transform.RuleTransformer(new TextSymTransformer(Nil))
    "not require a 'type' attribute for shield images" in {
      val transformed =
        tx(<ShieldSymbolizer name="ref" fontset_name="bold-fonts" size="10" fill="#fff" placement="line" file="&symbols;/mot_shield1.png" min_distance="30" spacing="750"/>)
      transformed must \("Graphic")
    }
  }
}
