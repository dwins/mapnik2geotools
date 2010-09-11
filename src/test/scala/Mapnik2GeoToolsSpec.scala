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
}
