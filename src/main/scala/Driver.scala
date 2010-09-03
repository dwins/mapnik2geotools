import xml._
import Mapnik2GeoTools._

object Driver {
  // quick n' dirty command line parser
  // supports --switch=foo and --switch foo syntax.
  // -- stops parsing, everything else is a generic argument
  def parseOpts(args: Seq[String]): (Map[String, String], Seq[String]) =
    args match {
      case Seq() => (Map.empty, Seq.empty)
      case Seq("--", xs @ _*) => (Map.empty, xs)
      case Seq(switch, xs @ _*) 
        if (switch startsWith "--") && (switch contains "=")
        =>
        val name = switch drop 2 takeWhile("=" !=)
        val value = switch dropWhile("=" !=) tail
        val (switches, args) = parseOpts(xs)
        (switches + (name -> value), args)
      case Seq(switch, value, xs @ _*) if switch startsWith "--" =>
        val name = switch drop 2
        val (switches, args) = parseOpts(xs)
        (switches + (name -> value), args)
      case Seq(x) => (Map.empty, Seq(x))
      case Seq(x, xs @ _*) => 
        val (switches, args) = parseOpts(xs)
        (switches, x +: args)
    }

  def getSink(source: java.io.File, switches: Map[String, String]): Output = {
    if (Set("rest", "datadir") subsetOf switches.keySet) {
      new GeoServer(
        switches("rest"),
        switches.getOrElse("user", "admin"),
        switches.getOrElse("password", "geoserver"),
        switches("datadir")
      )
    } else {
      val outname = switches.getOrElse("out", "output")
      val outdir = new java.io.File(source.getParent(), outname)
      new FileSystem(outdir)
    }
  }

  def main(args: Array[String]) {
    val (switches, files) = parseOpts(args)
    for (file <- files) {
      val sink: Output = getSink(new java.io.File(file), switches)

      val original = XML.load(file)
      val convert =
        new transform.RuleTransformer(
          FilterTransformer,
          PointSymTransformer,
          LineSymTransformer,
          PolygonSymTransformer,
          new TextSymTransformer(original \\ "FontSet"),
          RuleCleanup
        )
      val doc = convert(original)
      for (style <- doc \\ "Style") sink.writeStyle(style)
      sink.writeLayers(doc \\ "Layer")
    }
  }
}
