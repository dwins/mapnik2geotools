package me.winslow.d.mn2gt.driver

import me.winslow.d.mn2gt._, Mapnik2GeoTools._
import xml._, transform._

sealed trait Operation {
  def run()
}

case class LocalConversion(
  mapnikFile: java.io.File,
  outputDirectory: java.io.File
) extends Operation {
  val printer = new PrettyPrinter(80, 2)

  def run() {
    val original = xml.XML.load(mapnikFile.getAbsolutePath)
    val convert = 
      new RuleTransformer(
        FilterTransformer,
        PointSymbolizerTransformer,
        MarkersSymbolizerTransformer,
        LineSymTransformer,
        PolygonSymTransformer,
        RasterSymTransformer,
        new TextSymbolizerTransformer(original \\ "FontSet")
      ) andThen (new RuleTransformer(RuleCleanup))

    val converted = convert(original)
    val styles = converted \\ "Style"
    styles.foreach { s =>
      writeStyle(s)
      // TODO: Progress notification for GUI
    }
    val layers = converted \\ "Layer"
    writeLayers(layers)
  }

  private def save(f: java.io.File, xml: Node) {
    val writer = new java.io.FileWriter(f)
    writer.write(printer.format(xml))
    writer.close()
  }

  def writeStyle(style: Node) {
    val name = style.attribute("name").map(_.text).getOrElse("style")
    val wrapper =
      <StyledLayerDescriptor
        version="1.0.0"
        xmlns="http://www.opengis.net/sld"
        xmlns:ogc="http://www.opengis.net/ogc"
        xmlns:xlink="http://www.w3.org/1999/xlink"
      >
        <NamedLayer>
          <Name>{ name }</Name>
          <UserStyle>
            <Name>{ name }</Name>
            <FeatureTypeStyle>
              { style.child }
            </FeatureTypeStyle>
          </UserStyle>
        </NamedLayer>
      </StyledLayerDescriptor>

    save(new java.io.File(outputDirectory, name + ".sld"), wrapper)
  }

  def writeLayers(layers: NodeSeq) {
    def params(datastore: NodeSeq): Map[String, String] =
      datastore \ "Parameter" map {
        p => (p.attributes.asAttrMap("name"), p.text)
      } toMap

    val selectPattern = """(?si:\(SELECT\s+(.*)\)\s+AS)""".r

    val datalayers =
      for {
        layer <- layers
        settings = params(layer \ "Datasource")
        if settings contains "table"
      } yield {
        val db = (settings("user"), settings("host"), settings("port"), settings("dbname"))
        val name = layer.attributes.asAttrMap("name")
        val table = settings("table")
        val styles = layer \ "StyleName" map(_.text)
        (name, db, table, styles)
      }

    val databases = datalayers map(_._2) distinct

    for (database <- databases) {
      val writer = new java.io.FileWriter(new java.io.File(outputDirectory, database._4 + ".sql"))
      for {
        (name, db, table, styles) <- datalayers
        where <- selectPattern.findFirstMatchIn(table) map(_.group(1).trim)
      } {
        val cleanName = name.replaceAll("[\\s-]", "_");
        val sql = Seq(
          "DROP TABLE IF EXISTS " + cleanName + ";",
          "DELETE FROM geometry_columns WHERE f_table_name = '" + cleanName + "';",
          "CREATE TABLE " + cleanName + " AS SELECT " + where + ";",
          "ALTER TABLE " + cleanName + " ADD COLUMN id SERIAL PRIMARY KEY;",
          "INSERT INTO geometry_columns VALUES ( '', 'public', '" + cleanName + "', 'way', 2, 900913, 'GEOMETRY');",
          "CREATE INDEX " + cleanName + "_idx ON " + cleanName + " USING GIST(way);"
        ).mkString("\n")
        writer.write(sql)
        writer.write("\n\n")
      }
      writer.write("VACUUM ANALYZE;")
      writer.close()
    }

    val writer = new java.io.FileWriter(new java.io.File(outputDirectory, "loader.sh"))
    for ((user, host, port, name) <- databases)
      writer.write(
        "psql -U"+user+" -h"+host+" -p"+port+" -d"+name+" -f"+name+".sql\n"
      )
    writer.close()
  }
}

case class PublishToGeoServer(
  mapnikFile: java.io.File,
  connection: GeoServerConnection
) extends Operation {
  def run() {
    val original = xml.XML.load(mapnikFile.getAbsolutePath)
    val convert = 
      new RuleTransformer(
        FilterTransformer,
        PointSymbolizerTransformer,
        MarkersSymbolizerTransformer,
        LineSymTransformer,
        PolygonSymTransformer,
        RasterSymTransformer,
        new TextSymbolizerTransformer(original \\ "FontSet")
      ) andThen (new RuleTransformer(
        RuleCleanup, new URLResolver(new java.net.URL(connection.base))
      ))

    val converted = convert(original)
    val styles = converted \\ "Style"
    styles.foreach { s =>
      writeStyle(s)
      // TODO: Progress notification for GUI
    }
    val layers = converted \\ "Layer"
    writeLayers(layers)
  }

  def normalizeStyleName(name: String) = name.replaceAll("\\s+", "-")

  class URLResolver(base: java.net.URL) extends xml.transform.RewriteRule {
    def resolve(path: String) =
      new java.net.URL(base, "styles/" + path).toString

    override def transform(n: Node): Seq[Node] = {
      n match {
        case e: Elem 
          if e.label == "OnlineResource" && e.attributes.exists(_.key == "href")
          => 
            val link =
              e.attributes.find(_.key == "href").get.value.head.text
            <OnlineResource xlink:href={ resolve(link) }/>
        case other => other
      }
    }
  }

  def writeStyle(style: Node) {
    val name = style.attribute("name").map(_.text).getOrElse("style")
    
    val resolve = 
      new xml.transform.RuleTransformer(
        new URLResolver(new java.net.URL("file:" + connection.datadir))
      )

    val wrapper =
      <StyledLayerDescriptor
        version="1.0.0"
        xmlns="http://www.opengis.net/sld"
        xmlns:ogc="http://www.opengis.net/ogc"
        xmlns:xlink="http://www.w3.org/1999/xlink"
      >
        <NamedLayer>
          <Name>{ name }</Name>
          <UserStyle>
            <Name>{ name }</Name>
            <FeatureTypeStyle>
              { style.child map resolve }
            </FeatureTypeStyle>
          </UserStyle>
        </NamedLayer>
      </StyledLayerDescriptor>

    connection.setStyle(normalizeStyleName(name), wrapper)
  }

  def writeLayers(layers: NodeSeq) {
    import connection.{ ShapefileStore, PostgisStore };

    def params(datastore: NodeSeq): Map[String, String] =
      datastore \ "Parameter" map {
        p => (p.attributes.asAttrMap("name"), p.text)
      } toMap

    val selectPattern = """(?si:\(SELECT\s+(.*)\)\s+AS)""".r
    val workspace = new connection.Workspace(
      connection.namespacePrefix, connection.namespaceUri
    )

    val datalayers =
      for {
        layer <- layers
        settings = params(layer \ "Datasource")
        storeType <- settings.get("type")
        if Set("shape","postgis") contains storeType
      } yield {
        val (datastore, table) =
          storeType match {
            case "shape" =>
              val store = ShapefileStore(workspace, settings("file"))
              (store, store.name)
            case "postgis" =>
              val store =
                PostgisStore(
                  workspace, 
                  settings("user"),
                  settings("host"),
                  settings("port"),
                  settings("dbname"),
                  settings("password")
                )

              val table =
                if (selectPattern.findFirstMatchIn(settings("table")).isDefined)
                  layer.attributes.asAttrMap("name")
                else
                  settings("table")

              (store, table)
          }

        val name = layer.attributes.asAttrMap("name")
        val styles = layer \ "StyleName" map(s => normalizeStyleName(s.text))

        (table.replaceAll("[\\s-]", "_"), datastore, table, styles)
      }

    val databases = datalayers map(_._2) distinct

    for (store <- databases) connection.setDataStore(workspace.prefix, store)

    // OMG HACKS XXX
    def id(store: (String, _, String, _)): String =
      //if (selectPattern.findFirstMatchIn(store._3).isDefined)
        store._1
      //else
      //  store._3

    for (store @ (name, ds, table, styles) <- datalayers) {
      connection.setFeatureType(id(store), workspace.prefix, ds.name, name)
      connection.attachStyles(id(store).replaceAll("[\\s-]", "_"), styles)
    }

    connection.setLayerGroup(connection.namespacePrefix, 
      datalayers map { x => (
        id(x).replaceAll("[\\s-]", "_"),
        x._4.map(normalizeStyleName)
      )}
    )
  }
}

