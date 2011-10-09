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
  def run() {
    ensureOutputDirectory()
    val original = xml.XML.load(mapnikFile.getAbsolutePath)
    val convert = 
      new RuleTransformer(Mapnik2GeoTools.rulesFor(original): _*)
    val cleanup = 
      new RuleTransformer(RuleCleanup)
    val fullTransform = convert andThen cleanup

    val converted = fullTransform(original)
    val styles = converted \\ "Style"
    styles.foreach { s =>
      writeStyle(s)
      // TODO: Progress notification for GUI
    }
    val layers = converted \\ "Layer"
    writeLayers(layers)
  }

  def ensureOutputDirectory() {
    if (outputDirectory exists) {
      require(outputDirectory isDirectory,
        "Please select a directory, not a file, for output (you chose " + outputDirectory.getAbsolutePath + ")")
      require(outputDirectory.canWrite && outputDirectory.canRead,
        "You do not have permissions to the output directory (" + outputDirectory.getAbsolutePath + ")")
    } else {
      val parent = outputDirectory.getParentFile
      require(parent.exists && parent.isDirectory,
        "The output directory doesn't exist! (I will create one directory, but not multiple nested ones.)  You tried to use " + outputDirectory.getAbsolutePath + " .")
      require(outputDirectory.getParentFile.canWrite,
        "You do not have permissions to create the output directory: " + outputDirectory.getAbsolutePath)
      outputDirectory.mkdir()
    }
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

    XML.save(new java.io.File(outputDirectory, name + ".sld").getAbsolutePath, wrapper)
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

    val rasterLayers = 
      for {
        layer <- layers
        settings = params(layer \ "Datasource")
        if settings("type") == "raster"
      } yield settings

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

    locally {
      val writer = 
        new java.io.FileWriter(
          new java.io.File(outputDirectory, "loader.sh")
        )
      for ((user, host, port, name) <- databases)
        writer.write(
          "psql -U"+user+" -h"+host+" -p"+port+" -d"+name+" -f"+name+".sql\n"
        )
      writer.close()
    }

    locally {
      val writer = 
        new java.io.FileWriter(
          new java.io.File(outputDirectory, "fixtiffs.sh")
        )
      for (settings <- rasterLayers) {
        val chunks = Seq(
          "gdal_translate -of GTiff",
          "-a_ullr %s %s %s %s".format(
            Seq("lox", "hiy", "hix", "loy").map(settings): _*),
          "%1$s %1$s".format(settings("file"))
        )

        writer.write(chunks.mkString(" "))
        writer.write("\n")
      }
      writer.close()
    }
  }
}

case class PublishToGeoServer(
  mapnikFile: java.io.File,
  connection: GeoServerConnection
) extends Operation {
  import java.net.URL

  def run() {
    val original = xml.XML.load(mapnikFile.getAbsolutePath)
    val convert = 
      new RuleTransformer(Mapnik2GeoTools.rulesFor(original): _*)
    val cleanup = 
      new RuleTransformer(RuleCleanup, URLResolver)
    val fullTransform = convert andThen cleanup

    val converted = fullTransform(original)
    val styles = converted \\ "Style"
    styles.foreach { s =>
      writeStyle(s)
      // TODO: Progress notification for GUI
    }
    connection.setWorkspace(connection.Workspace(
      connection.namespacePrefix, connection.namespaceUri)).ensuring(Set(200, 201) contains _)
    val layers = converted \\ "Layer"
    writeLayers(layers)
  }

  def normalizeStyleName(name: String) = name.replaceAll("\\s+", "-")

  object URLResolver extends xml.transform.RewriteRule {
    def resolve(path: String) = "file:" + path

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

    connection.setStyle(normalizeStyleName(name), wrapper).ensuring(Set(200, 201) contains _)
  }

  def writeLayers(layers: NodeSeq) {
    import connection.{
      Store, DataSet, FeatureType, Coverage,
      ShapefileStore, PostgisStore, GeoTIFFStore
    }

    def params(datastore: NodeSeq): Map[String, String] =
      datastore \ "Parameter" map {
        p => (p.attributes.asAttrMap("name"), p.text)
      } toMap

    val selectPattern = """(?si:\(SELECT\s+(.*)\)\s+AS)""".r
    val workspace = new connection.Workspace(
      connection.namespacePrefix, connection.namespaceUri
    )

    val datalayers: Seq[(Store, DataSet, Seq[String])] =
      for {
        layer <- layers
        settings = params(layer \ "Datasource")
        storeType <- settings.get("type")
        styles = layer \ "StyleName" map(s => normalizeStyleName(s.text))
        if Set("shape","postgis", "raster") contains storeType
      } yield {
        val (datastore, dataset) =
          storeType match {
            case "shape" =>
              val store = ShapefileStore(workspace, settings("file"))
              val data = FeatureType(store.name, store)
              (store, data)
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

              val ft = FeatureType(table, store)
              (store, ft)
            case "raster" =>
              val store = GeoTIFFStore(settings("file"), workspace)
              val data = Coverage(store.name, store)
              (store, data)
          }

        (datastore, dataset, styles)
      }

    val databases = datalayers map(_._1) distinct

    for (store <- databases)
      connection.setDataStore(workspace.prefix, store).ensuring(Set(200, 201) contains _)

    for ((_, data, styles) <- datalayers) {
      connection.setData(data).ensuring(Set(200, 201) contains _)
      connection.attachStyles(data, styles)
    }

    val styledLayers = 
      for {
        (_, layer, styles) <- datalayers
        style <- styles
      } yield (layer.name, style)

    connection.setLayerGroup(connection.namespacePrefix, styledLayers)
      .ensuring(Set(200, 201) contains _)
  }
}

