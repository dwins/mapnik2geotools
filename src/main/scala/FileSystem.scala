import xml._

class FileSystem(out: java.io.File) extends Mapnik2GeoTools.Output {
  val printer = new PrettyPrinter(80, 2)
  out.mkdirs()

  def save(file: java.io.File, xml: Node) {
    val writer = new java.io.FileWriter(file)
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

    save(new java.io.File(out, name + ".sld"), wrapper)
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
      val writer = new java.io.FileWriter(new java.io.File(out, database._4 + ".sql"))
      for {
        (name, db, table, styles) <- datalayers
        where <- selectPattern.findFirstMatchIn(table) map(_.group(1).trim)
      } {
	      val cleanName = name.replaceAll("[\\s-]", "_");
        val wrapper =
          """
          CREATE TABLE """ + cleanName + """ AS SELECT """ + where + """;
					ALTER TABLE """ + cleanName + """ ADD COLUMN id SERIAL;
					ALTER TABLE """ + cleanName + """ ADD PRIMARY KEY (id);
					INSERT INTO geometry_columns VALUES ( '', 'public', '""" + cleanName + """', 'way', 2, 900913, 'GEOMETRY');
					CREATE INDEX """ + cleanName + """_idx ON """ + cleanName + """ USING GIST(way);
          """
        writer.write(wrapper)
      }
			writer.write("VACUUM ANALYZE;")

      writer.close()
    }

    val writer = new java.io.FileWriter(new java.io.File(out, "loader.sh"))
    for ((user, host, port, name) <- databases)
      writer.write(
        "psql -U"+user+" -h"+host+" -p"+port+" -d"+name+" -f"+name+".sql\n"
      )
    writer.close()
  }
}
