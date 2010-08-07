import xml._
import java.net.URLEncoder.encode
import org.apache.commons.httpclient

class GeoServer(base: String, auth: (String, String)) extends Mapnik2GeoTools.Output {
  val client = new httpclient.HttpClient()

  {
    val url = new java.net.URL(base)
    val credentials =
      new httpclient.UsernamePasswordCredentials(auth._1, auth._2)
    client.getState().setCredentials(
      new httpclient.auth.AuthScope(
        url.getHost(), url.getPort(), httpclient.auth.AuthScope.ANY_REALM
      ),
      credentials
    )
  }

  def normalizeStyleName(name: String) = name.replaceAll("\\s+", "-")

  def addStyle(name: String, style: Node): Int = {
    val url = base + "/styles?name=" + encode(name, "UTF-8")
    val post =
      new httpclient.methods.PostMethod(url)
    post.setRequestEntity(
      new httpclient.methods.StringRequestEntity(
        style.toString, "application/vnd.ogc.sld+xml", "utf-8"
      )
    )
    val status = client.executeMethod(post)
    val in =
      io.Source.fromInputStream(post.getResponseBodyAsStream()).mkString
    if (!(200 to 299 contains status)) println(url + ": " + in)
    post.releaseConnection()
    status
  }

  def updateStyle(name: String, style: Node): Int = {
    val url = "%s/styles/%s.sld".format(base, name)
    val put =
      new httpclient.methods.PutMethod(url)
    put.setRequestEntity(
      new httpclient.methods.StringRequestEntity(
        style.toString, "application/vnd.ogc.sld+xml", "utf-8"
      )
    )
    val status = client.executeMethod(put)
    val in =
      io.Source.fromInputStream(put.getResponseBodyAsStream()).mkString
    if (!(200 to 299 contains status)) println(url + ": " + in)
    put.releaseConnection()
    status
  }

  def setStyle(name: String, style: Node): Int = {
    val status = updateStyle(name, style)
    if ((400 until 500) contains status) {
      addStyle(name, style)
    } else {
      status
    }
  }

  def createDataStore(user: String, host: String, port: String, database: String): Int = {
    val message = 
      <dataStore>
        <name>{ database }</name>
        <description>Auto-loaded datastore from Mapnik style</description>
        <type>PostGIS</type>
        <enabled>true</enabled>
        <workspace><name>osm</name></workspace>
        <connectionParameters>
          <entry key="user">{ user }</entry>
          <entry key="host">{ host }</entry>
          <entry key="port">{ port }</entry>
          <entry key="database">{ database }</entry>
          <entry key="namespace">http://mercury/osm/</entry> 
          <entry key="dbtype">postgis</entry>
          <entry key="Connection timeout">20</entry> 
          <entry key="validate connections">false</entry> 
          <entry key="max connections">10</entry> 
          <entry key="schema">public</entry> 
          <entry key="Loose bbox">true</entry> 
          <entry key="Expose primary keys">false</entry> 
          <entry key="fetch size">1000</entry> 
          <entry key="Max open prepared statements">50</entry> 
          <entry key="preparedStatements">false</entry> 
          <entry key="min connections">1</entry> 
        </connectionParameters>
      </dataStore>
    
    val url = base + "/workspaces/osm/datastores/?name=" + database
    val post =
      new httpclient.methods.PostMethod(url)
    post.setRequestEntity(
      new httpclient.methods.StringRequestEntity(
        message.toString, "application/xml", "utf-8"
      )
    )
    val status = client.executeMethod(post)
    val in =
      io.Source.fromInputStream(post.getResponseBodyAsStream()).mkString
    post.releaseConnection()
    status
  }

  def createFeatureType(name: String, datastore: String, table: String) {
    val cleanedName = name.replaceAll("[\\s-]", "_")
    val message = 
      <featureType>
        <name>{ cleanedName }</name>
        <nativeName>{ cleanedName }</nativeName>
        <namespace>
          <name>osm</name>
        </namespace>
        <title>name</title>
        <srs>EPSG:900913</srs>
        <enabled>true</enabled>
        <store class="dataStore"><name>osm_cpk</name></store>
      </featureType>

    val url = base + "/workspaces/osm/datastores/" + datastore + "/featuretypes/?name=" + cleanedName
    val post =
      new httpclient.methods.PostMethod(url)
    post.setRequestEntity(
      new httpclient.methods.StringRequestEntity(
        message.toString, "application/xml", "utf-8"
      )
    )
    val status = client.executeMethod(post)
    val in =
      io.Source.fromInputStream(post.getResponseBodyAsStream()).mkString
    if (!(200 to 299 contains status)) println(cleanedName + ": " + in)
    post.releaseConnection()
    status
  }

  def attachStyles(typename: String, styles: Seq[String]): Int = {
    val message = 
      <layer> 
        <name>{ typename }</name> 
        <type>VECTOR</type> 
        <styles> 
          { for (s <- styles) yield <style><name>{s}</name></style> }
        </styles>
        <resource class="featureType"><name>{ typename }</name></resource> 
        <enabled>true</enabled> 
      </layer>

    val url = base + "/layers/" + typename
    val put =
      new httpclient.methods.PutMethod(url)
    put.setRequestEntity(
      new httpclient.methods.StringRequestEntity(
        message.toString, "application/xml", "utf-8"
      )
    )
    val status = client.executeMethod(put)
    val in =
      io.Source.fromInputStream(put.getResponseBodyAsStream()).mkString
    if (!(200 to 299 contains status)) println(typename + ": " + in)
    put.releaseConnection()
    status
  }

  def createLayerGroup(layers: Seq[(String, Seq[String])]): Int = {
    val message = 
      <layerGroup>
        <name>osm</name> <layers>
          { for ((layer, styles) <- layers; _ <- styles) 
            yield <layer><name>{layer}</name></layer>
          }
        </layers>
        <styles>
          { for ((_, styles) <- layers; style <- styles)
            yield <style><name>{ style }</name></style>
          }
        </styles>
        <bounds> 
          <minx>-8508704.72</minx> 
          <maxx>-8457610.3</maxx> 
          <miny>4394180.29</miny> 
          <maxy>4432833.99</maxy> 
          <crs class="projected">EPSG:900913</crs> 
        </bounds> 
      </layerGroup>

    val url = base + "/layergroups/osm.xml"
    val put =
      new httpclient.methods.PutMethod(url)
    put.setRequestEntity(
      new httpclient.methods.StringRequestEntity(
        message.toString, "application/xml", "utf-8"
      )
    )
    val status = client.executeMethod(put)
    val in =
      io.Source.fromInputStream(put.getResponseBodyAsStream()).mkString
    println(url + ": (" + status + ") " + in)
    put.releaseConnection()
    status
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

    setStyle(normalizeStyleName(name), wrapper)
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
        val styles = layer \ "StyleName" map(s => normalizeStyleName(s.text))
        (name, db, table, styles)
      }

    val databases = datalayers map(_._2) distinct

    for ((user, host, port, database) <- databases)
      createDataStore(user, host, port, database)

    for ((name, db, table, styles) <- datalayers) {
      if (selectPattern.findFirstMatchIn(table) isDefined)
        createFeatureType(name, db._4, name)
      else {
        createFeatureType(name, db._4, table)
      }
      attachStyles(name.replaceAll("[\\s-]", "_"), styles)
    }

    createLayerGroup(datalayers map {
      x => (
        x._1.replaceAll("[\\s-]", "_"), 
        x._4.map(normalizeStyleName)
      )
    })
  }
}
