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

  def createDataStore(store: Store): Int = {
    val message = store.toXML

    val url = base + "/workspaces/osm/datastores/?name=" + store.name
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
    if (!(200 to 299 contains status)) println(url + ": (" + status + ") " + in)
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
        <store class="dataStore"><name>{ datastore }</name></store>
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
    if (!(200 to 299 contains status)) println(url + ": (" + status + ") " + in)
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
    if (!(200 to 299 contains status)) println(url + ": " + in)
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

    val url = base + "/layergroups/"// osm.xml"
    val put =
      new httpclient.methods.PostMethod(url)
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

  trait Store {
    def name: String
    def toXML: Node
  }

  case class PostgisStore(
    user: String,
    host: String,
    port: String,
    database: String
  ) extends Store {
    val name = database.replaceAll("[\\s-]", "_")
    val toXML =
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
  }

  case class ShapefileStore(
    file: String
  ) extends Store {
    val name = file
        .drop(file.lastIndexOf("/") + 1)
        .replaceAll("[\\s-]", "_")

    val toXML =
      <dataStore>
        <name>{ name }</name>
        <type>Shapefile</type>
        <enabled>true</enabled>
        <workspace>
          <name>osm</name>
        </workspace>
        <connectionParameters>
          <entry key="memory mapped buffer">true</entry>
          <entry key="create spatial index">true</entry>
          <entry key="charset">ISO-8859-1</entry>
          <entry key="filetype">shapefile</entry>
          <entry key="url">{ "file:data/" + file + ".shp" }</entry>
          <entry key="namespace">http://mercury/osm/</entry>
        </connectionParameters>
      </dataStore>
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
        storeType <- settings.get("type").filter(Set("shape","postgis").contains)
      } yield {
        val (datastore, table) =
          storeType match {
            case "shape" =>
              val store = ShapefileStore(settings("file"))
              (store, store.name)
            case "postgis" =>
              val store =
                PostgisStore(settings("user"), settings("host"), settings("port"), settings("dbname"))
              (store, settings("table"))
          }

        val name = layer.attributes.asAttrMap("name")
        val styles = layer \ "StyleName" map(s => normalizeStyleName(s.text))

        (name, datastore, table, styles)
      }

    val databases = datalayers map(_._2) distinct

    for (store <- databases) createDataStore(store)

    def id(store: (String, _, String, _)): String =
      if (selectPattern.findFirstMatchIn(store._3).isDefined)
        store._1
      else
        store._3

    for (store@(name, ds, table, styles) <- datalayers) {
      createFeatureType(id(store), ds.name, name)
      attachStyles(id(store).replaceAll("[\\s-]", "_"), styles)
    }

    createLayerGroup(datalayers map {
      x => (
        id(x).replaceAll("[\\s-]", "_"),
        x._4.map(normalizeStyleName)
      )
    })
  }
}
