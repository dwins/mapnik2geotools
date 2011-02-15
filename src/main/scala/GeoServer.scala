import xml._
import java.net.URLEncoder.encode
import org.apache.commons.httpclient

class GeoServer(base: String, user: String, password: String, datadir: String, prefix: String, namespace: String) 
extends Mapnik2GeoTools.Output {
  val dataUrl = new java.io.File(datadir).toURI.toURL
  val client = new httpclient.HttpClient()

  {
    val url = new java.net.URL(base)
    val credentials =
      new httpclient.UsernamePasswordCredentials(user, password)
    client.getState().setCredentials(
      new httpclient.auth.AuthScope(
        url.getHost(), url.getPort(), httpclient.auth.AuthScope.ANY_REALM
      ),
      credentials
    )
  }

  trait Store {
    def name: String
    def toXML: Node
  }

  trait Layer {
    def id: String
    def name: String
    def store: Store
    def styles: Seq[String]
    def collectionURL: String
    def resourceURL: String
    def asResourceXML: Node
    // def asLayerXML: Node
  }

  def normalizeStyleName(name: String) = name.replaceAll("\\s+", "-")

  def post(url: String, message: Node, mime: String = "application/xml")
  : Int = {
    val request = new httpclient.methods.PostMethod(url)
    request.setRequestEntity(
      new httpclient.methods.StringRequestEntity(
        message.toString, mime, "utf-8"
      )
    )
    val status = client.executeMethod(request)
    if (!(200 to 299 contains status)) {
      val body =
        io.Source.fromInputStream(request.getResponseBodyAsStream()).mkString
      println("%s: (%d) %s".format(url, status, body))
    }
    status
  }

  def put(url: String, message: Node, mime: String = "application/xml")
  : Int = {
    val request = new httpclient.methods.PutMethod(url)
    request.setRequestEntity(
      new httpclient.methods.StringRequestEntity(
        message.toString, mime, "utf-8"
      )
    )
    val status = client.executeMethod(request)
    if (!(200 to 299 contains status)) {
      val body =
        io.Source.fromInputStream(request.getResponseBodyAsStream()).mkString
      println("%s: (%d) %s".format(url, status, body))
    }
    status
  }

  def addStyle(name: String, style: Node): Int =
    post(
      base + "/styles?name=" + encode(name, "UTF-8"),
      style, 
      "application/vnd.ogc.sld+xml"
    )

  def updateStyle(name: String, style: Node): Int =
    put(
      base + "/styles/" + name + ".sld",
      style,
      "application/vnd.ogc.sld+xml"
    )

  def setStyle(name: String, style: Node): Int = {
    val status = updateStyle(name, style)
    if (400 to 499 contains status) {
      addStyle(name, style)
    } else {
      status
    }
  }

  def addDataStore(store: Store): Int =
    post(
      base + "/workspaces/" + prefix + "/datastores/",
      store.toXML
    )

  def updateDataStore(store: Store): Int = 
    put(
      base + "/workspaces/" + prefix + "/datastores/" + store.name,
      store.toXML
    )

  def setDataStore(store: Store): Int = {
    val status = updateDataStore(store)
    if (400 to 499 contains status) {
      addDataStore(store)
    } else {
      status
    }
  }

  def setFeatureType(layer: Layer): Int = {
    val status = updateFeatureType(layer)
    if (400 to 499 contains status) {
      addFeatureType(layer)
    } else {
      status
    }
  }

  def addFeatureType(layer: Layer): Int =
    post(
      layer.collectionURL,
      layer.asResourceXML
    )

  def updateFeatureType(layer: Layer): Int =
    put(layer.resourceURL, layer.asResourceXML)

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

    put(base + "/layers/" + typename, message)
  }

  def layerGroupXML(layers: Seq[(String, String)]) =
    <layerGroup>
      <name>{ prefix }</name>
      <layers>
        { for ((layer, _) <- layers) yield <layer><name>{ layer }</name></layer> }
      </layers>
      <styles>
        { for ((_, style) <- layers) yield <style><name>{ style }</name></style> }
      </styles>
    </layerGroup>

  def addLayerGroup(layers: Seq[(String, String)]): Int =
    post(
      base + "/layergroups/",
      layerGroupXML(layers)
    )

  def updateLayerGroup(layers: Seq[(String, String)]): Int = 
    put(
      base + "/layergroups/" + prefix + ".xml",
      layerGroupXML(layers)
    )

  def setLayerGroup(layers: Seq[(String, String)]): Int = {
    val status = updateLayerGroup(layers)
    if (400 to 499 contains status) {
      addLayerGroup(layers)
    } else {
      status
    }
  }

  def writeStyle(style: Node) {
    val name = style.attribute("name").map(_.text).getOrElse("style")
    
    val resolve = new xml.transform.RuleTransformer(URLResolver)
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

    setStyle(normalizeStyleName(name), wrapper)
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
        <workspace><name>{ prefix }</name></workspace>
        <connectionParameters>
          <entry key="user">{ user }</entry>
          <entry key="host">{ host }</entry>
          <entry key="port">{ port }</entry>
          <entry key="database">{ database }</entry>
          <entry key="namespace">{ namespace }</entry>
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
        .replaceAll(".shp$","")
    val fullPath = "file:data/" + (if (file endsWith ".shp") file else file + ".shp")

    val toXML =
      <dataStore>
        <name>{ name }</name>
        <type>Shapefile</type>
        <enabled>true</enabled>
        <workspace>
          <name>{ prefix }</name>
        </workspace>
        <connectionParameters>
          <entry key="memory mapped buffer">true</entry>
          <entry key="create spatial index">true</entry>
          <entry key="charset">ISO-8859-1</entry>
          <entry key="filetype">shapefile</entry>
          <entry key="url">{ fullPath }</entry>
          <entry key="namespace">{ namespace }</entry>
        </connectionParameters>
      </dataStore>
  }

  case class VectorLayer(name: String, store: Store, styles: Seq[String]) extends Layer {
    val id = name.replaceAll("[\\s-]", "_")

    val collectionURL =
      "%s/workspaces/%s/datastorese/%s/featuretypes/" format(base, prefix, store.name)

    val resourceURL =
      "%s/workspaces/%s/datastorese/%s/featuretypes/%s.xml" format(base, prefix, store.name, name)

    val asResourceXML = 
      <featureType>
        <name>{ name.replaceAll("[\\s-]", "_") }</name>
        <nativeName>{ name.replaceAll("[\\s-]", "_") }</nativeName>
        <namespace>
          <name>{ prefix }</name>
        </namespace>
        <title>{ name }</title>
        <srs>EPSG:900913</srs>
        <enabled>true</enabled>
        <store class="dataStore">
          <name>{ store.name }</name>
        </store>
      </featureType>
  }

  object URLResolver extends xml.transform.RewriteRule {
    override def transform(n: Node): Seq[Node] = {
      n match {
        case e: Elem 
          if e.label == "OnlineResource" && e.attributes.exists(_.key == "href")
          => 
            val link =
              e.attributes.find(_.key == "href").get.value.head.text
            val resolved = new java.net.URL(dataUrl, "styles/" + link)
            <OnlineResource xlink:href={ resolved.toString }/>
        case other => other
      }
    }
  }

  def writeLayers(layers: NodeSeq) {
    def params(datastore: NodeSeq): Map[String, String] =
      datastore \ "Parameter" map {
        p => (p.attributes.asAttrMap("name"), p.text)
      } toMap

    val SelectStatement = """(?si:.*\(SELECT\s+.*\)\s+AS).*""".r

    val datalayers: Seq[Layer] =
      for {
        layer <- layers
        settings = params(layer \ "Datasource")
        storeType <- settings.get("type")
        if Set("shape","postgis") contains storeType
      } yield {
        val store =
          storeType match {
            case "shape" => ShapefileStore(settings("file"))
            case "postgis" =>
              val dbname =
                settings("table") match {
                  case SelectStatement() => layer.attributes.asAttrMap("name")
                  case table => table
                }

              PostgisStore(settings("user"), settings("host"), settings("port"), dbname)
          }

        val name = layer.attributes.asAttrMap("name")
        val styles = layer \ "StyleName" map(s => normalizeStyleName(s.text))

        VectorLayer(name, store, styles)
      }

    val stores = datalayers.map(_.store).distinct

    for (s <- stores) setDataStore(s)

    for (lyr <- datalayers; styles = lyr.styles) {
      setFeatureType(lyr)
      attachStyles(lyr.id, styles)
    }

    setLayerGroup(
      for (lyr <- datalayers; style <- lyr.styles) yield (lyr.id, style)
    )
  }
}
