package me.winslow.d.mn2gt.driver

import xml._
import java.net.URLEncoder.encode
import org.apache.commons.httpclient

sealed case class GeoServerConnection(
  base: String,
  username: String,
  password: String,
  namespacePrefix: String,
  namespaceUri: String
) {
  val client = {
    val url = new java.net.URL(base)
    val c = new httpclient.HttpClient()
    val credentials = 
      new httpclient.UsernamePasswordCredentials(username, password)
    val scope = 
      new httpclient.auth.AuthScope(
        url.getHost, url.getPort, httpclient.auth.AuthScope.ANY_REALM
      )
    c.getState().setCredentials(scope, credentials)
    c 
  }

  case class Workspace(prefix: String, uri: String)

  trait Store {
    def dataType: String
    def workspace: Workspace
    def name: String
    def toXML: Node
  }

  trait DataStore extends Store {
    val dataType = "datastores"
  }

  trait CoverageStore extends Store { 
    val dataType = "coveragestores"
  }

  case class PostgisStore(
    workspace: Workspace,
    user: String,
    host: String,
    port: String,
    database: String,
    dbpass: String
  ) extends DataStore {
    val name = database.replaceAll("[\\s-]", "_")
    val toXML =
      <dataStore>
        <name>{ database }</name>
        <description>Auto-loaded datastore from Mapnik style</description>
        <type>PostGIS</type>
        <enabled>true</enabled>
        <workspace><name>{ workspace.prefix }</name></workspace>
        <connectionParameters>
          <entry key="user">{ user }</entry>
          <entry key="host">{ host }</entry>
          <entry key="port">{ port }</entry>
          <entry key="passwd">{ dbpass }</entry>
          <entry key="database">{ database }</entry>
          <entry key="namespace">{ workspace.uri }</entry>
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
    workspace: Workspace,
    file: String
  ) extends DataStore {
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
          <name>{ workspace.prefix }</name>
        </workspace>
        <connectionParameters>
          <entry key="memory mapped buffer">true</entry>
          <entry key="create spatial index">true</entry>
          <entry key="charset">ISO-8859-1</entry>
          <entry key="filetype">shapefile</entry>
          <entry key="url">{ fullPath }</entry>
          <entry key="namespace">{ workspace.uri }</entry>
        </connectionParameters>
      </dataStore>
  }

  case class GeoTIFFStore(
    path: String,
    workspace: Workspace
  ) extends CoverageStore {
    val name = path
        .drop(path.lastIndexOf("/") + 1)
        .replaceAll("[\\s-]", "_")
        .replaceAll("(?i).tiff?$","")

    def toXML = 
      <coverageStore>
        <name>{ name }</name>
        <type>GeoTIFF</type>
        <enabled>true</enabled>
        <workspace>
          <name>{ workspace.prefix }</name>
        </workspace>
        <url>file:data/{ path }</url>
      </coverageStore>
  }

  def post(url: String, message: Node, mime: String = "application/xml") 
  : Int = {
    val request = new httpclient.methods.PostMethod(url)
    request.setRequestEntity(
      new httpclient.methods.StringRequestEntity(
        message.toString, mime, "utf-8"
      )
    )
    lazy val body = 
      io.Source.fromInputStream(request.getResponseBodyAsStream()).mkString
    val status = client.executeMethod(request)
    require(status == 201, 
      "POST %s failed with status %d and message %s; was trying to send \n %s".format(url, status, body, message)
    )
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
    lazy val body =
      io.Source.fromInputStream(request.getResponseBodyAsStream).mkString
    client.executeMethod(request).ensuring(
      { _ == 200 },
      "PUT %s failed with message %s; was trying to send \n %s".format(url, body, message)
    )
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
    try 
      updateStyle(name, style)
    catch {
      case _ => addStyle(name, style)
    }
  }

  def addDataStore(workspace: String, store: Store): Int =
    post(
      "%s/workspaces/%s/%s".format(base, workspace, store.dataType),
      store.toXML
    )

  def updateDataStore(workspace: String, store: Store): Int =
    put(
      "%s/workspaces/%s/%s/%s".format(
        base, workspace, store.dataType, store.name),
      store.toXML
    )

  def setDataStore(workspace: String, store: Store): Int =
    try
      updateDataStore(workspace, store)
    catch {
      case (ex: AssertionError) => addDataStore(workspace, store)
    }

  trait DataSet {
    def name: String
    def store: Store
    def datasetType: String
    def layerType: String
    def resourceType: String
    def toXML: xml.Node
  }

  case class FeatureType(
    rawName: String,
    store: DataStore
  ) extends DataSet {
    def name = rawName.replaceAll("[\\s-]", "_")
    val datasetType = "featuretypes"
    val layerType = "VECTOR"
    val resourceType = "featureType"
    def toXML =
      <featureType>
        <name>{ name }</name>
        <nativeName>{ name }</nativeName>
        <title>{ rawName }</title>
        <namespace>
          <name>{ store.workspace.prefix }</name>
        </namespace>
        <srs>EPSG:900913</srs>
        <enabled>true</enabled>
        <store class="dataStore"><name>{ store.name }</name></store>
      </featureType>
  }

  case class Coverage(
    rawName: String,
    store: CoverageStore
  ) extends DataSet {
    def name = rawName.replaceAll("[\\s-]", "_")
    val datasetType = "coverages"
    val layerType = "RASTER"
    val resourceType = "coverage"
    def toXML =
      <coverage>
        <name>{ name }</name>
        <nativeName>{ name }</nativeName>
        <namespace>
          <name>{ store.workspace.prefix }</name>
        </namespace>
        <title>{ rawName }</title>
        <description>Autoconfigured by mapnik2geotools</description>
        <srs>EPSG:900913</srs>
        <projectionPolicy>FORCE_DECLARED</projectionPolicy>
        <enabled>true</enabled>
        <store class="coverageStore">
          <name>{ store.name }</name>
        </store>
        <nativeFormat>GeoTIFF</nativeFormat>
        <supportedFormats>
          <string>GIF</string>
          <string>PNG</string>
          <string>JPEG</string>
          <string>TIFF</string>
          <string>GEOTIFF</string>
        </supportedFormats>
        <interpolationMethods>
          <string>bilinear</string>
          <string>bicubic</string>
          <string>nearest neighbour</string>
        </interpolationMethods>
        <defaultInterpolationMethod>bicubic</defaultInterpolationMethod>
      </coverage>
  }

  def addData(d: DataSet): Int =
    post(
      Seq(base, "workspaces", d.store.workspace.prefix, d.store.dataType,
        d.store.name, d.datasetType
      ).mkString("/"),
      d.toXML
    )

  def updateData(d: DataSet): Int =
    put(
      Seq(base, "workspaces", d.store.workspace.prefix, d.store.dataType,
        d.store.name, d.datasetType, d.name + ".xml"
      ).mkString("/"),
      d.toXML
    )

  def setData(d: DataSet): Int =
    try 
      updateData(d)
    catch {
      case (ex: AssertionError) => addData(d)
    }

  def attachStyles(data: DataSet, styles: Seq[String]): Int = {
    val message =
      <layer>
        <name>{ data.name }</name>
        <type>{ data.layerType }</type>
        <defaultStyle>
          <name>{ styles.head }</name>
        </defaultStyle>
        <styles>
          { for (s <- styles) yield <style><name>{s}</name></style> }
        </styles>
        <resource class={data.resourceType}>
          <name>{ data.name }</name>
        </resource>
        <enabled>true</enabled>
      </layer>

    put(base + "/layers/" + data.name, message)
  }

  def layerGroupXML(name: String, layers: Seq[(String, String)]) =
    <layerGroup>
      <name>{ name }</name>
      <layers>
        {
          for ((layer, _) <- layers) yield 
            <layer><name>{ layer }</name></layer>
        }
      </layers>
      <styles>
        {
          for ((_, style) <- layers) yield 
          <style><name>{ style }</name></style>
        }
      </styles>
    </layerGroup>

  def addLayerGroup(name: String, layers: Seq[(String, String)]): Int =
    post(
      base + "/layergroups/",
      layerGroupXML(name, layers)
    )

  def updateLayerGroup(name: String, layers: Seq[(String, String)]): Int = 
    put(
      base + "/layergroups/" + name + ".xml",
      layerGroupXML(name, layers)
    )

  def setLayerGroup(name: String, layers: Seq[(String, String)]): Int =
    try 
      updateLayerGroup(name, layers)
    catch {
      case (ex: AssertionError) => addLayerGroup(name, layers)
    }

  private def workspaceXML(ws: Workspace) =
    <namespace>
      <prefix>{ws.prefix}</prefix>
      <uri>{ws.uri}</uri>
    </namespace>
      

  def addWorkspace(ws: Workspace): Int =
    post(
      base + "/namespaces/",
      workspaceXML(ws)
    )

  def updateWorkspace(ws: Workspace): Int = 
    put(
      base + "/namespaces/" + ws.prefix + ".xml",
      workspaceXML(ws)
    )

  def setWorkspace(ws: Workspace): Int = {
    try 
      updateWorkspace(ws)
    catch {
      case _ => addWorkspace(ws)
    }
  }
}
