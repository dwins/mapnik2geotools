package me.winslow.d.mn2gt.driver

import xml._
import java.net.URLEncoder.encode
import org.apache.commons.httpclient

sealed case class GeoServerConnection(
  base: String,
  username: String,
  password: String,
  datadir: String,
  namespacePrefix: String,
  namespaceUri: String
) {
  val dataUrl = new java.io.File(datadir).toURI.toURL
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

  trait DataStore extends Store { val dataType = "datastores" }
  trait CoverageStore extends Store { val dataType = "coveragestores" }

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
    client.executeMethod(request).ensuring(
      { _ == 201 },
      "POST %s failed with message %s; was trying to send \n %s".format(url, body, message)
    )
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
    val status = updateStyle(name, style)
    if (400 to 499 contains status)
      addStyle(name, style)
    else
      status
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

  def featureTypeXML(name: String, store: Store): Node =
    <featureType>
      <name>{ name.replaceAll("[\\s-]", "_") }</name>
      <nativeName>{ name.replaceAll("[\\s-]", "_") }</nativeName>
      <namespace>
        <name>{ store.workspace.prefix }</name>
      </namespace>
      <title>{ name }</title>
      <srs>EPSG:900913</srs>
      <enabled>true</enabled>
      <store class="dataStore"><name>{ store.name }</name></store>
    </featureType>

  def addFeatureType(name: String, store: Store): Int =
    post(
      "%s/workspaces/%s/datastores/%s/featuretypes/"
        .format(base, store.workspace.prefix, store.name),
      featureTypeXML(name, store)
    )

  def updateFeatureType(name: String, store: Store): Int =
    put(
      "%s/workspaces/%s/datastores/%s/featuretypes/%s.xml"
        .format(base, store.workspace.prefix, store.name, name),
      featureTypeXML(name, store)
    )

  def setFeatureType(name: String, store: Store): Int = {
    try 
      updateFeatureType(name, store)
    catch {
      case (ex: AssertionError) => addFeatureType(name, store)
    }
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

    put(base + "/layers/" + typename, message)
  }

  def layerGroupXML(name: String, layers: Seq[(String, Seq[String])]) =
    <layerGroup>
      <name>{ name }</name>
      <layers>
        {
          for ((layer, styles) <- layers; _ <- styles)
          yield <layer><name>{ layer }</name></layer>
        }
      </layers>
      <styles>
        {
          for ((_, styles) <- layers; style <- styles)
          yield <style><name>{ style }</name></style>
        }
      </styles>
    </layerGroup>

  def addLayerGroup(name: String, layers: Seq[(String, Seq[String])]): Int =
    post(
      base + "/layergroups/",
      layerGroupXML(name, layers)
    )

  def updateLayerGroup(name: String, layers: Seq[(String, Seq[String])]): Int = 
    put(
      base + "/layergroups/" + name + ".xml",
      layerGroupXML(name, layers)
    )

  def setLayerGroup(name: String, layers: Seq[(String, Seq[String])]): Int =
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
    val status = updateWorkspace(ws)
    if (400 to 499 contains status)
      addWorkspace(ws)
    else
      status
  }
}
