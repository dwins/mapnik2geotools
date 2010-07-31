import xml._
import java.net.URLEncoder.encode
import org.apache.commons.httpclient

class GeoServer(base: String, auth: (String, String)) {
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

  def addStyle(name: String, style: Node) {
    val url = base + "/styles?name=" + encode(name, "UTF-8")
    val post =
      new httpclient.methods.PostMethod(url)
    post.setRequestEntity(
      new httpclient.methods.StringRequestEntity(
        style.toString, "application/vnd.ogc.sld+xml", "utf-8"
      )
    )
    val status = client.executeMethod(post)
    val in = post.getResponseBodyAsString()
  }

  def updateStyle(name: String, style: Node) {
    val url = "%s/styles/%s.sld".format(base, name)
    val put =
      new httpclient.methods.PutMethod(url)
    put.setRequestEntity(
      new httpclient.methods.StringRequestEntity(
        style.toString, "application/vnd.ogc.sld+xml", "utf-8"
      )
    )
    val status = client.executeMethod(put)
    val in = put.getResponseBodyAsString()
  }
}
