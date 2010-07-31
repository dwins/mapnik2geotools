import sbt._

class Mapnik2GeoToolsProject(info: ProjectInfo) extends DefaultProject(info) {
  override def libraryDependencies = super.libraryDependencies ++ Set(
    "commons-httpclient" % "commons-httpclient" % "3.1"
  )
}
