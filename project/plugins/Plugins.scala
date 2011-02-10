class Plugins(info: sbt.ProjectInfo) extends sbt.PluginDefinition(info) {
  val codaRepo = "codarepo" at "http://repo.codahale.com/"
  val assemblySBT = "com.codahale" % "assembly-sbt" % "0.1"
}
